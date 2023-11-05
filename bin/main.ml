open Base
open Lwt.Syntax
open System4_lsp
open Types
open Project

let show_error notify_back message =
  let params =
    Lsp.Types.ShowMessageParams.create ~type_:Lsp.Types.MessageType.Error
      ~message
  in
  notify_back#send_notification (Lsp.Server_notification.ShowMessage params)

let show_exn notify_back e =
  let msg = Stdlib.Printexc.to_string e in
  Lwt.join
    [
      show_error notify_back msg;
      (* Also notify the backtrace with logMessage. *)
      notify_back#send_log_msg ~type_:Lsp.Types.MessageType.Error
        (msg ^ "\n" ^ Backtrace.to_string (Backtrace.Exn.most_recent ()));
    ]

(* The default exception printer escapes utf-8 sequences. Try to prevent that
   from happening as much as possible. *)
let () =
  Stdlib.Printexc.register_printer (function
    | Sys_error msg -> Some msg
    | _ -> None)

class lsp_server =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server as super
    val project = Project.create ()
    method spawn_query_handler f = Linol_lwt.spawn f

    method private _on_doc ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (uri : Lsp.Types.DocumentUri.t) (contents : string) =
      try
        let diagnostics = update_document project uri contents in
        notify_back#send_diagnostic diagnostics
      with e -> show_exn notify_back e

    (* Do not use incremental update, to work around a bug in lsp 1.14 where its
       content change application logic is confused when the newline code is CRLF. *)
    method! config_sync_opts =
      {
        (super#config_sync_opts) with
        change = Some Lsp.Types.TextDocumentSyncKind.Full;
      }

    method! on_req_initialize ~notify_back i =
      let* () =
        let options =
          InitializationOptions.t_of_yojson
            (Option.value i.initializationOptions ~default:(`Assoc []))
        in
        try
          Project.initialize project options;
          (* AinDecompiler generates type definitions in classes.jaf and global
             variables in globals.jaf. Loading these helps with Goto Definition
             and type checking for functypes. *)
          (try
             load_document project "classes.jaf";
             load_document project "globals.jaf"
           with _ -> ());
          notify_back#send_log_msg ~type_:Lsp.Types.MessageType.Info
            (options.ainPath ^ " loaded")
        with e -> show_exn notify_back e
      in
      super#on_req_initialize ~notify_back i

    method on_notif_doc_did_open ~notify_back d ~content =
      self#_on_doc ~notify_back d.uri content

    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old
        ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    method on_notif_doc_did_close ~notify_back:_ _ = Lwt.return ()
    method! config_hover = Some (`Bool true)

    method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_ _ =
      get_hover project uri pos |> Lwt.return

    method! config_definition = Some (`Bool true)

    method! on_req_definition ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        ~partialResultToken:_ _ =
      get_definition project uri pos |> Lwt.return
  end

let run () =
  let s = new lsp_server in
  let server = Linol_lwt.Jsonrpc2.create_stdio s in
  let task = Linol_lwt.Jsonrpc2.run server in
  Linol_lwt.run task

let print_version () =
  Stdio.printf "system4-lsp %s\n"
    (match Build_info.V1.version () with
    | None -> "n/a"
    | Some v -> Build_info.V1.Version.to_string v)

let () =
  let argv = Sys.get_argv () in
  if Array.length argv = 2 && String.equal argv.(1) "--version" then
    print_version ()
  else run ()
