open Base
open Lwt.Syntax
open Sys4c
open System4_lsp

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

class lsp_server ain_path =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server as super
    val mutable ain : Ain.t = Ain.create 4 0

    val buffers : (string, Document.t) Hashtbl.t =
      Hashtbl.create (module String)

    method spawn_query_handler f = Linol_lwt.spawn f

    method private _on_doc ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (uri : Lsp.Types.DocumentUri.t) (contents : string) =
      try
        let doc = Document.create ain contents in
        Hashtbl.set buffers ~key:(Lsp.Types.DocumentUri.to_string uri) ~data:doc;
        notify_back#send_diagnostic
          (List.map doc.errors ~f:(fun (range, message) ->
               Lsp.Types.Diagnostic.create ~range ~message ()))
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
        if not (String.is_empty ain_path) then
          try
            ain <- Ain.load ain_path;
            notify_back#send_log_msg ~type_:Lsp.Types.MessageType.Info
              (ain_path ^ " loaded")
          with e -> show_exn notify_back e
        else Lwt.return ()
      in
      super#on_req_initialize ~notify_back i

    method on_notif_doc_did_open ~notify_back d ~content =
      self#_on_doc ~notify_back d.uri content

    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old
        ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    method on_notif_doc_did_close ~notify_back:_ d =
      Hashtbl.remove buffers (Lsp.Types.DocumentUri.to_string d.uri);
      Lwt.return ()

    method! config_hover = Some (`Bool true)

    method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_ _ =
      (match Hashtbl.find buffers (Lsp.Types.DocumentUri.to_string uri) with
      | Some doc -> Hover.get_hover doc pos
      | None -> None)
      |> Lwt.return
  end

let run ain =
  let s = new lsp_server ain in
  let server = Linol_lwt.Jsonrpc2.create_stdio s in
  let task = Linol_lwt.Jsonrpc2.run server in
  Linol_lwt.run task

let () =
  let ain = ref "" in
  let usage_msg = "Usage: system4-lsp --ain <file>" in
  let speclist =
    [ ("--ain", Stdlib.Arg.Set_string ain, ".ain file to load") ]
  in
  let anon_fun s =
    Stdlib.Arg.usage speclist
      (Printf.sprintf "extra argument \"%s\"\n%s" s usage_msg);
    Stdlib.exit 2
  in
  Stdlib.Arg.parse speclist anon_fun usage_msg;
  run !ain
