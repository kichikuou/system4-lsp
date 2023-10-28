open Base
open Sys4c
open System4_lsp

let send_log_msg notify_back type_ msg =
  notify_back#send_log_msg ~type_ msg |> ignore

let log_info notify_back msg =
  send_log_msg notify_back Lsp.Types.MessageType.Info msg

let log_error notify_back msg =
  send_log_msg notify_back Lsp.Types.MessageType.Error msg

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
      with e ->
        log_error notify_back
          (Exn.to_string e ^ "\n"
          ^ Backtrace.to_string (Backtrace.Exn.most_recent ()));
        Lwt.return ()

    (* Do not use incremental update, to work around a bug in lsp 1.14 where its
       content change application logic is confused when the newline code is CRLF. *)
    method! config_sync_opts =
      {
        (super#config_sync_opts) with
        change = Some Lsp.Types.TextDocumentSyncKind.Full;
      }

    method! on_req_initialize ~notify_back i =
      (if not (String.is_empty ain_path) then
         try
           ain <- Ain.load ain_path;
           log_info notify_back (ain_path ^ " loaded")
         with e -> log_error notify_back (Exn.to_string e));
      super#on_req_initialize ~notify_back i

    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_lwt.t =
      self#_on_doc ~notify_back d.uri content

    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old
        ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    method on_notif_doc_did_close ~notify_back:_ d : unit Linol_lwt.t =
      Hashtbl.remove buffers (Lsp.Types.DocumentUri.to_string d.uri);
      Linol_lwt.return ()

    method! config_hover = Some (`Bool true)

    method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_ _ =
      match Hashtbl.find buffers (Lsp.Types.DocumentUri.to_string uri) with
      | Some doc -> Linol_lwt.return (Hover.get_hover doc pos)
      | None -> Linol_lwt.return None
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
