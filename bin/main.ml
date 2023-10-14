open Base
open Sys4c
open System4_lsp

let send_log_msg notify_back type_ msg =
  notify_back#send_log_msg ~type_ msg |> ignore

let log_info notify_back msg =
  send_log_msg notify_back Lsp.Types.MessageType.Info msg

let log_error notify_back msg =
  send_log_msg notify_back Lsp.Types.MessageType.Error msg

class lsp_server =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server as super
    val mutable workspace : Workspace.t option = None
    val mutable ain : Ain.t = Ain.create 4 0

    method private load_workspace ~notify_back path =
      try
        let ws = Workspace.load path in
        workspace <- Some ws;
        ain <- Ain.load (Workspace.ain_path ws);
        log_info notify_back (Workspace.ain_path ws ^ " loaded")
      with e -> log_error notify_back (Exn.to_string e)

    method! on_req_initialize ~notify_back i =
      (match i.rootPath with
      | Some (Some path) -> self#load_workspace ~notify_back path
      | _ -> ());
      super#on_req_initialize ~notify_back i

    method on_notif_doc_did_open ~notify_back:_ _d ~content:_ : unit Linol_lwt.t
        =
      Linol_lwt.return ()

    method on_notif_doc_did_change ~notify_back:_ _d _c ~old_content:_old
        ~new_content:_ =
      Linol_lwt.return ()

    method on_notif_doc_did_close ~notify_back:_ _d : unit Linol_lwt.t =
      Linol_lwt.return ()
  end

let run () =
  let s = new lsp_server in
  let server = Linol_lwt.Jsonrpc2.create_stdio s in
  let task = Linol_lwt.Jsonrpc2.run server in
  Linol_lwt.run task

let () = run ()
