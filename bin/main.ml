class lsp_server =
  object
    inherit Linol_lwt.Jsonrpc2.server

    method on_notif_doc_did_open ~notify_back:_ _d ~content:_ : unit Linol_lwt.t =
      Linol_lwt.return ()

    method on_notif_doc_did_change ~notify_back:_ _d _c ~old_content:_old ~new_content:_ =
      Linol_lwt.return ()

    method on_notif_doc_did_close ~notify_back:_ _d : unit Linol_lwt.t =
      Linol_lwt.return ()
  end

let run () =
  let s = new lsp_server in
  let server = Linol_lwt.Jsonrpc2.create_stdio s in
  let task = Linol_lwt.Jsonrpc2.run server in
  match Linol_lwt.run task with
  | () -> ()
  | exception e ->
    let e = Printexc.to_string e in
    Printf.eprintf "error: %s\n%!" e;
    exit 1

let () = run ()
