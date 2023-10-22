open Base
open Sys4c
open System4_lsp

let send_log_msg notify_back type_ msg =
  notify_back#send_log_msg ~type_ msg |> ignore

let log_info notify_back msg =
  send_log_msg notify_back Lsp.Types.MessageType.Info msg

let log_error notify_back msg =
  send_log_msg notify_back Lsp.Types.MessageType.Error msg

let make_diagnostic (lexbuf : Lexing.lexbuf) node_opt message =
  let range =
    match node_opt with
    | Some node ->
        Jaf.ast_node_pos node |> Document.to_lsp_range lexbuf.lex_buffer
    | None ->
        Document.to_lsp_range lexbuf.lex_buffer
          (lexbuf.lex_start_p, lexbuf.lex_curr_p)
  in
  Lsp.Types.Diagnostic.create ~range ~message ()

let predefined_constants =
  [
    Jaf.
      {
        name = "true";
        type_spec = { data = Bool; qualifier = Some Const };
        location = (Lexing.dummy_pos, Lexing.dummy_pos);
        array_dim = [];
        initval = None;
        index = None;
      };
    Jaf.
      {
        name = "false";
        type_spec = { data = Bool; qualifier = Some Const };
        location = (Lexing.dummy_pos, Lexing.dummy_pos);
        array_dim = [];
        initval = None;
        index = None;
      };
  ]

class lsp_server =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server as super
    val mutable workspace : Workspace.t option = None
    val mutable ain : Ain.t = Ain.create 4 0

    val buffers : (string, Document.t) Hashtbl.t =
      Hashtbl.create (module String)

    method private load_workspace ~notify_back path =
      try
        let ws = Workspace.load path in
        workspace <- Some ws;
        ain <- Ain.load (Workspace.ain_path ws);
        log_info notify_back (Workspace.ain_path ws ^ " loaded")
      with e -> log_error notify_back (Exn.to_string e)

    method spawn_query_handler f = Linol_lwt.spawn f

    method private _on_doc ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (uri : Lsp.Types.DocumentUri.t) (contents : string) =
      let lexbuf = Lexing.from_string contents in
      Lexing.set_filename lexbuf (Lsp.Types.DocumentUri.to_path uri);
      let errors =
        try
          let ctx =
            Jaf.
              {
                ain;
                import_ain = Ain.create 4 0;
                const_vars = predefined_constants;
              }
          in
          let jaf = Parser.jaf Lexer.token lexbuf in
          let document =
            Document.{ ctx; text = lexbuf.lex_buffer; toplevel = jaf }
          in
          Hashtbl.set buffers
            ~key:(Lsp.Types.DocumentUri.to_string uri)
            ~data:document;
          Declarations.resolve_types ctx jaf false;
          TypeAnalysis.check_types ctx jaf
        with e -> [ e ]
      in
      notify_back#send_diagnostic
        (self#handle_errors ~notify_back ~lexbuf errors)

    method private handle_errors ~notify_back ~lexbuf errors =
      List.filter_map errors ~f:(function
        | Parser.Error -> Some (make_diagnostic lexbuf None "Syntax error.")
        | CompileError.CompileError (msg, node) ->
            Some (make_diagnostic lexbuf (Some node) msg)
        | CompileError.Undefined_variable (name, node) ->
            Some
              (make_diagnostic lexbuf (Some node)
                 ("Undefined variable: " ^ name))
        | CompileError.Not_lvalue_error (_e, node) ->
            Some (make_diagnostic lexbuf (Some node) "Lvalue expected")
        | CompileError.Type_error (expected, actual_opt, node) ->
            let actual =
              match actual_opt with
              | Some actual -> (
                  match actual.valuetype with
                  | Some t -> "\n Actual type: " ^ Ain.Type.to_string t
                  | None -> "")
              | None -> ""
            in
            Some
              (make_diagnostic lexbuf (Some node)
                 ("Type error.\n Expected type: "
                 ^ Ain.Type.to_string expected
                 ^ actual))
        | CompileError.Arity_error (func, args, node) ->
            Some
              (make_diagnostic lexbuf (Some node)
                 (Printf.sprintf
                    "Arity error. '%s' expects %d arguments, but %d provided."
                    func.name func.nr_args (List.length args)))
        | e ->
            log_error notify_back
              (Exn.to_string e ^ "\n"
              ^ Backtrace.to_string (Backtrace.Exn.most_recent ()));
            None)

    (* Do not use incremental update, to work around a bug in lsp 1.14 where its
       content change application logic is confused when the newline code is CRLF. *)
    method! config_sync_opts =
      {
        (super#config_sync_opts) with
        change = Some Lsp.Types.TextDocumentSyncKind.Full;
      }

    method! on_req_initialize ~notify_back i =
      (match i.rootPath with
      | Some (Some path) -> self#load_workspace ~notify_back path
      | _ -> ());
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

let run () =
  let s = new lsp_server in
  let server = Linol_lwt.Jsonrpc2.create_stdio s in
  let task = Linol_lwt.Jsonrpc2.run server in
  Linol_lwt.run task

let () = run ()
