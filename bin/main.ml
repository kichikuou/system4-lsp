open Base
open Sys4c
open System4_lsp

let send_log_msg notify_back type_ msg =
  notify_back#send_log_msg ~type_ msg |> ignore

let log_info notify_back msg =
  send_log_msg notify_back Lsp.Types.MessageType.Info msg

let log_error notify_back msg =
  send_log_msg notify_back Lsp.Types.MessageType.Error msg

let to_lsp_position p =
  Lsp.Types.Position.create ~line:(p.Lexing.pos_lnum - 1)
    ~character:(p.Lexing.pos_cnum - p.Lexing.pos_bol)

let to_lsp_range (start, end_) =
  Lsp.Types.Range.create ~start:(to_lsp_position start)
    ~end_:(to_lsp_position end_)

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
      let diagnostics =
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
          Declarations.resolve_types ctx jaf false;
          TypeAnalysis.check_types ctx jaf;
          []
        with
        | Parser.Error ->
            let diag =
              Lsp.Types.Diagnostic.create
                ~range:(to_lsp_range (lexbuf.lex_start_p, lexbuf.lex_curr_p))
                ~message:"Syntax error." ()
            in
            [ diag ]
        | CompileError.CompileError (msg, node) ->
            let diag =
              Lsp.Types.Diagnostic.create
                ~range:(Jaf.ast_node_pos node |> to_lsp_range)
                ~message:msg ()
            in
            [ diag ]
        | CompileError.Undefined_variable (name, node) ->
            let diag =
              Lsp.Types.Diagnostic.create
                ~range:(Jaf.ast_node_pos node |> to_lsp_range)
                ~message:("Undefined variable: " ^ name)
                ()
            in
            [ diag ]
      in
      notify_back#send_diagnostic diagnostics

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

    method on_notif_doc_did_close ~notify_back:_ _d : unit Linol_lwt.t =
      Linol_lwt.return ()
  end

let run () =
  let s = new lsp_server in
  let server = Linol_lwt.Jsonrpc2.create_stdio s in
  let task = Linol_lwt.Jsonrpc2.run server in
  Linol_lwt.run task

let () = run ()
