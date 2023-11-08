open Base
open Sys4c

(* LSP expects character offsets based on utf-16 representation. *)
let count_utf16_code_units_of_utf8_bytes bytes start end_ =
  let rec loop i n =
    if i >= end_ then n
    else
      let ch = Bytes.get bytes i in
      if Char.O.(ch <= '\x7f') then loop (i + 1) (n + 1)
      else if Char.O.(ch <= '\xbf') then failwith "invalid utf-8 sequence"
      else if Char.O.(ch <= '\xdf') then loop (i + 2) (n + 1)
      else if Char.O.(ch <= '\xef') then loop (i + 3) (n + 1)
      else if Char.O.(ch <= '\xf7') then loop (i + 4) (n + 2)
      else failwith "invalid utf-8 sequence"
  in
  loop start 0

let to_lsp_position (text : bytes) (p : Lexing.position) =
  try
    Lsp.Types.Position.create ~line:(p.pos_lnum - 1)
      ~character:
        (count_utf16_code_units_of_utf8_bytes text p.pos_bol p.pos_cnum)
  with _ ->
    Printf.failwithf "to_lsp_position failed: %s %d %d %d" p.pos_fname
      p.pos_lnum p.pos_bol p.pos_cnum ()

let to_lsp_range text (start, end_) =
  Lsp.Types.Range.create
    ~start:(to_lsp_position text start)
    ~end_:(to_lsp_position text end_)

let range_contains text range (pos : Lsp.Types.Position.t) =
  let Lsp.Types.Range.{ start; end_ } = to_lsp_range text range in
  (start.line < pos.line
  || (start.line = pos.line && start.character <= pos.character))
  && (end_.line > pos.line
     || (end_.line = pos.line && end_.character >= pos.character))

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

let predefined_constants =
  [
    Jaf.
      {
        name = "true";
        type_ =
          {
            spec = { data = Bool; qualifier = Some Const };
            location = dummy_loc;
          };
        location = dummy_loc;
        array_dim = [];
        initval = None;
        index = None;
      };
    Jaf.
      {
        name = "false";
        type_ =
          {
            spec = { data = Bool; qualifier = Some Const };
            location = dummy_loc;
          };
        location = dummy_loc;
        array_dim = [];
        initval = None;
        index = None;
      };
  ]

type t = {
  ctx : Jaf.context;
  text : bytes;
  toplevel : Jaf.declaration list;
  errors : (Lsp.Types.Range.t * string) list;
}

external reraise : exn -> 'a = "%reraise"

let make_error ain lexbuf exn =
  let make (lexbuf : Lexing.lexbuf) loc message =
    let range = to_lsp_range lexbuf.lex_buffer loc in
    (range, message)
  in
  match exn with
  | Lexer.Error | Parser.Error ->
      make lexbuf (lexbuf.lex_start_p, lexbuf.lex_curr_p) "Syntax error."
  | CompileError.SyntaxError (msg, loc) -> make lexbuf loc msg
  | CompileError.CompileError (msg, node) ->
      make lexbuf (Jaf.ast_node_pos node) msg
  | CompileError.Undefined_variable (name, node) ->
      make lexbuf (Jaf.ast_node_pos node) ("Undefined variable: " ^ name)
  | CompileError.Not_lvalue_error (_e, node) ->
      make lexbuf (Jaf.ast_node_pos node) "Lvalue expected."
  | CompileError.Type_error (expected, actual_opt, node) ->
      let actual =
        match actual_opt with
        | Some actual -> (
            match actual.valuetype with
            | Some t -> "\n Actual type: " ^ Ain.type_to_string_hum ain t
            | None -> "")
        | None -> ""
      in
      make lexbuf (Jaf.ast_node_pos node)
        ("Type error.\n Expected type: "
        ^ Ain.type_to_string_hum ain expected
        ^ actual)
  | CompileError.Arity_error (func, args, node) ->
      make lexbuf (Jaf.ast_node_pos node)
        (Printf.sprintf
           "Arity error. '%s' expects %d arguments, but %d provided." func.name
           func.nr_args (List.length args))
  | e -> reraise e

let create ain ~fname text =
  let lexbuf = Lexing.from_string text in
  Lexing.set_filename lexbuf fname;
  let ctx =
    Jaf.{ ain; import_ain = Ain.create 4 0; const_vars = predefined_constants }
  in
  try
    let toplevel = Parser.jaf Lexer.token lexbuf in
    Declarations.register_type_declarations ctx toplevel;
    Declarations.resolve_types ctx toplevel;
    Declarations.define_types ctx toplevel;
    let errors =
      TypeAnalysis.check_types ctx toplevel
      |> List.map ~f:(make_error ain lexbuf)
    in
    { ctx; text = lexbuf.lex_buffer; toplevel; errors }
  with e ->
    {
      ctx;
      text = lexbuf.lex_buffer;
      toplevel = [];
      errors = [ make_error ain lexbuf e ];
    }

class ast_locator (doc : t) (pos : Lsp.Types.Position.t) =
  object
    inherit Jaf.ivisitor doc.ctx as super
    val mutable nodes : Jaf.ast_node list = []
    method nodes = nodes

    method! visit_expression expr =
      if range_contains doc.text expr.loc pos then (
        nodes <- ASTExpression expr :: nodes;
        super#visit_expression expr)

    method! visit_statement stmt =
      if range_contains doc.text stmt.loc pos then (
        nodes <- ASTStatement stmt :: nodes;
        super#visit_statement stmt)

    method! visit_declaration decl =
      let node = Jaf.ASTDeclaration decl in
      if range_contains doc.text (Jaf.ast_node_pos node) pos then (
        nodes <- node :: nodes;
        super#visit_declaration decl)

    method! visit_struct_declaration decl =
      let node = Jaf.ASTStructDecl decl in
      if range_contains doc.text (Jaf.ast_node_pos node) pos then (
        nodes <- node :: nodes;
        super#visit_struct_declaration decl)

    method! visit_variable var =
      let node = Jaf.ASTVariable var in
      if range_contains doc.text (Jaf.ast_node_pos node) pos then (
        nodes <- node :: nodes;
        super#visit_variable var)

    method! visit_type_specifier t =
      let node = Jaf.ASTType t in
      if range_contains doc.text (Jaf.ast_node_pos node) pos then (
        nodes <- node :: nodes;
        super#visit_type_specifier t)
  end

(* Returns the most specific node first. *)
let get_nodes_for_pos doc pos =
  let locator = new ast_locator doc pos in
  locator#visit_toplevel doc.toplevel;
  locator#nodes
