open Base
open Sys4c

type t = { ctx : Jaf.context; text : bytes; toplevel : Jaf.declaration list }

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

let to_lsp_position (text : bytes) p =
  Lsp.Types.Position.create ~line:(p.Lexing.pos_lnum - 1)
    ~character:
      (count_utf16_code_units_of_utf8_bytes text p.Lexing.pos_bol
         p.Lexing.pos_cnum)

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
  end

(* Returns the most specific node first. *)
let get_nodes_for_pos doc pos =
  let locator = new ast_locator doc pos in
  locator#visit_toplevel doc.toplevel;
  locator#nodes
