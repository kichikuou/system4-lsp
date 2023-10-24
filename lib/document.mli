type t = {
  ctx : Sys4c.Jaf.context;
  text : bytes;
  toplevel : Sys4c.Jaf.declaration list;
  errors : (Lsp.Types.Range.t * string) list;
}

val create : Sys4c.Ain.t -> string -> t
val get_nodes_for_pos : t -> Lsp.Types.Position.t -> Sys4c.Jaf.ast_node list

val to_lsp_range :
  bytes -> Lexing.position * Lexing.position -> Lsp.Types.Range.t
