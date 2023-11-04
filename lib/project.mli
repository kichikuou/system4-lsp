type t = {
  mutable ain : Sys4c.Ain.t;
  documents : (string, Document.t) Base.Hashtbl.t;
}

val create : Sys4c.Ain.t -> t
val set_document : t -> Lsp.Uri.t -> string -> Lsp.Types.Diagnostic.t list

(* LSP request handlers *)

val get_hover :
  t -> Lsp.Uri.t -> Lsp.Types.Position.t -> Lsp.Types.Hover.t option

val get_definition :
  t -> Lsp.Uri.t -> Lsp.Types.Position.t -> Lsp.Types.Locations.t option
