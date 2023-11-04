open Base
open Sys4c

type t = {
  ain : Ain.t;
  srcdir : string;
  documents : (string, Document.t) Hashtbl.t;
}

val create : Ain.t -> string -> t
val set_document : t -> Lsp.Uri.t -> string -> Lsp.Types.Diagnostic.t list
val load_document : t -> string -> unit

(* LSP request handlers *)

val get_hover :
  t -> Lsp.Uri.t -> Lsp.Types.Position.t -> Lsp.Types.Hover.t option

val get_definition :
  t -> Lsp.Uri.t -> Lsp.Types.Position.t -> Lsp.Types.Locations.t option
