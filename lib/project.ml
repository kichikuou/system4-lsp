open Base
open Sys4c
open Document

type t = {
  ain : Ain.t;
  srcdir : string;
  documents : (string, Document.t) Hashtbl.t;
}

let create ain srcdir =
  { ain; srcdir; documents = Hashtbl.create (module String) }

let set_document proj uri contents =
  let doc =
    Document.create proj.ain ~fname:(Lsp.Types.DocumentUri.to_path uri) contents
  in
  Hashtbl.set proj.documents ~key:(Lsp.Types.DocumentUri.to_path uri) ~data:doc;
  List.map doc.errors ~f:(fun (range, message) ->
      Lsp.Types.Diagnostic.create ~range ~message ())

let load_document proj fname =
  let path = Stdlib.Filename.concat proj.srcdir fname in
  let contents = Stdio.In_channel.read_all path |> UtfSjis.sjis2utf in
  set_document proj (Lsp.Types.DocumentUri.of_path path) contents |> ignore

let rec base_type = function
  | Jaf.Array { data; _ } | Jaf.Wrap { data; _ } -> base_type data
  | t -> t

let get_hover proj uri pos =
  match Hashtbl.find proj.documents (Lsp.Types.DocumentUri.to_path uri) with
  | None -> None
  | Some doc -> (
      match get_nodes_for_pos doc pos with
      | [] -> None
      | Jaf.ASTExpression { valuetype = Some t; loc; _ } :: _ ->
          let content = Ain.type_to_string_hum proj.ain t in
          let range = to_lsp_range doc.text loc in
          Some
            (Lsp.Types.Hover.create
               ~contents:
                 (`MarkupContent
                   (Lsp.Types.MarkupContent.create ~kind:PlainText
                      ~value:content))
               ~range ())
      | _ -> None)

let get_definition proj uri pos =
  match Hashtbl.find proj.documents (Lsp.Types.DocumentUri.to_path uri) with
  | None -> None
  | Some doc -> (
      let return =
        Option.bind ~f:(fun loc ->
            let fname = (fst loc).Lexing.pos_fname in
            match Hashtbl.find proj.documents fname with
            | None -> None
            | Some doc ->
                let range = to_lsp_range doc.text loc in
                let uri = Lsp.Types.DocumentUri.of_path fname in
                Some (`Location [ Lsp.Types.Location.create ~uri ~range ]))
      in
      match get_nodes_for_pos doc pos with
      | Jaf.ASTExpression { node = Ident (_, Some (GlobalVariable i)); _ } :: _
        ->
          return (Ain.get_global_by_index proj.ain i).location
      | Jaf.ASTExpression { node = Ident (_, Some (FunctionName i)); _ } :: _ ->
          return (Ain.get_function_by_index proj.ain i).def_loc
      | Jaf.ASTType { spec; _ } :: _ -> (
          match base_type spec.data with
          | Struct (_, i) ->
              return (Ain.get_struct_by_index proj.ain i).location
          | FuncType (_, i) ->
              return (Ain.get_functype_by_index proj.ain i).location
          | Delegate (_, i) ->
              return (Ain.get_delegate_by_index proj.ain i).location
          | _ -> None)
      | _ -> None)
