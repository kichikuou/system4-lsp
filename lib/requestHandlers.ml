open Sys4c

let get_hover doc pos =
  match Document.get_nodes_for_pos doc pos with
  | [] -> None
  | Jaf.ASTExpression { valuetype = Some t; loc; _ } :: _ ->
      let content = Ain.type_to_string_hum doc.ctx.ain t in
      let range = Document.to_lsp_range doc.text loc in
      Some
        (Lsp.Types.Hover.create
           ~contents:
             (`MarkupContent
               (Lsp.Types.MarkupContent.create ~kind:PlainText ~value:content))
           ~range ())
  | _ -> None

let get_definition (doc : Document.t) pos =
  let return_location loc =
    let range = Document.to_lsp_range doc.text loc in
    let uri = Lsp.Types.DocumentUri.of_path (fst loc).pos_fname in
    Some (`Location [ Lsp.Types.Location.create ~uri ~range ])
  in
  match Document.get_nodes_for_pos doc pos with
  | Jaf.ASTExpression { node = Ident (_, Some (GlobalVariable i)); _ } :: _ -> (
      match (Ain.get_global_by_index doc.ctx.ain i).location with
      | Some loc -> return_location loc
      | None -> None)
  | Jaf.ASTExpression { node = Ident (_, Some (FunctionName i)); _ } :: _ -> (
      match (Ain.get_function_by_index doc.ctx.ain i).def_loc with
      | Some loc -> return_location loc
      | None -> None)
  | _ -> None
