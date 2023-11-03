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

let rec base_type = function
  | Jaf.Array { data; _ } | Jaf.Wrap { data; _ } -> base_type data
  | t -> t

let get_definition (doc : Document.t) pos =
  let return =
    Option.map (fun loc ->
        let range = Document.to_lsp_range doc.text loc in
        let uri = Lsp.Types.DocumentUri.of_path (fst loc).pos_fname in
        `Location [ Lsp.Types.Location.create ~uri ~range ])
  in
  match Document.get_nodes_for_pos doc pos with
  | Jaf.ASTExpression { node = Ident (_, Some (GlobalVariable i)); _ } :: _ ->
      return (Ain.get_global_by_index doc.ctx.ain i).location
  | Jaf.ASTExpression { node = Ident (_, Some (FunctionName i)); _ } :: _ ->
      return (Ain.get_function_by_index doc.ctx.ain i).def_loc
  | Jaf.ASTType { spec; _ } :: _ -> (
      match base_type spec.data with
      | Struct (_, i) ->
          return (Ain.get_struct_by_index doc.ctx.ain i).location
      | FuncType (_, i) ->
          return (Ain.get_functype_by_index doc.ctx.ain i).location
      | Delegate (_, i) ->
          return (Ain.get_delegate_by_index doc.ctx.ain i).location
      | _ -> None)
  | _ -> None
