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
