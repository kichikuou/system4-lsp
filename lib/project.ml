open Base
open Sys4c
open Document

type t = {
  mutable ain : Ain.t;
  mutable srcdir : string;
  documents : (string, Document.t) Hashtbl.t;
}

let create () =
  {
    ain = Ain.create 4 0;
    srcdir = "";
    documents = Hashtbl.create (module String);
  }

let initialize proj (options : Types.InitializationOptions.t) =
  proj.srcdir <- options.srcDir;
  if not (String.is_empty options.ainPath) then
    proj.ain <- Ain.load options.ainPath

let update_document proj uri contents =
  let doc =
    Document.create proj.ain ~fname:(Lsp.Types.DocumentUri.to_path uri) contents
  in
  Hashtbl.set proj.documents ~key:(Lsp.Types.DocumentUri.to_path uri) ~data:doc;
  List.map doc.errors ~f:(fun (range, message) ->
      Lsp.Types.Diagnostic.create ~range ~message ())

let load_document proj fname =
  let path = Stdlib.Filename.concat proj.srcdir fname in
  let contents = Stdio.In_channel.read_all path |> UtfSjis.sjis2utf in
  update_document proj (Lsp.Types.DocumentUri.of_path path) contents |> ignore

let rec base_type = function
  | Jaf.Array { data; _ } | Jaf.Wrap { data; _ } -> base_type data
  | t -> t

let get_hover proj uri pos =
  match Hashtbl.find proj.documents (Lsp.Types.DocumentUri.to_path uri) with
  | None -> None
  | Some doc -> (
      let make_hover location content =
        Some
          (Lsp.Types.Hover.create
             ~contents:
               (`MarkupContent
                 (Lsp.Types.MarkupContent.create ~kind:PlainText ~value:content))
             ~range:(to_lsp_range doc.text location)
             ())
      in
      match get_nodes_for_pos doc pos with
      | Jaf.ASTExpression
          { node = Member (_, _, Some (SystemFunction sys)); loc; _ }
        :: _ ->
          let f = Bytecode.function_of_syscall sys in
          make_hover loc (Ain.function_to_string_hum proj.ain f)
      | Jaf.ASTExpression
          { node = Member (_, _, Some (HLLFunction (lib_no, fun_no))); loc; _ }
        :: _ ->
          let f = Ain.function_of_hll_function_index proj.ain lib_no fun_no in
          make_hover loc (Ain.function_to_string_hum proj.ain f)
      | Jaf.ASTExpression
          { node = Member (obj, _, Some (BuiltinMethod builtin)); loc; _ }
        :: _ ->
          let elem_t =
            Option.(
              obj.valuetype >>= function
              | { data = Array t; _ } -> Some t
              | _ -> None)
          in
          let f = Bytecode.function_of_builtin builtin elem_t in
          make_hover loc (Ain.function_to_string_hum proj.ain f)
      | Jaf.ASTExpression { valuetype = Some t; loc; _ } :: _ ->
          make_hover loc (Ain.type_to_string_hum proj.ain t)
      | Jaf.ASTType { spec; location } :: _ -> (
          match base_type spec.data with
          | Struct (_, i) ->
              let s = Ain.get_struct_by_index proj.ain i in
              make_hover location ("class " ^ s.name)
          | FuncType (_, i) ->
              let ft = Ain.get_functype_by_index proj.ain i in
              make_hover location ("functype " ^ ft.name)
          | Delegate (_, i) ->
              let dg = Ain.get_delegate_by_index proj.ain i in
              make_hover location ("delegate " ^ dg.name)
          | _ -> None)
      | _ -> None)

let filename_of_func ain (func : Ain.Function.t) =
  let code = Ain.get_code ain in
  let rec find_eof addr =
    match Bytecode.opcode_of_int (Stdlib.Bytes.get_int16_le code addr) with
    | EOF ->
        let i = Stdlib.Bytes.get_int32_le code (addr + 2) in
        Ain.get_filename ain (Int32.to_int_exn i)
    | op ->
        let nr_args = List.length (Bytecode.args_of_opcode op) in
        find_eof (addr + 2 + (nr_args * 4))
  in
  find_eof func.address

let backslash_to_slash = String.map ~f:(function '\\' -> '/' | c -> c)

let location_of_func proj i =
  let func = Ain.get_function_by_index proj.ain i in
  match func.def_loc with
  | Some loc -> Some loc
  | None ->
      (* Load .jaf file and try again. *)
      let fname = filename_of_func proj.ain func in
      load_document proj (backslash_to_slash fname);
      (Ain.get_function_by_index proj.ain i).def_loc

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
          return (location_of_func proj i)
      | Jaf.ASTExpression { node = Member (_, _, Some (ClassMethod (_, i))); _ }
        :: _ ->
          return (location_of_func proj i)
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
