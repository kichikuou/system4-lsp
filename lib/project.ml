open Base
open Sys4c
open Document

type encoding = UTF8 | ShiftJIS

let encoding_of_string s =
  match String.lowercase s with
  | "utf-8" -> UTF8
  | "shift_jis" -> ShiftJIS
  | _ -> raise (Invalid_argument ("Invalid encoding: " ^ s))

type t = {
  mutable ctx : Jaf.context;
  mutable srcdir : string;
  mutable srcEncoding : encoding;
  documents : (string, Document.t) Hashtbl.t;
}

let create () =
  let ctx = Jaf.context_from_ain (Ain.create 4 0) in
  {
    ctx;
    srcdir = "";
    srcEncoding = ShiftJIS;
    documents = Hashtbl.create (module String);
  }

let initialize proj (options : Types.InitializationOptions.t) =
  proj.srcdir <- options.srcDir;
  if not (String.is_empty options.ainPath) then
    proj.ctx <- Jaf.context_from_ain (Ain.load options.ainPath);
  if not (String.is_empty options.srcEncoding) then
    proj.srcEncoding <- encoding_of_string options.srcEncoding

let update_document proj uri contents =
  let doc =
    Document.create proj.ctx ~fname:(Lsp.Types.DocumentUri.to_path uri) contents
  in
  Hashtbl.set proj.documents ~key:(Lsp.Types.DocumentUri.to_path uri) ~data:doc;
  List.map doc.errors ~f:(fun (range, message) ->
      Lsp.Types.Diagnostic.create ~range ~message:(`String message) ())

let load_document proj fname =
  let path = Stdlib.Filename.concat proj.srcdir fname in
  let to_utf8 =
    match proj.srcEncoding with UTF8 -> Fn.id | ShiftJIS -> Sjis.to_utf8
  in
  let contents = Stdio.In_channel.read_all path |> to_utf8 in
  update_document proj (Lsp.Types.DocumentUri.of_path path) contents |> ignore

let rec jaf_base_type = function
  | Jaf.Ref t | Jaf.Array t | Jaf.Wrap t -> jaf_base_type t
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
      | Jaf.ASTExpression { node = Member (_, _, SystemFunction sys); loc; _ }
        :: _ ->
          let f = Builtin.fundecl_of_syscall sys in
          make_hover loc (Jaf.decl_to_string (Function f))
      | Jaf.ASTExpression
          { node = Member (_, _, HLLFunction (lib_name, fun_name)); loc; _ }
        :: _ ->
          Option.bind (Jaf.find_hll_function proj.ctx lib_name fun_name)
            ~f:(fun decl -> make_hover loc (Jaf.decl_to_string (Function decl)))
      | (Jaf.ASTExpression
           { node = Member (obj, _, BuiltinMethod builtin); loc; _ } as ast_node)
        :: _ ->
          let elem_t = match obj.ty with Array t -> t | _ -> Void in
          let f =
            Builtin.fundecl_of_builtin proj.ctx builtin elem_t (Some ast_node)
          in
          make_hover loc (Jaf.decl_to_string (Function f))
      | Jaf.ASTExpression
          { node = Member (_, _, ClassMethod (name, _)); loc; _ }
        :: _
      | Jaf.ASTExpression { node = Ident (_, FunctionName name); loc; _ } :: _
        ->
          Option.bind (Hashtbl.find proj.ctx.functions name) ~f:(fun decl ->
              make_hover loc
                (Jaf.decl_to_string (Function { decl with body = None })))
      | Jaf.ASTExpression { ty; loc; _ } :: _ ->
          make_hover loc (Jaf.jaf_type_to_string ty)
      | Jaf.ASTType { ty; location } :: _ -> (
          match jaf_base_type ty with
          | Struct (_, i) ->
              let s = Ain.get_struct_by_index proj.ctx.ain i in
              make_hover location ("class " ^ s.name)
          | FuncType (Some (_, i)) ->
              let ft = Ain.get_functype_by_index proj.ctx.ain i in
              make_hover location ("functype " ^ ft.name)
          | Delegate (Some (_, i)) ->
              let dg = Ain.get_delegate_by_index proj.ctx.ain i in
              make_hover location ("delegate " ^ dg.name)
          | _ -> None)
      | (Jaf.ASTStructDecl sdecl as decl) :: _ ->
          make_hover (Jaf.ast_node_pos decl) (Jaf.sdecl_to_string sdecl)
      | _ -> None)

let filename_of_func ain (func : Ain.Function.t) =
  let code = Ain.get_code ain in
  let rec find_eof addr =
    match Bytecode.opcode_of_int (Stdlib.Bytes.get_int16_le code addr) with
    | EOF ->
        let i = Stdlib.Bytes.get_int32_le code (addr + 2) in
        Ain.get_file ain (Int32.to_int_exn i)
    | op ->
        let nr_args = List.length (Bytecode.args_of_opcode op) in
        find_eof (addr + 2 + (nr_args * 4))
  in
  find_eof func.address

let backslash_to_slash = String.map ~f:(function '\\' -> '/' | c -> c)

let location_of_func proj name =
  match Hashtbl.find proj.ctx.functions name with
  | Some f when Option.is_some f.body -> Some f.loc
  | _ ->
      (* Load .jaf file and try again. *)
      Option.(
        Ain.get_function proj.ctx.ain name >>= fun func ->
        filename_of_func proj.ctx.ain func >>= fun fname ->
        load_document proj (backslash_to_slash fname);
        Hashtbl.find proj.ctx.functions name >>| fun f -> f.loc)

let find_location proj uri pos f =
  match Hashtbl.find proj.documents (Lsp.Types.DocumentUri.to_path uri) with
  | None -> None
  | Some doc -> (
      match f (get_nodes_for_pos doc pos) with
      | Some loc -> (
          let fname = (fst loc).Lexing.pos_fname in
          match Hashtbl.find proj.documents fname with
          | None -> None
          | Some doc ->
              let range = to_lsp_range doc.text loc in
              let uri = Lsp.Types.DocumentUri.of_path fname in
              Some (`Location [ Lsp.Types.Location.create ~uri ~range ]))
      | None -> None)

let get_definition proj uri pos =
  find_location proj uri pos (function
    | Jaf.ASTExpression { node = Ident (_, LocalVariable loc); _ } :: _ ->
        Some loc
    | Jaf.ASTExpression { node = Ident (name, GlobalVariable _); _ } :: _ -> (
        match Hashtbl.find proj.ctx.globals name with
        | Some v -> Some v.location
        | None -> None)
    | Jaf.ASTExpression { node = Ident (_, FunctionName name); _ } :: _
    | Jaf.ASTExpression { node = Member (_, _, ClassMethod (name, _)); _ } :: _
      ->
        location_of_func proj name
    | Jaf.ASTExpression
        {
          node = Member ({ ty = Struct (s_name, _); _ }, m_name, ClassVariable _);
          _;
        }
      :: _ ->
        let s = Hashtbl.find_exn proj.ctx.structs s_name in
        let v = Hashtbl.find_exn s.members m_name in
        Some v.location
    | Jaf.ASTType { ty; _ } :: _ -> (
        match jaf_base_type ty with
        | Struct (name, _) -> Some (Hashtbl.find_exn proj.ctx.structs name).loc
        | FuncType (Some (name, _)) ->
            Some (Hashtbl.find_exn proj.ctx.functypes name).loc
        | Delegate (Some (name, _)) ->
            Some (Hashtbl.find_exn proj.ctx.delegates name).loc
        | _ -> None)
    | Jaf.ASTStructDecl (Method d | Constructor d | Destructor d) :: _ ->
        location_of_func proj (Jaf.mangled_name d)
    | _ -> None)

let get_type_definition proj uri pos =
  find_location proj uri pos (function
    | Jaf.ASTExpression { ty; _ } :: _ -> (
        match jaf_base_type ty with
        | Struct (name, _) -> Some (Hashtbl.find_exn proj.ctx.structs name).loc
        | FuncType (Some (name, _)) ->
            Some (Hashtbl.find_exn proj.ctx.functypes name).loc
        | Delegate (Some (name, _)) ->
            Some (Hashtbl.find_exn proj.ctx.delegates name).loc
        | _ -> None)
    | _ -> None)
