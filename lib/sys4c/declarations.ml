(* Copyright (C) 2021 Nunuhara Cabbage <nunuhara@haniwa.technology>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://gnu.org/licenses/>.
 *)

open Base
open Jaf
open CompileError

(*
 * AST pass over top-level declarations register names in the .ain file.
 *)
class type_declare_visitor ctx = object (self)
  inherit ivisitor ctx

  val functions = Stack.create ()

  method declare_function (decl : fundecl) =
    decl.index <- Some (match Ain.get_function ctx.ain decl.name with
      | Some f -> f.index;
      | None -> (Ain.add_function ctx.ain decl.name).index);
    Stack.push functions decl

  method! visit_declaration decl =
    match decl with
    | Global (d) ->
        List.iter d.vars ~f:(fun g ->
          match g.type_.spec.qualifier with
          | Some Const -> ()
          | _ ->
              g.index <- Some (match Ain.get_global ctx.ain g.name with
                | Some g -> g.index
                | None -> Ain.add_global ctx.ain g.name))
    | Function (f) ->
        Option.iter f.struct_name ~f:(fun s_name ->
          f.name <- s_name ^ "@" ^ f.name;
          f.class_index <- Ain.get_struct_index ctx.ain s_name);
        self#declare_function f
    | FuncTypeDef (f) ->
        if Option.is_none (Ain.get_functype ctx.ain f.name) then
          ignore (Ain.add_functype ctx.ain f.name : Ain.FunctionType.t)
    | DelegateDef (f) ->
        if Option.is_none (Ain.get_delegate ctx.ain f.name) then
          ignore (Ain.add_delegate ctx.ain f.name : Ain.FunctionType.t)
    | StructDef (s) ->
        let ain_s = (match Ain.get_struct ctx.ain s.name with
          | Some s -> s
          | None -> Ain.add_struct ctx.ain s.name) in
        let visit_decl = function
          | AccessSpecifier _ -> ()
          | Constructor f ->
              if not (String.equal f.name s.name) then
                compile_error "constructor name doesn't match struct name" (ASTDeclaration (Function f));
              f.name <- s.name ^ "@0";
              f.class_index <- Some ain_s.index;
              self#declare_function f
          | Destructor f ->
              if not (String.equal f.name s.name) then
                compile_error "destructor name doesn't match struct name" (ASTDeclaration (Function f));
              f.name <- s.name ^ "@1";
              f.class_index <- Some ain_s.index;
              self#declare_function f
          | Method f ->
              f.name <- s.name ^ "@" ^ f.name;
              f.class_index <- Some ain_s.index;
              self#declare_function f
          | MemberDecl _ -> ()
        in
        List.iter s.decls ~f:visit_decl
    | Enum (_) ->
        compile_error "enum types not yet supported" (ASTDeclaration decl)
end

let register_type_declarations ctx decls =
  (new type_declare_visitor ctx)#visit_toplevel decls

(*
 * AST pass to resolve user-defined types (struct/enum/function types).
 *)
class type_resolve_visitor ctx decl_only = object (self)
  inherit ivisitor ctx as super

  method resolve_type name node =
    match Ain.get_struct_index ctx.ain name with
    | Some i -> Struct (name, i)
    | None ->
        begin match Ain.get_struct ctx.import_ain name with
        | Some s ->
            (* import struct declaration *)
            Struct (name, Ain.write_new_struct ctx.ain s)
        | None ->
            begin match Ain.get_functype_index ctx.ain name with
            | Some i -> FuncType (name, i)
            | None ->
                begin match Ain.get_functype ctx.import_ain name with
                | Some ft ->
                    (* import functype declaration *)
                    FuncType (name, Ain.write_new_functype ctx.ain ft)
                | None ->
                    begin match Ain.get_delegate_index ctx.ain name with
                    | Some i -> Delegate (name, i)
                    | None ->
                        begin match Ain.get_delegate ctx.import_ain name with
                        | Some ft ->
                            (* import delegate declaration *)
                            Delegate (name, Ain.write_new_delegate ctx.ain ft)
                        | None ->
                            compile_error ("Undefined type: " ^ name) node
                        end
                    end
                end
            end
        end

  method resolve_typespec ts node =
    match ts.data with
    | Unresolved (t) ->
        ts.data <- self#resolve_type t node
    | Array (t)
    | Wrap (t) ->
        self#resolve_typespec t node
    | _ -> ()

  method! visit_expression expr =
    begin match expr.node with
    | New (Unresolved (t), e, _) ->
        expr.node <- New (self#resolve_type t (ASTExpression expr), e, None)
    | _ -> ()
    end;
    super#visit_expression expr

  method! visit_variable decl =
    self#resolve_typespec decl.type_.spec (ASTVariable decl);
    super#visit_variable decl

  method! visit_declaration decl =
    let resolve_function f =
      self#resolve_typespec f.return.spec (ASTDeclaration(Function f));
      List.iter f.params ~f:(fun v -> self#resolve_typespec v.type_.spec (ASTVariable v))
    in
    begin match decl with
    | Function (f) ->
        resolve_function f
    | FuncTypeDef (f) | DelegateDef (f) ->
        resolve_function f
    | Global (_) -> ()
    | StructDef (s) ->
        let resolve_structdecl = function
          | AccessSpecifier _
          | MemberDecl (_) ->
              ()
          | Constructor (f)
          | Destructor (f)
          | Method (f) ->
              resolve_function f
        in
        List.iter s.decls ~f:resolve_structdecl
    | Enum (_) ->
        compile_error "enum types not yet supported" (ASTDeclaration decl)
    end;
    if not decl_only then
      super#visit_declaration decl
end

let resolve_types ctx decls decl_only =
  (new type_resolve_visitor ctx decl_only)#visit_toplevel decls

(*
 * AST pass over top-level declarations to define function/struct types.
 *)
class type_define_visitor ctx = object
  inherit ivisitor ctx

  method! visit_declaration decl =
    match decl with
    | Global (d) ->
        List.iter d.vars ~f:(fun g ->
          match g.type_.spec.qualifier with
          | Some Const ->
              ctx.const_vars <- g::ctx.const_vars  (* FIXME: replace existing entry *)
          | _ ->
              Ain.set_global_type_loc ctx.ain g.name (jaf_to_ain_type g.type_.spec) g.location)
    | Function (f) ->
        let obj = Ain.get_function_by_index ctx.ain (Option.value_exn f.index) in
        obj |> jaf_to_ain_function f |> Ain.write_function ctx.ain
    | FuncTypeDef (f) ->
        begin match Ain.get_functype ctx.ain f.name with
        | Some (obj) -> obj |> jaf_to_ain_functype f |> Ain.write_functype ctx.ain
        | None -> compiler_bug "undefined functype" (Some(ASTDeclaration decl))
        end
    | DelegateDef (f) ->
        begin match Ain.get_delegate ctx.ain f.name with
        | Some (obj) -> obj |> jaf_to_ain_functype f |> Ain.write_delegate ctx.ain
        | None -> compiler_bug "undefined delegate" (Some(ASTDeclaration decl))
        end
    | StructDef (s) ->
        begin match Ain.get_struct ctx.ain s.name with
        | Some (obj) -> obj |> jaf_to_ain_struct s |> Ain.write_struct ctx.ain
        | None -> compiler_bug "undefined struct" (Some(ASTDeclaration decl))
        end
    | Enum (_) ->
        compile_error "Enum types not yet supported" (ASTDeclaration decl)
end

let define_types ctx decls =
  (new type_define_visitor ctx)#visit_toplevel decls

let define_library ctx decls name =
  let is_struct_def decl =
    match decl with
    | StructDef (_) -> true
    | _ -> false
  in
  let (struct_defs, fun_decls) = List.partition_tf decls ~f:is_struct_def in
  (* handle struct definitions *)
  register_type_declarations ctx struct_defs;
  resolve_types ctx struct_defs true;
  define_types ctx struct_defs;
  (* define library *)
  let functions = List.map fun_decls ~f:(function
    | Function (f) -> jaf_to_ain_hll_function f
    | decl -> compiler_bug "unexpected declaration in .hll file" (Some (ASTDeclaration decl))
  ) in
  let lib = { (Ain.add_library ctx.ain name) with functions } in
  Ain.write_library ctx.ain lib
