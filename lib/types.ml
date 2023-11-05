open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module InitializationOptions = struct
  type t = { srcDir : string; [@default ""] ainPath : string [@default ""] }
  [@@deriving_inline of_yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lib/types.ml.InitializationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
         let srcDir_field = ref Ppx_yojson_conv_lib.Option.None
         and ainPath_field = ref Ppx_yojson_conv_lib.Option.None
         and duplicates = ref []
         and extra = ref [] in
         let rec iter = function
           | (field_name, _field_yojson) :: tail ->
               (match field_name with
               | "srcDir" -> (
                   match Ppx_yojson_conv_lib.( ! ) srcDir_field with
                   | Ppx_yojson_conv_lib.Option.None ->
                       let fvalue = string_of_yojson _field_yojson in
                       srcDir_field := Ppx_yojson_conv_lib.Option.Some fvalue
                   | Ppx_yojson_conv_lib.Option.Some _ ->
                       duplicates :=
                         field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
               | "ainPath" -> (
                   match Ppx_yojson_conv_lib.( ! ) ainPath_field with
                   | Ppx_yojson_conv_lib.Option.None ->
                       let fvalue = string_of_yojson _field_yojson in
                       ainPath_field := Ppx_yojson_conv_lib.Option.Some fvalue
                   | Ppx_yojson_conv_lib.Option.Some _ ->
                       duplicates :=
                         field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
               | _ -> ());
               iter tail
           | [] -> ()
         in
         iter field_yojsons;
         match Ppx_yojson_conv_lib.( ! ) duplicates with
         | _ :: _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
               _tp_loc
               (Ppx_yojson_conv_lib.( ! ) duplicates)
               yojson
         | [] -> (
             match Ppx_yojson_conv_lib.( ! ) extra with
             | _ :: _ ->
                 Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
                   _tp_loc
                   (Ppx_yojson_conv_lib.( ! ) extra)
                   yojson
             | [] ->
                 let srcDir_value, ainPath_value =
                   ( Ppx_yojson_conv_lib.( ! ) srcDir_field,
                     Ppx_yojson_conv_lib.( ! ) ainPath_field )
                 in
                 {
                   srcDir =
                     (match srcDir_value with
                     | Ppx_yojson_conv_lib.Option.None -> ""
                     | Ppx_yojson_conv_lib.Option.Some v -> v);
                   ainPath =
                     (match ainPath_value with
                     | Ppx_yojson_conv_lib.Option.None -> ""
                     | Ppx_yojson_conv_lib.Option.Some v -> v);
                 }))
     | _ as yojson ->
         Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
           yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  [@@@deriving.end]

  let default = { srcDir = ""; ainPath = "" }
end
