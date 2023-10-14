open Core

type t = {
  root_path : string;
  source_dir : string;
  output_dir : string;
  ain_name : string;
}

let ain_path ws = Filename.concat ws.output_dir ws.ain_name

let pje_load path =
  let lines = In_channel.read_lines path in
  let re = Str.regexp {|^\([A-Za-z]+\) *= *"\([^"]*\)"|} in
  List.filter_map
    ~f:(fun line ->
      if Str.string_match re line 0 then
        let key = Str.matched_group 1 line in
        let value = Str.matched_group 2 line in
        Some (key, value)
      else None)
    lines

let pje_get pje key =
  match List.Assoc.find ~equal:String.equal pje key with
  | Some value -> value
  | None -> Printf.failwithf "\"%s\" not found in pje file." key ()

let load root_path =
  let pje_files =
    Sys_unix.readdir root_path
    |> Array.filter ~f:(fun file -> Filename.check_suffix file ".pje")
  in
  match pje_files with
  | [||] -> failwith "No pje file in workspace root."
  | [| pje_file |] ->
      let pje_file = Filename.concat root_path pje_file in
      let pje = pje_load pje_file in
      let source_dir = pje_get pje "SourceDir" in
      let output_dir = pje_get pje "OutputDir" in
      let ain_name = pje_get pje "CodeName" in
      { root_path; source_dir; output_dir; ain_name }
  | _ -> failwith "Too many pje files in workspace root."
