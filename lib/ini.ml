let section_rex = Re.Pcre.regexp "[ \t]*\\[[ \t]*([^ \t]+)[ \t]*\\][ \t]*"
let pair_rex = Re.Pcre.regexp "[ \t]*([^ \t]+)[ \t]*=[ \t]*([^ \t]+)*[ \t]*"

module SM = Map.Make (String)

type t = string SM.t SM.t
type simple = (string * (string * string) list) list

let of_simple simple =
  let open SM in
  let stringmap_of_assoc l =
    List.fold_left (fun a (k, v) -> SM.add k v a) SM.empty l in
  List.fold_left (fun a (k, v) -> add k (stringmap_of_assoc v) a) empty simple

let to_simple ini = SM.(bindings (map bindings ini))

let of_channel ic =
  let open Re.Pcre in
  let rec inner sec acc =
    try
      let l = String.trim (input_line ic) in
      match l with
      | "" -> inner sec acc
      | s when s.[0] = ';' || s.[0] = '#' -> inner sec acc
      | s when pmatch ~rex:section_rex s ->
          (* section *)
          let subs = extract ~rex:section_rex s in
          inner subs.(1) SM.(add subs.(1) empty acc)
      | s when pmatch ~rex:pair_rex s ->
          (* pairs *)
          let rex = pair_rex in
          let subs = extract ~rex s in
          let old_map = SM.find sec acc in
          inner sec SM.(add sec (add subs.(1) subs.(2) old_map) acc)
      | _ -> acc
    with End_of_file -> acc in
  inner "" SM.empty

let to_channel oc ini =
  SM.iter
    (fun k v ->
      if k <> "" then Printf.fprintf oc "[%s]\n" k ;
      SM.iter (fun k v -> Printf.fprintf oc "%s = %s\n" k v) v )
    ini
