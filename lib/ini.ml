open Re_pcre
open Printf

let section_rex = regexp "[ \t]*\\[[ \t]*([^ \t]+)[ \t]*\\][ \t]*"
let pair_rex = regexp "[ \t]*([^ \t]+)[ \t]*=[ \t]*([^ \t]+)*[ \t]*"

module SM = Map.Make(String)

type t = string SM.t SM.t

type simple = (string * ((string * string) list)) list

let of_simple simple =
  let open SM in
  let stringmap_of_assoc l =
    List.fold_left (fun a (k, v) -> SM.add k v a) SM.empty l in
  List.fold_left (fun a (k, v) -> add k (stringmap_of_assoc v) a) empty simple

let to_simple ini = SM.(bindings (map bindings ini))

let of_channel ic =
  let open SM in
  let rec inner sec acc =
    try
      let l = String.trim (input_line ic) in
      match l with
      | "" -> inner sec acc
      | s when s.[0] = ';' || s.[0] = '#' -> inner sec acc
      | s when pmatch ~rex:section_rex s -> (* section *)
        let rex = section_rex in
        let subs = extract ~rex s in
        inner subs.(1) (add subs.(1) empty acc)
      | s when pmatch ~rex:pair_rex s -> (* pairs *)
        let rex = pair_rex in
        let subs = extract ~rex s in
        let old_map = find sec acc in
        inner sec (add sec (add subs.(1) subs.(2) old_map) acc)
      | _ -> acc
    with End_of_file -> acc
  in inner "" (singleton "" empty)

let to_channel oc ini =
  let open SM in
  iter (fun k v ->
      if k <> "" then fprintf oc "[%s]\n" k;
      iter (fun k v -> fprintf oc "%s = %s\n" k v) v;
      fprintf oc "\n"
    ) ini
