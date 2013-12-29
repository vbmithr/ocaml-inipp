open Ini

let () =
  let ic = if Array.length Sys.argv > 1 then open_in Sys.argv.(1) else stdin in
  to_channel stdout (of_channel ic)
