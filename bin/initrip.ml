let () =
  let ic = if Array.length Sys.argv > 1 then open_in Sys.argv.(1) else stdin in
  Ini.to_channel stdout (Ini.of_channel ic)
