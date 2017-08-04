
let timestamp () =
  let date = Unix.localtime (Unix.time ()) in
  let open Unix in
  Printf.sprintf "[%02d.%02d %02d:%02d:%02d] " date.tm_mday (date.tm_mon + 1)
                 date.tm_hour date.tm_min date.tm_sec

let log str =
  Printf.sprintf "%s%s" (timestamp ()) str|> print_endline

let fmt = Printf.sprintf
