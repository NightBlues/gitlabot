open Lwt
open Cohttp
open Cohttp_lwt_unix
open Bot


let () =
  let config = Config.read "config.json" in
  match config with
  | Result.Ok config ->
     let stream, push = Lwt_stream.create () in
     Lwt_main.run (Todo.fetcher config push <&> Bot.main config stream)
  | Result.Error e -> Printf.printf "Error in config:\n%s\n" e
