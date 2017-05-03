open Lwt
open Cohttp
open Cohttp_lwt_unix
open Bot


let () =
  let config = Config.read "config.json" in
  match config with
  | Result.Ok config ->
     let stream, push = Lwt_stream.create () in
     Lwt_main.run
         (match%lwt Bot.find_chat_id config with
          | Error e -> raise (Failure e)
          | Ok id ->
            let config = {config with Config.telegram_id = id} in
            Todo.fetcher config push <&> Bot.main config stream)
  | Result.Error e -> Printf.printf "Error in config:\n%s\n" e
