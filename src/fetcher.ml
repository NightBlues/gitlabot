open Lwt
open Cohttp
open Cohttp_lwt_unix
open Bot


let serve threads =
  let _runner i f = let%lwt res = f () in return i in
  let running_threads = List.mapi _runner threads in
  let rec loop running_threads =
    let%lwt finished, working = nchoose_split running_threads in
    let rerun = List.map (fun i -> _runner i @@ List.nth threads i) finished in
    loop (working @ rerun)
  in
  loop running_threads


let () =
  let config = Config.read "config.json" in
  match config with
  | Result.Ok config ->
     Lwt_main.run
         (match%lwt Bot.find_chat_id config with
          | Error e -> raise (Failure e)
          | Ok id ->
             let config = {config with Config.telegram_id = id} in
             let stream, push = Lwt_stream.create () in
             let threads = [(fun () -> Todo.fetcher config push);
                            (fun () -> Bot.main config stream)] in
            serve threads)
  | Result.Error e -> Printf.printf "Error in config:\n%s\n" e
