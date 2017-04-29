open Lwt
open Cohttp
open Cohttp_lwt_unix


module Payload = struct
  let send_message chat_id text =
    `Assoc [
       ("chat_id", (`Int chat_id));
       ("text", (`String text))
     ] |> Yojson.to_string

  let get_updates id =
    `Assoc [
       ("update_id", (`Int id))] |> Yojson.to_string
end

module Responses = struct
  type chat = { username: string; id: int } [@@deriving of_yojson { strict = false }]
  type message = { chat: chat } [@@deriving of_yojson { strict = false }]
  type msg_wrap = { update_id: int; message: message } [@@deriving of_yojson { strict = false }]
  type updates = { result: msg_wrap list } [@@deriving of_yojson { strict = false }]
end


let rpc_call ?data config api_method =
  let headers = Header.init_with "Content-Type" "application/json" in
  let url = (config.Config.telegram ^ api_method)  |> Uri.of_string in
  (match data with
   | Some data -> Client.post ~body:(Cohttp_lwt_body.of_string data) ~headers url
   | None -> Client.get ~headers url)
  >>= fun (resp, body) -> Cohttp_lwt_body.to_string body

let send_message config text =
  let msg = Payload.send_message config.Config.telegram_id text in
  Printf.printf "telegram: sending message: \n%s\n" msg;
  rpc_call ~data:msg config "sendMessage"

let get_updates ?(id=0) config =
  rpc_call ~data:(Payload.get_updates id) config "getUpdates"
  >>= fun s -> Yojson.Safe.from_string s |> Responses.updates_of_yojson |> return

let find_chat_id ?(id=0) config =
  let open Responses in
  let rec loop chat_id = function
    | [] -> chat_id
    | hd::tl -> let id = if hd.message.chat.username = config.Config.telegram_username
                         then hd.message.chat.id else chat_id
                in loop id tl
  in
  get_updates ~id config >>= function
  | Ok upd -> (match loop 0 upd.result with
               | 0 -> return (Error "Could not find chat_id - send smth to your bot.")
               | id -> return (Ok id))
  | Error e -> return (Error e)

let main config stream =
  let rec loop config stream =
    Lwt_stream.get stream
    >>= function
    | Some msg ->
       send_message config msg
       >>= fun _ -> loop config stream
    | None -> return ()
  in
  match%lwt find_chat_id config with
  | Error e -> raise (Failure e)
  | Ok id -> loop {config with Config.telegram_id = id} stream
