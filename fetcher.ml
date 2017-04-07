open Lwt
open Cohttp
open Cohttp_lwt_unix


module Config = struct
  type uri_t = Uri.t
  let uri_t_of_yojson = function
    | `String u -> Result.Ok (Uri.of_string u)
    | _ -> Result.Error "uri must be string"

  type regexp_t = Str.regexp
  let regexp_t_of_yojson = function
    | `String r ->
       (try
         let r = Str.regexp ("^.*" ^ r) in
         Result.Ok r
       with Not_found -> Result.Error "filter must be valid regexp")
    | _ -> Result.Error "filter must be a regexp"

  type gitlab_t = {
      url: uri_t;
      access_key: string;
    } [@@deriving of_yojson]

  type t = {
      gitlabs: gitlab_t list;
      telegram: string;
      telegram_id: int;
      filter: regexp_t;
    } [@@deriving of_yojson]

  let read filename =
    Yojson.Safe.from_file filename |> of_yojson
end


module Todo = struct
  type author = {username: string;} [@@deriving of_yojson { strict = false }]
  type target = {description: string;} [@@deriving of_yojson { strict = false }]
  type t = {
      id: int;
      state: string;
      author: author;
      target_url: string;
      body: string;
      target: target;
    } [@@deriving of_yojson { strict = false }]
  type todos = t list [@@deriving of_yojson]
  let of_string data =
    Yojson.Safe.from_string data |> todos_of_yojson

  let rpc_call ?data ?httpmethod config postfix =
    let headers = Header.init_with "PRIVATE-TOKEN" config.Config.access_key in
    let url = Uri.to_string config.Config.url ^ postfix |> Uri.of_string in
    Printf.printf "gitlab: request: %s\n" (Uri.to_string url); flush_all ();
    (match data with
     | Some data ->
        let headers = Header.add
                        headers
                        "Content-Length"
                        (String.length data |> string_of_int)
        in
        Client.post ~body:(Cohttp_lwt_body.of_string data) ~headers url
     | None ->
        match httpmethod with
        | Some httpmethod -> Client.call  ~headers httpmethod url
        | None -> Client.get ~headers url)
    >>= fun (resp, body) -> Cohttp_lwt_body.to_string body

  let handle_todo config gitlab todo send_f =
    let data = Printf.sprintf "@%s:\n%s\n%s\n%s\n" todo.author.username
                              todo.body todo.target_url todo.target.description
    in
    if Str.string_match config.Config.filter data 0 then
      send_f (Some data);
    rpc_call ~httpmethod:`DELETE gitlab (Printf.sprintf "/%d" todo.id)
    >>= fun s -> Printf.printf "gitlab: mark as done: %s" s; flush_all ();return ()

  let rec fetcher config send_f =
    let fetcher_ gitlab =
      rpc_call gitlab "?state=pending"
      >>=
        fun body ->
        match of_string body with
        | Result.Ok body ->
           Lwt_list.map_s
             (fun todo -> handle_todo config gitlab todo send_f) body
             >>= fun _ -> return_unit
        | Result.Error e -> print_endline ("Error while parsing todos: " ^ e); return ()
    in
    Lwt_list.iter_p fetcher_ config.Config.gitlabs
    >>= fun _ -> Lwt_unix.sleep 30.
    >>= fun _ -> fetcher config send_f
end


module Bot = struct
  module Payload = struct
    let send_message chat_id text =
      `Assoc [
         ("chat_id", (`Int chat_id));
         ("text", (`String text))
       ] |> Yojson.to_string
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

  let rec main config stream =
    (* rpc_call ~data:"{\"update_id\":0}" config "getUpdates" *)
    Lwt_stream.get stream
    >>= function
    | Some msg ->
       send_message config msg
       >>= fun _ -> main config stream
    | None -> return ()
end


let () =
  let config = Config.read "config.json" in
  match config with
  | Result.Ok config ->
     let stream, push = Lwt_stream.create () in
     Lwt_main.run (Todo.fetcher config push <&> Bot.main config stream)
  | Result.Error e -> Printf.printf "Error in config:\n%s\n" e
