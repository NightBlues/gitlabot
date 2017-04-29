open Lwt
open Cohttp
open Cohttp_lwt_unix


type author_t = {
    name: string;
    username: string;
  } [@@deriving of_yojson { strict = false }]
type target_t = {
    description: string;
    assignee: author_t option;
  } [@@deriving of_yojson { strict = false }]
type t = {
    id: int;
    state: string;
    author: author_t;
    target_url: string;
    body: string;
    target: target_t;
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
  let data = Printf.sprintf "@%s:\n%s\n---\n%s (assigned to %s)\n%s\n"
                            todo.author.username
                            todo.body todo.target_url
                            (match todo.target.assignee with
                             | Some author -> author.username
                             | None -> "nobody")
                            todo.target.description
  in
  if Str.string_match config.Config.filter data 0 then
    send_f (Some data);
  rpc_call ~httpmethod:`DELETE gitlab (Printf.sprintf "/%d" todo.id)
  >>= fun s -> Printf.printf "gitlab: mark as done: %s" s; flush_all ();return ()

let rec fetcher config send_f =
  let fetcher_ gitlab =
    let%lwt body = rpc_call gitlab "?state=pending" in
    match of_string body with
    | Result.Ok body ->
       Lwt_list.map_s
         (fun todo -> handle_todo config gitlab todo send_f) body
       >>= fun _ -> return_unit
    | Result.Error e ->
       Printf.sprintf "Error while parsing todos: %s\nin data:\n%s" e body
       |> print_endline ; return ()
  in
  let%lwt _ = Lwt_list.iter_p fetcher_ config.Config.gitlabs in
  let%lwt _ = Lwt_unix.sleep 30. in
  fetcher config send_f
