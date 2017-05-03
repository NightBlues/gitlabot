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
    action_name: string;
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

let prettify_url url =
  let rec loop = function
    | [] | [_] | [_;_] -> url
    | _ :: [repo;_;num] -> Printf.sprintf "!%s/%s" repo num
    | _ :: tl -> loop tl
  in
  loop (Str.split_delim (Str.regexp "/") url)

let handle_todo config gitlab todo send_f =
  let data = Printf.sprintf "@%s %s in [%s](%s) (assigned to %s):\n%s\n---\n%s\n"
                            todo.author.username todo.action_name
                            (prettify_url todo.target_url)
                            todo.target_url
                            (match todo.target.assignee with
                             | Some author -> author.username
                             | None -> "nobody")
                             todo.body todo.target.description
  in
  (if Str.string_match config.Config.filter data 0 then
     (Printf.printf "\nMessage matched: %s\n" data;
      send_f (Some data))
   else
     Printf.printf "\nMessage didn't match: %s\n" data; flush_all ());
  rpc_call ~httpmethod:`DELETE gitlab (Printf.sprintf "/%d" todo.id)
  >>= fun s -> Printf.printf "gitlab: mark as done: %s" s; flush_all ();return ()

let rec fetcher config send_f =
  let fetcher_ gitlab =
    let params = "?state=pending" in
    (* let params = "?state=done&per_page=5&page=1" in *)
    let%lwt body = rpc_call gitlab params in
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
