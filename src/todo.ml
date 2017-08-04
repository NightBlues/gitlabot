open Lwt
open Cohttp
open Cohttp_lwt_unix
open Logging

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
  log @@ fmt  "gitlab: request: %s" (Uri.to_string url);
  let connect () =
    match data with
    | Some data ->
       Client.post ~body:(Cohttp_lwt_body.of_string data) ~headers url
    | None ->
       match httpmethod with
       | Some httpmethod -> Client.call  ~headers httpmethod url
       | None -> Client.get ~headers url
  in
  let%lwt (resp, body) =
    try%lwt
          connect ()
    with
    | exn ->
       let msg = Printexc.to_string exn in
       log @@ fmt "Error occured while connecting to gitlab: %s" msg;
       fail exn
  in
  let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
  log @@ fmt "gitlab: got response %d for: %s" status (Uri.to_string url);
  if Cohttp.Code.is_success status then
    let%lwt body = Cohttp_lwt_body.to_string body in
    return_ok body
  else
    return_error (Printf.sprintf "response status %d" status)

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
     (log @@ fmt "Message matched: %s" data;
      send_f (Some data))
   else
     log @@ fmt "Message didn't match: %s\n" data);
  rpc_call ~httpmethod:`DELETE gitlab (Printf.sprintf "/%d" todo.id)
  >>= function
  | Result.Ok s -> log @@ fmt "gitlab: mark as done: %s" s; return ()
  | Result.Error e -> log @@ fmt "gitlab: error %s" e; return ()

let rec fetcher config send_f =
  let fetcher_ gitlab =
    let params = "?state=pending" in
    (* let params = "?state=done&per_page=5&page=1" in *)
    let%lwt body = rpc_call gitlab params in
    match body with
    | Result.Error e -> log "Bad rpc_call to gitlab"; return ()
    | Result.Ok body ->
       match of_string body with
       | Result.Ok body ->
          Lwt_list.map_s
            (fun todo -> handle_todo config gitlab todo send_f) body
          >>= fun _ -> return_unit
       | Result.Error e ->
          log @@ fmt "Error while parsing todos: %s\nin data:\n%s" e body;
          return ()
  in
  let%lwt _ =
    try%lwt
          Lwt_list.iter_p fetcher_ config.Config.gitlabs
    with
    | exn ->
       let msg = Printexc.to_string exn in
       log @@ fmt "Error occured while fetching todos: %s\n" msg;
       fail exn
  in
  Lwt_unix.sleep 30.
