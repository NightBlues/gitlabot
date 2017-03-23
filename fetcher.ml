open Lwt
open Cohttp
open Cohttp_lwt_unix


module Config = struct
  type uri_t = Uri.t
  let uri_t_of_yojson = function
    | `String u -> Result.Ok (Uri.of_string u)
    | _ -> Result.Error "uri must be string"

  type t = {
      url: uri_t;
      access_key: string;
    } [@@deriving of_yojson]

  let read filename =
    Yojson.Safe.from_file filename |> of_yojson

  let of_strings access_key url =
    let url = Uri.of_string url in
    {access_key;url}
end


let fetcher config =
  let headers = Header.init_with "PRIVATE-TOKEN" config.Config.access_key in
  Client.get ~headers config.Config.url
  >>= fun (resp, body) -> Cohttp_lwt_body.to_string body
  >|= fun body -> Printf.printf "%s\n" body
       

let () =
  let config = Config.read "config.json" in
  match config with
    | Result.Ok config -> Lwt_main.run (fetcher config)
    | Result.Error e -> Printf.printf "Error in config:\n%s\n" e
