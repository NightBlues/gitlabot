type uri_t = Uri.t
let uri_t_of_yojson = function
  | `String u -> Result.Ok (Uri.of_string u)
  | _ -> Result.Error "uri must be string"

type regexp_t = Str.regexp
let regexp_t_of_yojson = function
  | `String r ->
     (try
        let r = Str.regexp ("\\(\n\\|.\\)*" ^ r) in
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
    telegram_username: string;
    telegram_id: int; [@default 0]
    filter: regexp_t;
  } [@@deriving of_yojson]

let read filename =
  Yojson.Safe.from_file filename |> of_yojson
