type t = Nominal.t

module Stringtbl = Hashtbl.Make(String)

type env = t Stringtbl.t

exception AlreadyInEnvironment of env * string

let empty () =
  Stringtbl.create 100

let lookup x env =
  Stringtbl.find env x

let rec fresh_string_of_t env str =
  try
    ignore (lookup env str);
    fresh_string_of_t env (str ^ "'")
  with Not_found -> str

let add env x nominal =
  try
    ignore (lookup env str);
    raise (AlreadyInEnvironment (env, str))
  with Not_found -> Stringtbl.add env x nominal
