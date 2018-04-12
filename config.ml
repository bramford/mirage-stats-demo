open Mirage

let get_exn name fn =
  let res = Sys.getenv name in
  Printf.printf "\027[33mENV\027[m         %s => %s\n%!" name res;
  fn (String.lowercase_ascii res)

let get name ~default fn =
  try get_exn name fn
  with Not_found -> default

let ip name default =
  let fn = Ipaddr.V4.of_string_exn in
  get name ~default:(fn default) fn

let mask name default =
  let fn = Ipaddr.V4.Prefix.of_string_exn in
  get name ~default:(fn default) fn

let ipv4_config =
  let network  = (mask "NM" "10.0.0.0/24", ip "IP" "10.0.0.10") in
  let gateway = Some (ip "GW" "10.0.0.1") in
  { network; gateway }

let stack = static_ipv4_stack default_network ~config:ipv4_config

let server =
  conduit_direct stack

let http_srv = http_server server
let data = crunch "static"

let main =
  let packages = [
    package "str"; package "magic-mime"; package "re.str"; package "opam-lib.repository"; package "mirage-kv"
  ] in
  foreign
    ~packages
    "Dispatch.Main" (kv_ro @-> http @-> job)

let () =
  register "www" [
    main $ default_console $ data $ http_srv
  ]
