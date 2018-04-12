open Lwt
open Printf
(* open V1_LWT *)

let pb = Printf.bprintf

module Index = struct

  let header =
    "<meta charset=\"utf-8\">\n\
     <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n\
     <title>Index</title>\n\
     <link rel=\"stylesheet\" href=\"css/foundation.css\">\n\
     <script src=\"js/vendor/modernizr.js\"></script>"

  let footer =
    "<script src=\"js/vendor/jquery.js\"></script>\n\
     <script src=\"js/foundation.min.js\"></script>\n\
     <script>\n\
    \  $(document).foundation();\n\
     </script>"

  let manifest () =
    let buf = Buffer.create 1024 in
    let opam_r = OpamRepositoryBackend.default () in
    pb
      buf
      "<div class=\"large-4 columns\">\n\
            <h3>Build Manifest<br/><small>%d packages</small></h3>\n\
            <table>\n\
            <thead><tr><th>Name</th><th>Version</th></tr></thead>"
      (OpamPackage.Set.length (OpamRepository.packages opam_r))
    ;
    OpamPackage.Set.iter
      (fun pkg ->
        let name = (OpamPackage.name_to_string pkg) in
        let version = (OpamPackage.version_to_string pkg) in
        pb
          buf
          "<tr><td>%s</td><td>%s</td></tr>\n"
          name
          version
      )
      (OpamRepository.packages opam_r)
    ;
    pb buf "</table>\n\
            </div>";
    Buffer.contents buf

  let gc () =
    let open Gc in
    let k f = Printf.sprintf "%dk" (f / 1_000) in
    let m f = Printf.sprintf "%.0fm" (f /. 1_000_000.) in
    let t = Gc.stat () in
    Printf.sprintf
      "<div class=\"large-4 columns\">\n\
       <h3>Live GC Stats</h3>
       <table>\n\
       <tr><td>Allocated Bytes</td><td>%s</td></tr>\n\
       <tr><td>Head Words</td><td>%s</td></tr>\n\
       <tr><td>Love Words</td><td>%s</td></tr>\n\
       </table>\n\
       </div>"
      (m (Gc.allocated_bytes ()))
      (k t.heap_words)
      (k t.live_words)

  let create () =
    Printf.sprintf
      "<!doctype html>\n\
       <html class=\"no-js\ lang=\"en\">\n\
       <head>\n\
       %s\n\
       </head>\n\
       <body>\n\
       <div class=\"row\">
       <h1>Hello World!</h1>\
       </div>
       <div class=\"row\">\n\
       %s\n\
       %s\n\
       </div>\n\
       %s\n\
       </body>\n\
       </html>"
      header (gc ()) (manifest ()) footer

end

(* Split a URI into a list of path segments *)
let split_path uri =
  let path = Uri.path uri in
  Re_str.(split_delim (regexp_string "/") path)
  |> List.filter (fun e -> e <> "")

module Main (KV: Mirage_kv_lwt.RO) (S: Cohttp_lwt.S.Server) =
struct

(*
  (* get start time from xen *)
  let start_time () =
    OS.Xs.make () >>= fun client ->
    OS.Xs.(immediate client (fun x -> read x "vm")) >>= fun vm ->
    OS.Xs.(immediate client (fun x -> read x (vm^"/start_time")))
    >|= fun start_time ->
    (* TODO HACK strtod not implemented in minios, so can't use
       float_of_string without segfault *)
    let a = Str.split (Str.regexp "[.]+") start_time in
    let v = int_of_string (List.hd a) in
    let d_str = (List.hd (List.tl a)) in
    Printf.printf "%s %s (%s)\n" (List.hd a) d_str start_time;
    let d = int_of_string d_str in
    assert ((String.length d_str) == 2);
    (float_of_int v) +. ((float_of_int d) /. 100.0)
    (* end of HACK *)
*)

  (* write key in xenstore to let jitsu know we are ready *)
(*
  let im_ready c =
    let key = "data/status" and value = "ready" in
    OS.Xs.make () >>= fun xs ->
    OS.Xs.(immediate xs (fun h -> write h key value))
*)

  let read_static kv name =
    let open KV in
    size kv name >>= function
    | Error (`Unknown_key _) -> Lwt.return_none
    | Ok size ->
      read kv name (Int64.of_int 0) size >>= function
      | Error (`Unknown_key _) -> Lwt.return_none
      | Ok bufs -> Lwt.return (Some ((Cstruct.copyv bufs)))

  (* dispatch non-file URLs *)
  let rec dispatcher kv = function
    | [] -> dispatcher kv ["index.html"]
    | ["index.html"] ->
      let body = Index.create () in
      S.respond_string ~status:`OK ~body ()
    | path ->
      let path = String.concat "/" path in
      let mimetype = Magic_mime.lookup path in
      let headers = Cohttp.Header.of_list [
          "contents-type", mimetype
        ] in
      read_static kv path >>= function
      | None   -> S.respond_not_found ()
      | Some s -> S.respond_string ~headers ~status:`OK ~body:s ()

  let start kv http =
    Logs.info "Starting ....\n";
    (* HTTP callback *)
    let callback conn_id request body =
      let uri = Cohttp_lwt.Request.uri request in
      dispatcher kv (split_path uri)
    in
    let conn_closed (_,conn_id) =
      let cid = Cohttp.Connection.to_string conn_id in
      Logs.info (Printf.sprintf "conn %s closed" cid)
    in
    let mode = `TCP 80 in
    http mode (S.make ~conn_closed ~callback ())

end
