(* Serving up that sweet, sweet content *)
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Printf

(* Builder *)
let builder file =
  let md = Page_parser.stringify file in 
  let html = Page_parser.generate_html md in 
  let oc = open_out (file ^ ".html") in 
    fprintf oc "%s\n" html;
    close_out oc;; 

let output_meta uri meth header = 
  builder "test.md";
  Lwt_io.printf "URI: %s, Method: %s \n" uri meth

(* Simple regex to extract subpages *)
let strip str = 
  let re = Re2.create_exn "\\/(\\w*)" in 
    match Re2.find_all re str with
    | Core_kernel.Result.Ok s -> List.nth s ((List.length s) - 1)
    | Core_kernel.Result.Error _ -> ""

(* Routing function *)
let router = function 
  | "/" -> "index.html"
  | "/test" -> "test.md.html"
  | _ -> "404.html"

(* Server *)
let serve =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt.Body.to_string |> (fun body -> 
    Lwt.ignore_result (output_meta (strip uri) meth headers);
    let fname = router (strip uri) in
      Server.respond_file ~fname ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run serve);;