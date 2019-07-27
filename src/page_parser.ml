open Omd

(* HTML generation using Omd *)
let generate lex s =
  let module E = struct
    include Omd_parser.Default_env(struct end)
  end
  in 
  let module Parser = Omd_parser.Make(E) in 
  let md = Parser.parse (lex s) in
  Parser.make_paragraphs md

let generate_html s = to_html (generate Omd_lexer.lex s)

(* Filename to string of contents *)
let stringify file = 
  let content = ref "" in
  let channel = open_in file in 
    try 
      while true; do
        content := !content ^ input_line channel ^ "\n"
      done; !content
    with End_of_file -> 
      close_in channel;
      !content;;
