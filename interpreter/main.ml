open Lexer;;
open Parser;;
open Interpreter;;

(* System Checks *)

if Array.length Sys.argv < 2 then begin
  print_string ("Error!\nNo input file. Exiting ...\n");
  exit 0;
end;;

if Array.length Sys.argv > 2 then begin
  print_string ("Error!\nToo many arguements. Exiting ...\n");
  exit 0;
end;;

let file = open_in Sys.argv.(1);;

let parsed_program = Parser.program Lexer.tokenize (Lexing.from_channel file);;

let program = preprocess_program parsed_program;;

print_string "Program loaded successfully.\n";;

try 
  while(true) do
    print_string ("| ?- ");
    let line = read_line() in
    if line = "halt." then exit 0
    else try
      let goal = Parser.goal Lexer.tokenize (Lexing.from_string line) in
      let (b, _) = (interpret program goal)
      in print_bool(b)
    with err -> print_string((Printexc.to_string err) ^ "\n")
  done

with _ -> print_string ("\nEnding interpreter!\n")