open Ast
open Parser
open Lexer
open Printf
open String

let parse_program file_name =
  let channel = open_in file_name in
  let lexbuf = Lexing.from_channel channel in
  try
    let ast = Parser.program Lexer.token lexbuf in
    close_in channel;
    ast
  with
  | Lexer.InvalidToken c ->
      Printf.printf "Lexer error: Invalid token '%c'\n" c;
      exit 1
;;

let write_to_file file_name content =
  let oc = open_out file_name in
  output_string oc content;
  close_out oc
;;

let () =
  if Array.length Sys.argv <> 2 || length Sys.argv.(1) < 5  then begin
    Printf.printf "Usage: %s input_file.mol \n" Sys.argv.(0);
    exit 1
  end else begin
    let file_name = Sys.argv.(1) in
    let s = sub file_name 0 (length file_name -4) in
    let program_ast = parse_program file_name in
    let ast_str = Ast.string_of_program program_ast in
    write_to_file (s ^ ".ast") ast_str
  end
  
;;

