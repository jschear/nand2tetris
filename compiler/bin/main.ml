open Core
open Frontend
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse lexer lexbuf =
  try Parser.classDecEof lexer lexbuf with
  | Lexer.SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg;
      exit (-1)
  | Parser.Error ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf;
      exit (-1)

let list_files filename_or_directory =
  match Sys_unix.is_directory filename_or_directory with
  | `Yes ->
      Sys_unix.readdir filename_or_directory
      |> Array.to_list
      |> List.filter ~f:(fun filename ->
             String.is_suffix filename ~suffix:".jack")
      |> List.map ~f:(fun filename -> filename_or_directory ^ "/" ^ filename)
  | `No | `Unknown -> [ filename_or_directory ]

type output_files = {
  bytecode_file : string;
  tokens_file : string option;
  ast_file : string option;
}

let make_output_files filename ~emit_tokens ~emit_ast =
  let basename = Filename.chop_suffix filename ".jack" in
  let bytecode_file = basename ^ ".vm" in
  let tokens_file =
    match emit_tokens with true -> Some (basename ^ "~T.xml") | false -> None
  in
  let ast_file =
    match emit_ast with true -> Some (basename ^ "~.xml") | false -> None
  in
  { bytecode_file; tokens_file; ast_file }

(* TODO: draw the rest of the owl *)
let compile output_filename (ast : Ast.Class.t) =
  Out_channel.with_file output_filename ~f:(fun oc ->
      fprintf oc "// TODO codegen for %s" ast.name |> ignore)

let maybe_open_channel filename =
  match filename with
  | Some file -> Some (Out_channel.create file)
  | None -> None

let maybe_close_channel channel =
  match channel with Some channel -> Out_channel.close channel | None -> ()

let compile_file filename ~emit_tokens ~emit_ast =
  In_channel.with_file filename ~f:(fun ic ->
      let lexbuf = Lexing.from_channel ic in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

      let output_files = make_output_files filename ~emit_tokens ~emit_ast in
      let tokens_oc = maybe_open_channel output_files.tokens_file in
      let lexer =
        match tokens_oc with
        | Some oc -> Xml_tokens.wrap_lexer Lexer.read_token oc
        | None -> Lexer.read_token
      in
      let ast = parse lexer lexbuf in
      let () =
        match output_files.ast_file with
        | Some filename ->
            Out_channel.with_file filename ~f:(fun oc ->
                Xml_ast.XmlableClass.write ast oc)
        | None -> ()
      in
      maybe_close_channel tokens_oc;
      compile output_files.bytecode_file ast)

let compile filename_or_directory ~emit_tokens ~emit_ast =
  let input_files = list_files filename_or_directory in
  List.iter input_files ~f:(fun filename ->
      compile_file filename ~emit_tokens ~emit_ast)

let command =
  Command.basic ~summary:"Compile Jack code."
    (let%map_open.Command filename_or_directory =
       anon ("filename_or_directory" %: string)
     and emit_tokens =
       flag "--emit-tokens" no_arg
         ~doc:
           "Emit an XML file containing the stream of tokens for each Jack \
            file."
     and emit_ast =
       flag "--emit-ast" no_arg
         ~doc:
           "Emit an XML file containing the abstract syntax tree for each Jack \
            file."
     in
     fun () -> compile filename_or_directory ~emit_tokens ~emit_ast)

let () = Command_unix.run ~version:"1.0" command
