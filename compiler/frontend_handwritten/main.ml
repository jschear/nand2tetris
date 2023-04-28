open Core
open Tokens

let tokenize filename_or_directory =
  let is_directory = Sys_unix.is_directory filename_or_directory in
  let input_files, output_filename =
    match is_directory with
    | `Yes ->
        let files =
          Sys_unix.readdir filename_or_directory
          |> Array.to_list
          |> List.filter ~f:(fun filename ->
                 String.is_suffix filename ~suffix:".jack")
          |> List.map ~f:(fun filename ->
                 filename_or_directory ^ "/" ^ filename)
        in
        ( files,
          filename_or_directory ^ "/"
          ^ Filename.basename filename_or_directory
          ^ ".asm" )
    | `No | `Unknown ->
        ( [ filename_or_directory ],
          Filename.chop_suffix filename_or_directory ".jack" ^ "Me.xml" )
  in
  Out_channel.with_file output_filename ~f:(fun oc ->
      List.iter input_files ~f:(fun filename ->
          let _ = Filename.chop_suffix (Filename.basename filename) ".jack" in
          In_channel.with_file filename ~f:(fun ic ->
              Printf.fprintf oc "<tokens>\n";
              let rec consume_and_output tok =
                match Tokenizer.next_token tok with
                | None, tok -> tok
                | Some token, tok ->
                    let type_tag = Token.to_type_tag token in
                    Printf.fprintf oc "<%s> %s </%s>\n" type_tag
                      (Token.to_string token) type_tag;
                    consume_and_output tok
              in
              let tokenizer = Tokenizer.create ic in
              let _ = consume_and_output tokenizer in
              Printf.fprintf oc "</tokens>\n")))

let command =
  Command.basic ~summary:"Tokenize Jack code."
    (let%map_open.Command filename_or_directory =
       anon ("filename_or_directory" %: string)
     in
     fun () -> tokenize filename_or_directory)

let () = Command_unix.run ~version:"1.0" command
