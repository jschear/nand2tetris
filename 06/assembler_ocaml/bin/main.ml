open Core

type dest = { a : bool; d : bool; m : bool } [@@deriving sexp]

type comp =
  | Zero
  | One
  | Neg_one
  | D
  | A
  | Not_D
  | Not_A
  | Neg_D
  | Neg_A
  | D_plus_one
  | A_plus_one
  | D_minus_one
  | A_minus_one
  | D_plus_A
  | D_minus_A
  | A_minus_D
  | D_and_A
  | D_or_A
  | M
  | Not_M
  | Neg_M
  | M_plus_one
  | M_minus_one
  | D_plus_M
  | D_minus_M
  | M_minus_D
  | D_and_M
  | D_or_M
[@@deriving sexp]

type jmp = NULL | JGT | JEQ | JGE | JLT | JNE | JLE | JMP [@@deriving sexp]
type value = Literal of int | Variable of string [@@deriving sexp]

type instruction = Label of string | A of value | C of dest * comp * jmp
[@@deriving sexp]

type symbol_table = (string, int, String.comparator_witness) Map.t

let binstr_of_bool = function true -> "1" | false -> "0"

let str_of_dest dest =
  List.map ~f:binstr_of_bool [ dest.a; dest.d; dest.m ] |> String.concat

let dest_of_str str =
  let a = String.contains str 'A' in
  let d = String.contains str 'D' in
  let m = String.contains str 'M' in
  { a; d; m }

let str_of_comp = function
  | Zero -> "0101010"
  | One -> "0111111"
  | Neg_one -> "0111010"
  | D -> "0001100"
  | A -> "0110000"
  | Not_D -> "0001101"
  | Not_A -> "0110001"
  | Neg_D -> "0001111"
  | Neg_A -> "0110011"
  | D_plus_one -> "0011111"
  | A_plus_one -> "0110111"
  | D_minus_one -> "0001110"
  | A_minus_one -> "0110010"
  | D_plus_A -> "0000010"
  | D_minus_A -> "0010011"
  | A_minus_D -> "0000111"
  | D_and_A -> "0000000"
  | D_or_A -> "0010101"
  | M -> "1110000"
  | Not_M -> "1110001"
  | Neg_M -> "1110011"
  | M_plus_one -> "1110111"
  | M_minus_one -> "1110010"
  | D_plus_M -> "1000010"
  | D_minus_M -> "1010011"
  | M_minus_D -> "1000111"
  | D_and_M -> "1000000"
  | D_or_M -> "1010101"

let comp_of_str = function
  | "0" -> Zero
  | "1" -> One
  | "-1" -> Neg_one
  | "D" -> D
  | "A" -> A
  | "!D" -> Not_D
  | "!A" -> Not_A
  | "-D" -> Neg_D
  | "-A" -> Neg_A
  | "D+1" -> D_plus_one
  | "A+1" -> A_plus_one
  | "D-1" -> D_minus_one
  | "A-1" -> A_minus_one
  | "D+A" -> D_plus_A
  | "D-A" -> D_minus_A
  | "A-D" -> A_minus_D
  | "D&A" -> D_and_A
  | "D|A" -> D_or_A
  | "M" -> M
  | "!M" -> Not_M
  | "-M" -> Neg_M
  | "M+1" -> M_plus_one
  | "M-1" -> M_minus_one
  | "D+M" -> D_plus_M
  | "D-M" -> D_minus_M
  | "M-D" -> M_minus_D
  | "D&M" -> D_and_M
  | "D|M" -> D_or_M
  | x -> failwith ("Unexpected comp: " ^ x)

let str_of_jmp = function
  | NULL -> "000"
  | JGT -> "001"
  | JEQ -> "010"
  | JGE -> "011"
  | JLT -> "100"
  | JNE -> "101"
  | JLE -> "110"
  | JMP -> "111"

let jmp_of_str = function
  | "JGT" -> JGT
  | "JEQ" -> JEQ
  | "JGE" -> JGE
  | "JLT" -> JLT
  | "JNE" -> JNE
  | "JLE" -> JLE
  | "JMP" -> JMP
  | _ -> failwith "Not a jmp."

let str_of_instruction = function
  | Label _ -> failwith "Unexpected label."
  | A (Literal value) ->
      "0"
      ^ Pp_binary_ints.Int.make_to_string ~zero_padding:true ~left_padding:true
          ~min_width:15 ~separators:false ~prefix:false () value
  | A (Variable value) -> failwith "Unbound variable: " ^ value
  | C (dest, comp, jump) ->
      String.concat
        [ "111"; str_of_comp comp; str_of_dest dest; str_of_jmp jump ]

let trim_comment line =
  let comment_idx = String.substr_index line ~pattern:"//" in
  match comment_idx with
  | Some idx -> String.sub line ~pos:0 ~len:idx
  | None -> line

let parse_c_instruction line =
  let expression, jmp =
    match String.lsplit2 line ~on:';' with
    | None -> (line, NULL)
    | Some (before, jump_str) -> (before, jmp_of_str jump_str)
  in
  let opt_dest, comp =
    match String.lsplit2 expression ~on:'=' with
    | None -> (None, comp_of_str (String.strip expression))
    | Some (before, comp_str) ->
        (Some before, comp_of_str (String.strip comp_str))
  in
  let dest =
    match opt_dest with
    | Some x -> dest_of_str x
    | None -> { a = false; d = false; m = false }
  in
  C (dest, comp, jmp)

let parse_label line =
  let trimmed =
    String.strip ~drop:(fun char -> Char.(char = '(' || char = ')')) line
  in
  Label trimmed

let parse_a_instruction line =
  let trimmed = String.strip ~drop:(fun char -> Char.(char = '@')) line in
  match int_of_string_opt trimmed with
  | Some x -> A (Literal x)
  | None -> A (Variable trimmed)

let parse_line line =
  let parse =
    match String.get line 0 with
    | '@' -> parse_a_instruction
    | '(' -> parse_label
    | _ -> parse_c_instruction
  in
  parse line

let parse (assembly : string list) : instruction list =
  List.filter_map assembly ~f:(fun line ->
      let trimmed = trim_comment line |> String.strip in
      match trimmed with "" -> None | x -> Some (parse_line x))

let constant_symbol_alist =
  [
    ("R0", 0);
    ("R1", 1);
    ("R2", 2);
    ("R3", 3);
    ("R4", 4);
    ("R5", 5);
    ("R6", 6);
    ("R7", 7);
    ("R8", 8);
    ("R9", 9);
    ("R10", 10);
    ("R11", 11);
    ("R12", 12);
    ("R13", 13);
    ("R14", 14);
    ("R15", 15);
    ("SCREEN", 16384);
    ("KBD", 24576);
    ("SP", 0);
    ("LCL", 1);
    ("ARG", 2);
    ("THIS", 3);
    ("THAT", 4);
  ]

let first_pass (program : instruction list) : instruction list * symbol_table =
  let symbol_table = Map.of_alist_exn (module String) constant_symbol_alist in

  let _, symbols_with_labels =
    List.fold program ~init:(0, symbol_table)
      ~f:(fun (line_number, symbols) instr ->
        let new_line_number =
          match instr with
          | A _ | C _ -> line_number + 1
          | Label _ -> line_number
        in
        let new_symbols =
          match instr with
          | A _ | C _ -> symbols
          | Label new_symbol ->
              Map.add_exn symbols ~key:new_symbol ~data:new_line_number
        in
        (new_line_number, new_symbols))
  in

  let program_without_labels =
    List.filter program ~f:(fun instr ->
        match instr with Label _ -> false | _ -> true)
  in
  (program_without_labels, symbols_with_labels)

let second_pass (program, symbols) : instruction list =
  let _, instructions =
    List.fold_map program ~init:(16, symbols)
      ~f:(fun (next_idx, symbols) instr ->
        match instr with
        | C _ -> ((next_idx, symbols), instr)
        | Label _ -> ((next_idx, symbols), instr)
        | A (Literal _) -> ((next_idx, symbols), instr)
        | A (Variable variable) -> (
            match Map.find symbols variable with
            | Some value -> ((next_idx, symbols), A (Literal value))
            | None ->
                let new_symbols =
                  Map.add_exn symbols ~key:variable ~data:next_idx
                in
                ((next_idx + 1, new_symbols), A (Literal next_idx))))
  in
  instructions

let compile (assembly : string list) : instruction list =
  parse assembly |> first_pass |> second_pass

let write filename program =
  let data =
    List.map program ~f:(fun instruction -> str_of_instruction instruction)
    |> String.concat ~sep:"\n"
  in
  Out_channel.write_all filename ~data

let read_lines filename =
  In_channel.with_file filename ~f:In_channel.input_lines

let compile_to_file filename output =
  read_lines filename |> compile |> write output

let command =
  Command.basic ~summary:"Compile a Hack assembly file to Hack machine code."
    ~readme:(fun () -> "More detailed information!")
    (let%map_open.Command filename = anon ("filename" %: string)
     and output = anon ("output" %: string) in
     fun () -> compile_to_file filename output)

let () = Command_unix.run ~version:"1.0" command
