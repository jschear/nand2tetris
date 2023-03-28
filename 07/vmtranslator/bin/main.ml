open Core

(* VM code *)
module Segment = struct
  type t = Local | Argument | This | That | Constant | Static | Pointer | Temp
  [@@deriving sexp]

  let of_string = function
    | "local" -> Local
    | "argument" -> Argument
    | "this" -> This
    | "that" -> That
    | "constant" -> Constant
    | "static" -> Static
    | "pointer" -> Pointer
    | "temp" -> Temp
    | _ -> raise (Failure "Unknown segment")

  let to_string = function
    | Local -> "local"
    | Argument -> "argument"
    | This -> "this"
    | That -> "that"
    | Constant -> "constant"
    | Static -> "static"
    | Pointer -> "pointer"
    | Temp -> "temp"

  let variable_for = function
    | Local -> "LCL"
    | Argument -> "ARG"
    | This -> "THIS"
    | That -> "THAT"
    | _ -> failwith "Segment has no associated variable"
end

module Expr = struct
  type t =
    | Push of Segment.t * int
    | Pop of Segment.t * int
    | Add
    | Sub
    | Neg
    | Eq
    | Gt
    | Lt
    | And
    | Or
    | Not
  [@@deriving sexp]
end

(* Assembly code *)
module Dest = struct
  type t = { a : bool; d : bool; m : bool } [@@deriving sexp]

  let a = { a = true; d = false; m = false }
  let d = { a = false; d = true; m = false }
  let m = { a = false; d = false; m = true }
  let ad = { a = true; d = true; m = false }
  let am = { a = true; d = false; m = true }
  let dm = { a = false; d = true; m = true }
  let adm = { a = true; d = true; m = true }

  let to_string { a; d; m } =
    if not (a || d || m) then ""
    else
      let a = if a then "A" else "" in
      let d = if d then "D" else "" in
      let m = if m then "M" else "" in
      Printf.sprintf "%s%s%s=" a d m
end

module Comp = struct
  type t =
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

  let to_string = function
    | Zero -> "0"
    | One -> "1"
    | Neg_one -> "-1"
    | D -> "D"
    | A -> "A"
    | Not_D -> "!D"
    | Not_A -> "!A"
    | Neg_D -> "-D"
    | Neg_A -> "-A"
    | D_plus_one -> "D+1"
    | A_plus_one -> "A+1"
    | D_minus_one -> "D-1"
    | A_minus_one -> "A-1"
    | D_plus_A -> "D+A"
    | D_minus_A -> "D-A"
    | A_minus_D -> "A-D"
    | D_and_A -> "D&A"
    | D_or_A -> "D|A"
    | M -> "M"
    | Not_M -> "!M"
    | Neg_M -> "-M"
    | M_plus_one -> "M+1"
    | M_minus_one -> "M-1"
    | D_plus_M -> "D+M"
    | D_minus_M -> "D-M"
    | M_minus_D -> "M-D"
    | D_and_M -> "D&M"
    | D_or_M -> "D|M"
end

module Jmp = struct
  type t = NULL | JGT | JEQ | JGE | JLT | JNE | JLE | JMP [@@deriving sexp]

  let to_string = function
    | NULL -> ""
    | JGT -> ";JGT"
    | JEQ -> ";JEQ"
    | JGE -> ";JGE"
    | JLT -> ";JLT"
    | JNE -> ";JNE"
    | JLE -> ";JLE"
    | JMP -> ";JMP"
end

module Value = struct
  type t = Literal of int | Variable of string [@@deriving sexp]

  let to_string = function Literal x -> Int.to_string x | Variable x -> x
end

module Asm = struct
  type t =
    | Label of string
    | A of Value.t
    | C of Dest.t * Comp.t * Jmp.t
    | Comment of string
  [@@deriving sexp]

  let to_string = function
    | Label x -> Printf.sprintf "(%s)" x
    | A x -> Value.to_string x
    | C (dest, comp, jmp) ->
        Printf.sprintf "%s%s%s" (Dest.to_string dest) (Comp.to_string comp)
          (Jmp.to_string jmp)
    | Comment x -> Printf.sprintf "// %s" x
end

(* VM translator implementation *)
let sp = Value.Variable "SP"

let trim_comment line =
  let comment_idx = String.substr_index line ~pattern:"//" in
  match comment_idx with
  | Some idx -> String.sub line ~pos:0 ~len:idx
  | None -> line

let construct_segment_value line ~constr =
  let parts = String.split line ~on:' ' in
  let segment = Segment.of_string (List.nth_exn parts 1) in
  let value = List.nth_exn parts 2 |> Int.of_string in
  constr segment value

let parse_line line =
  let first = String.split line ~on:' ' |> List.hd_exn in
  match first with
  | "push" -> construct_segment_value line ~constr:(fun x y -> Expr.Push (x, y))
  | "pop" -> construct_segment_value line ~constr:(fun x y -> Expr.Pop (x, y))
  | "add" -> Expr.Add
  | _ -> raise (Failure "Not implemented")

let parse (bytecode : string list) : Expr.t list =
  List.filter_map bytecode ~f:(fun line ->
      let trimmed = trim_comment line |> String.strip in
      match trimmed with "" -> None | x -> Some (parse_line x))

(* RAM[SP] = D; SP++ *)
let push_from_d_register_asm =
  [
    (* @SP *)
    Asm.A sp;
    (* A=M *)
    Asm.C (Dest.a, M, NULL);
    (* M=D *)
    Asm.C (Dest.m, D, NULL);
    (* @SP *)
    Asm.A sp;
    (* M=M+1 *)
    Asm.C (Dest.m, M_plus_one, NULL);
  ]

(* SP--; D = RAM[SP] *)
let pop_to_d_register_asm =
  [
    (* @SP *)
    Asm.A sp;
    (* AM=M-1 *)
    Asm.C (Dest.am, M_minus_one, NULL);
    (* D=M *)
    Asm.C (Dest.d, M, NULL);
  ]

let push_constant_asm value =
  List.concat
    [
      [
        Asm.Comment (Printf.sprintf "push constant %i" value);
        (* @i *)
        Asm.A (Literal value);
        (* D=A *)
        Asm.C (Dest.d, A, NULL);
      ];
      push_from_d_register_asm;
    ]

(* addr ← LCL + i *)
let calculate_addr_asm segment value =
  [
    (* @i *)
    Asm.A (Literal value);
    (* D=A *)
    Asm.C (Dest.d, A, NULL);
    (* @LCL *)
    Asm.A (Value.Variable (Segment.variable_for segment));
    (* A=D+M *)
    Asm.C (Dest.a, D_plus_M, NULL);
  ]

(* RAM[SP] ← RAM[addr]; SP++ *)
let push_from_addr_asm =
  List.concat
    [ [ (* D=M *) Asm.C (Dest.d, M, NULL) ]; push_from_d_register_asm ]

(* SP--; RAM[addr] ← RAM[SP] *)
let pop_to_addr_asm =
  let temp_register = "R13" in
  List.concat
    [
      (* Save the value of A in R13 *)
      [
        (* D=A *)
        Asm.C (Dest.d, A, NULL);
        (* @R13 *)
        Asm.A (Value.Variable temp_register);
        (* M=D *)
        Asm.C (Dest.m, D, NULL);
      ];
      pop_to_d_register_asm;
      (* Store the popped value in the addr pointed to by R13 *)
      [
        (* @R13 *)
        Asm.A (Value.Variable temp_register);
        (* A=M *)
        Asm.C (Dest.a, M, NULL);
        (* M=D *)
        Asm.C (Dest.m, D, NULL);
      ];
    ]

let push_segment_asm segment value =
  List.concat
    [
      [
        Asm.Comment
          (Printf.sprintf "push %s %i" (Segment.to_string segment) value);
      ];
      calculate_addr_asm segment value;
      push_from_addr_asm;
    ]

let pop_segment_asm segment value =
  List.concat
    [
      [
        Asm.Comment
          (Printf.sprintf "pop %s %i" (Segment.to_string segment) value);
      ];
      calculate_addr_asm segment value;
      pop_to_addr_asm;
    ]

let push_static_asm filename value =
  List.concat
    [
      [
        Asm.Comment (Printf.sprintf "push static %i" value);
        (* @filename.i *)
        Asm.A (Variable (Printf.sprintf "%s.%i" filename value));
      ];
      push_from_addr_asm;
    ]

let pop_static_asm filename value =
  List.concat
    [
      [
        Asm.Comment (Printf.sprintf "pop static %i" value);
        (* @filename.i *)
        Asm.A (Variable (Printf.sprintf "%s.%i" filename value));
      ];
      pop_to_addr_asm;
    ]

let push_temp_asm value =
  List.concat
    [
      [
        Asm.Comment (Printf.sprintf "push temp %i" value);
        (* @5 *)
        Asm.A (Literal 5);
        (* D=A *)
        Asm.C (Dest.d, A, NULL);
        (* @i *)
        Asm.A (Literal value);
        (* A=D+A *)
        Asm.C (Dest.a, D_plus_A, NULL);
      ];
      push_from_addr_asm;
    ]

let push_temp_asm value =
  List.concat
    [
      [
        Asm.Comment (Printf.sprintf "push temp %i" value);
        (* @5 *)
        Asm.A (Literal 5);
        (* D=A *)
        Asm.C (Dest.d, A, NULL);
        (* @i *)
        Asm.A (Literal value);
        (* D=D+A *)
      ];
      push_from_addr_asm;
    ]

let pop_temp_asm value =
  List.concat
    [
      [
        Asm.Comment (Printf.sprintf "pop temp %i" value);
        (* @5 *)
        Asm.A (Literal 5);
        (* D=A *)
        Asm.C (Dest.d, A, NULL);
        (* @i *)
        Asm.A (Literal value);
        (* D=D+A *)
      ];
      pop_to_addr_asm;
    ]

let var_for_pointer_value value =
  let addr =
    match value with
    | 0 -> Segment.This
    | 1 -> Segment.That
    | _ -> failwith "Unexpected value in 'pointer x'"
  in
  Segment.variable_for addr

let push_pointer_asm value =
  List.concat
    [
      [
        Asm.Comment (Printf.sprintf "push pointer %i" value);
        (* @THIS or @THAT *)
        Asm.A (Variable (var_for_pointer_value value));
      ];
      push_from_addr_asm;
    ]

let pop_pointer_asm value =
  List.concat
    [
      [
        Asm.Comment (Printf.sprintf "pop pointer %i" value);
        (* @THIS or @THAT *)
        Asm.A (Variable (var_for_pointer_value value));
      ];
      pop_to_addr_asm;
    ]

(* Arithmetic *)
let add_asm =
  List.concat
    [
      [ Asm.Comment "add" ];
      pop_to_d_register_asm;
      [
        (* A=A-1 *)
        Asm.C (Dest.a, A_minus_one, NULL);
        (* M=D+M *)
        Asm.C (Dest.m, D_plus_M, NULL);
      ];
    ]

let sub_asm =
  List.concat
    [
      [ Asm.Comment "sub" ];
      pop_to_d_register_asm;
      [
        (* A=A-1 *)
        Asm.C (Dest.a, A_minus_one, NULL);
        (* M=M-D *)
        Asm.C (Dest.m, M_minus_D, NULL);
      ];
    ]

let neg_asm =
  [
    Asm.Comment "neg";
    Asm.A sp;
    Asm.C (Dest.a, A_minus_one, NULL);
    Asm.C (Dest.m, Neg_M, NULL);
  ]

let not_asm =
  [
    Asm.Comment "not";
    Asm.A sp;
    Asm.C (Dest.a, A_minus_one, NULL);
    Asm.C (Dest.m, Not_M, NULL);
  ]

let and_asm =
  List.concat
    [
      [ Asm.Comment "and" ];
      pop_to_d_register_asm;
      [
        (* A=A-1 *)
        Asm.C (Dest.a, A_minus_one, NULL);
        (* M=D&M *)
        Asm.C (Dest.m, D_and_M, NULL);
      ];
    ]

let or_asm =
  List.concat
    [
      [ Asm.Comment "or" ];
      pop_to_d_register_asm;
      [
        (* A=A-1 *)
        Asm.C (Dest.a, A_minus_one, NULL);
        (* M=D|M *)
        Asm.C (Dest.m, D_or_M, NULL);
      ];
    ]

(* let eq_asm =
   List.concat
     [
       [ Asm.Comment "eq" ];
       pop_to_d_register_asm;
       [
         (* A=A-1 *)
         Asm.C (Dest.a, A_minus_one, NULL);
         (* M=D&M *)
         Asm.C (Dest.m, D_, NULL);
       ];
     ] *)

(* let lt_asm =
   List.concat
     [
       [ Asm.Comment "lt" ];
       pop_to_d_register_asm;
       [
         (* A=A-1 *)
         Asm.C (Dest.a, A_minus_one, NULL);
         (* M=D&M *)
         Asm.C (Dest.m, D_, NULL);
       ];
     ] *)

(* let gt_asm =
   List.concat
     [
       [ Asm.Comment "gt" ];
       pop_to_d_register_asm;
       [
         (* A=A-1 *)
         Asm.C (Dest.a, A_minus_one, NULL);
         (* M=D&M *)
         Asm.C (Dest.m, D_, NULL);
       ];
     ] *)

let asm_of_bytecode filename = function
  | Expr.Push (Constant, value) -> push_constant_asm value
  | Expr.Pop (Constant, _) -> failwith "Cannot pop to constant segment"
  | Expr.Push ((Local as segment), value)
  | Expr.Push ((Argument as segment), value)
  | Expr.Push ((This as segment), value)
  | Expr.Push ((That as segment), value) ->
      push_segment_asm segment value
  | Expr.Pop ((Local as segment), value)
  | Expr.Pop ((Argument as segment), value)
  | Expr.Pop ((This as segment), value)
  | Expr.Pop ((That as segment), value) ->
      pop_segment_asm segment value
  | Expr.Push (Static, value) -> push_static_asm filename value
  | Expr.Pop (Static, value) -> pop_static_asm filename value
  | Expr.Push (Pointer, value) -> push_pointer_asm value
  | Expr.Pop (Pointer, value) -> pop_pointer_asm value
  | Expr.Push (Temp, value) -> push_temp_asm value
  | Expr.Pop (Temp, value) -> pop_temp_asm value
  | Expr.Add -> add_asm
  | _ -> raise (Failure "Not implemented")

let translate filename bytecode : Asm.t list =
  List.concat_map bytecode ~f:(fun bytecode ->
      asm_of_bytecode filename bytecode)

let write filename program =
  List.map program ~f:Asm.to_string |> String.concat ~sep:"\n" |> fun data ->
  Out_channel.write_all filename ~data

let read_lines filename =
  In_channel.with_file filename ~f:In_channel.input_lines

let translate_to_file filename output =
  read_lines filename |> parse |> translate filename |> write output

let command =
  Command.basic ~summary:"Translate Hack VM code to Hack assembly."
    (let%map_open.Command filename = anon ("filename" %: string)
     and output = anon ("output" %: string) in
     fun () -> translate_to_file filename output)

let () = Command_unix.run ~version:"1.0" command
