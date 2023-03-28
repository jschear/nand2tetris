open Core

(* Assembly code *)

module Dest = struct
  type t = { a : bool; d : bool; m : bool } [@@deriving sexp]

  let none = { a = false; d = false; m = false }
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

module Expr = struct
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
