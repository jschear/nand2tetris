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
    | Goto of string
    | IfGoto of string
    | Label of string
    | Call of string * int
    | Function of string * int
    | Return
  [@@deriving sexp]

  let to_string = function
    | Push (segment, index) ->
        sprintf "push %s %d" (Segment.to_string segment) index
    | Pop (segment, index) ->
        sprintf "pop %s %d" (Segment.to_string segment) index
    | Add -> "add"
    | Sub -> "sub"
    | Neg -> "neg"
    | Eq -> "eq"
    | Gt -> "gt"
    | Lt -> "lt"
    | And -> "and"
    | Or -> "or"
    | Not -> "not"
    | Goto label -> sprintf "goto %s" label
    | IfGoto label -> sprintf "if-goto %s" label
    | Label label -> sprintf "label %s" label
    | Call (name, n_args) -> sprintf "call %s %d" name n_args
    | Function (name, n_locals) -> sprintf "function %s %d" name n_locals
    | Return -> "return"
end
