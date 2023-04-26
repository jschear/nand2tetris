(* Abstract syntax tree for the Jack language. *)

module Identifier = struct
  type t = string
end

module Type = struct
  type t = Int | Char | Boolean | ClassName of Identifier.t
end

module rec Term : sig
  type keyword_constant = True | False | Null | This
  type unary_op = Minus | Not
  type op = Plus | Minus | Times | Divide | And | Or | Lt | Gt | Eq

  type subroutine_call = {
    target : Identifier.t option;
    subroutine_name : Identifier.t;
    arguments : Expression.t list;
  }

  type t =
    | IntegerConstant of int
    | StringConstant of string
    | KeywordConstant of keyword_constant
    | VarName of Identifier.t
    | ArrayAccess of { name : Identifier.t; index : Expression.t }
    | SubroutineCall of subroutine_call
    | UnaryOp of { op : unary_op; term : t }
    | Parenthesized of Expression.t
end =
  Term

and Expression : sig
  type t = { term : Term.t; op_terms : (Term.op * Term.t) list }
end =
  Expression

and Statement : sig
  type t =
    | Let of {
        var_name : Identifier.t;
        index : Expression.t option;
        value : Expression.t;
      }
    | If of {
        condition : Expression.t;
        then_statements : t list;
        else_statements : t list option;
      }
    | While of { condition : Expression.t; statements : t list }
    | Do of { call : Term.subroutine_call }
    | Return of { value : Expression.t option }
end =
  Statement

module Subroutine = struct
  type kind = Constructor | Function | Method
  type return_type = Some of Type.t | Void
  type var_dec = { typ : Type.t; names : Identifier.t list }
  type body = { vars : var_dec list; statements : Statement.t list }

  type t = {
    name : Identifier.t;
    kind : kind;
    return_type : return_type;
    parameters : (Type.t * Identifier.t) list;
    body : body;
  }
end

module ClassVar = struct
  type kind = Static | Field
  type t = { kind : kind; typ : Type.t; names : Identifier.t list }
end

module Class = struct
  type t = {
    name : Identifier.t;
    class_vars : ClassVar.t list;
    subroutines : Subroutine.t list;
  }
end
