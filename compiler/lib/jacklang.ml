module Keyword = struct
  type t =
    | Class
    | Constructor
    | Function
    | Method
    | Field
    | Static
    | Var
    | Int
    | Char
    | Boolean
    | Void
    | True
    | False
    | Null
    | This
    | Let
    | Do
    | If
    | Else
    | While
    | Return

  let of_string = function
    | "class" -> Some Class
    | "constructor" -> Some Constructor
    | "function" -> Some Function
    | "method" -> Some Method
    | "field" -> Some Field
    | "static" -> Some Static
    | "var" -> Some Var
    | "int" -> Some Int
    | "char" -> Some Char
    | "boolean" -> Some Boolean
    | "void" -> Some Void
    | "true" -> Some True
    | "false" -> Some False
    | "null" -> Some Null
    | "this" -> Some This
    | "let" -> Some Let
    | "do" -> Some Do
    | "if" -> Some If
    | "else" -> Some Else
    | "while" -> Some While
    | "return" -> Some Return
    | _ -> None

  let to_string = function
    | Class -> "class"
    | Constructor -> "constructor"
    | Function -> "function"
    | Method -> "method"
    | Field -> "field"
    | Static -> "static"
    | Var -> "var"
    | Int -> "int"
    | Char -> "char"
    | Boolean -> "boolean"
    | Void -> "void"
    | True -> "true"
    | False -> "false"
    | Null -> "null"
    | This -> "this"
    | Let -> "let"
    | Do -> "do"
    | If -> "if"
    | Else -> "else"
    | While -> "while"
    | Return -> "return"
end

module Symbol = struct
  type t =
    | LeftBrace
    | RightBrace
    | LeftParen
    | RightParen
    | LeftBracket
    | RightBracket
    | Dot
    | Comma
    | Semicolon
    | Plus
    | Minus
    | Asterisk
    | Slash
    | Ampersand
    | VerticalBar
    | LessThan
    | GreaterThan
    | Equal
    | Tilde

  let of_char = function
    | '{' -> Some LeftBrace
    | '}' -> Some RightBrace
    | '(' -> Some LeftParen
    | ')' -> Some RightParen
    | '[' -> Some LeftBracket
    | ']' -> Some RightBracket
    | '.' -> Some Dot
    | ',' -> Some Comma
    | ';' -> Some Semicolon
    | '+' -> Some Plus
    | '-' -> Some Minus
    | '*' -> Some Asterisk
    | '/' -> Some Slash
    | '&' -> Some Ampersand
    | '|' -> Some VerticalBar
    | '<' -> Some LessThan
    | '>' -> Some GreaterThan
    | '=' -> Some Equal
    | '~' -> Some Tilde
    | _ -> None

  let is_symbol char = match of_char char with Some _ -> true | None -> false

  let to_string = function
    | LeftBrace -> "{"
    | RightBrace -> "}"
    | LeftParen -> "("
    | RightParen -> ")"
    | LeftBracket -> "["
    | RightBracket -> "]"
    | Dot -> "."
    | Comma -> ","
    | Semicolon -> ";"
    | Plus -> "+"
    | Minus -> "-"
    | Asterisk -> "*"
    | Slash -> "/"
    | Ampersand -> "&"
    | VerticalBar -> "|"
    | LessThan -> "<"
    | GreaterThan -> ">"
    | Equal -> "="
    | Tilde -> "~"
end

module Token = struct
  type t =
    | Keyword of Keyword.t
    | Symbol of Symbol.t
    | IntegerConstant of int
    | StringConstant of string
    | Identifier of string

  let to_type_tag = function
    | Keyword _ -> "keyword"
    | Symbol _ -> "symbol"
    | IntegerConstant _ -> "integerConstant"
    | StringConstant _ -> "stringConstant"
    | Identifier _ -> "identifier"

  let to_string = function
    | Keyword keyword -> Keyword.to_string keyword
    | Symbol symbol -> Symbol.to_string symbol
    | IntegerConstant int -> string_of_int int
    | StringConstant str -> str
    | Identifier id -> id
end
