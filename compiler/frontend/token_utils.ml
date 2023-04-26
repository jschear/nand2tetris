open Parser
open Core

let tag_of_token = function
  | CLASS | NULL | WHILE | VOID | VAR | BOOLEAN | TRUE | FALSE | THIS | STATIC
  | FIELD | FUNCTION | METHOD | LET | RETURN | CONSTRUCTOR | INT | CHAR | IF
  | ELSE | DO ->
      "keyword"
  | COMMA | DOT | AMPERSAND | VERTICAL_BAR | TILDE | STAR | SLASH | PLUS | MINUS
  | LESS_THAN | GREATER_THAN | SEMICOLON | RIGHT_BRACE | RIGHT_BRACKET
  | RIGHT_PAREN | LEFT_BRACE | LEFT_BRACKET | LEFT_PAREN | EQUAL ->
      "symbol"
  | STRING_CONSTANT _ -> "stringConstant"
  | INTEGER_CONSTANT _ -> "integerConstant"
  | IDENTIFIER _ -> "identifier"
  | EOF -> "eof"

let string_of_token = function
  | WHILE -> "while"
  | VOID -> "void"
  | VERTICAL_BAR -> "|"
  | VAR -> "var"
  | TRUE -> "true"
  | TILDE -> "~"
  | THIS -> "this"
  | STRING_CONSTANT value -> value
  | STATIC -> "static"
  | STAR -> "*"
  | SLASH -> "/"
  | SEMICOLON -> ";"
  | RIGHT_PAREN -> ")"
  | RIGHT_BRACKET -> "]"
  | RIGHT_BRACE -> "}"
  | RETURN -> "return"
  | PLUS -> "+"
  | NULL -> "null"
  | MINUS -> "-"
  | METHOD -> "method"
  | LET -> "let"
  | LESS_THAN -> "<"
  | LEFT_PAREN -> "("
  | LEFT_BRACKET -> "["
  | LEFT_BRACE -> "{"
  | INTEGER_CONSTANT value -> sprintf "%d" value
  | INT -> "int"
  | IF -> "if"
  | IDENTIFIER value -> value
  | GREATER_THAN -> ">"
  | FUNCTION -> "function"
  | FIELD -> "field"
  | FALSE -> "false"
  | EQUAL -> "="
  | EOF -> "eof"
  | ELSE -> "else"
  | DOT -> "."
  | DO -> "do"
  | CONSTRUCTOR -> "constructor"
  | COMMA -> ","
  | CLASS -> "class"
  | CHAR -> "char"
  | BOOLEAN -> "boolean"
  | AMPERSAND -> "&"

(* Wraps a lexer function, fn, writing each token to an output stream in XML format. *)
let wrap_lexer_xml fn oc : Lexing.lexbuf -> Parser.token =
  fprintf oc "<tokens>\n";

  let wrapped lexbuf =
    let token = fn lexbuf in
    let () =
      match token with
      | Parser.EOF -> fprintf oc "</tokens>\n"
      | _ ->
          let tag = tag_of_token token in
          fprintf oc "<%s> %s </%s>\n" tag (string_of_token token) tag
    in
    token
  in
  wrapped
