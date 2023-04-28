(* A lexer specification for the Jack language. A lexer is generated from this file using ocamllex. *)

{
open Lexing
open Parser

exception SyntaxError of string

let keyword_table = Hashtbl.create 21
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
              [ "class", CLASS;
                "constructor", CONSTRUCTOR;
                "function", FUNCTION;
                "method", METHOD;
                "field", FIELD;
                "static", STATIC;
                "var", VAR;
                "int", INT;
                "char", CHAR;
                "boolean", BOOLEAN;
                "void", VOID;
                "true", TRUE;
                "false", FALSE;
                "null", NULL;
                "this", THIS;
                "let", LET;
                "do", DO;
                "if", IF;
                "else", ELSE;
                "while", WHILE;
                "return", RETURN ]

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let integerConstant = digit+
let identifier = (alpha) (alpha|digit|'_')*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read_token = parse
  | '{' { LEFT_BRACE }
  | '}' { RIGHT_BRACE }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | '[' { LEFT_BRACKET }
  | ']' { RIGHT_BRACKET }
  | '.' { DOT }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { SLASH }
  | '&' { AMPERSAND }
  | '|' { VERTICAL_BAR }
  | '<' { LESS_THAN }
  | '>' { GREATER_THAN }
  | '=' { EQUAL }
  | '~' { TILDE }
  | whitespace { read_token lexbuf }
  | "//" { single_line_comment lexbuf }
  | "/*" { multi_line_comment lexbuf }
  | integerConstant { INTEGER_CONSTANT (int_of_string (Lexing.lexeme lexbuf))}
  | identifier as id { try
                   Hashtbl.find keyword_table id
                 with Not_found ->
                   IDENTIFIER id }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | newline { new_line lexbuf; read_token lexbuf }
  | eof { EOF }
  | _ {raise (SyntaxError ("Illegal character: " ^ Lexing.lexeme lexbuf)) }

and single_line_comment = parse
  | newline { new_line lexbuf; read_token lexbuf }
  | eof { EOF }
  | _ { single_line_comment lexbuf }

and multi_line_comment = parse
  | "*/" { read_token lexbuf }
  | newline { new_line lexbuf; multi_line_comment lexbuf }
  | eof { raise (SyntaxError ("Unterminated comment.")) }
  | _ { multi_line_comment lexbuf }

and read_string buf = parse
  | '"' { STRING_CONSTANT (Buffer.contents buf) }
  | [^ '"' '\n']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("Unterminated String literal.")) }

