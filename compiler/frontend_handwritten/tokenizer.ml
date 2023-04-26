open Core
open Tokens

(* This is for fun/learning. Use ocamllex! *)

type state = Begin | Char of char | EOF
type t = { in_channel : In_channel.t; mutable state : state }

let create in_channel : t = { in_channel; state = Begin }

let advance t =
  match In_channel.input_char t.in_channel with
  | None -> t.state <- EOF
  | Some c -> t.state <- Char c

let rec consume_until t c =
  advance t;
  match t.state with
  | EOF | Begin -> ()
  | Char c' -> if Char.(c = c') then () else consume_until t c

let rec buffer_until t buf ~pred =
  advance t;
  match t.state with
  | Begin | EOF -> ()
  | Char c' ->
      if pred c' then ()
      else (
        Buffer.add_char buf c';
        buffer_until t buf ~pred)

let consume_inline_comment t =
  consume_until t '\n';
  advance t

let consume_multiline_comment t =
  consume_until t '*';
  consume_until t '*';
  consume_until t '/';
  advance t

let consume_identifier t c : Token.t =
  let buf = Buffer.create 16 in
  Buffer.add_char buf c;
  buffer_until t buf ~pred:(fun c -> not (Char.is_alphanum c));
  let string = Buffer.contents buf in
  match Keyword.of_string string with
  | Some k -> Token.Keyword k
  | None -> Token.Identifier (Buffer.contents buf)

let consume_integer t c : Token.t =
  let buf = Buffer.create 16 in
  Buffer.add_char buf c;
  buffer_until t buf ~pred:(fun c -> not (Char.is_digit c));
  Token.IntegerConstant (Buffer.contents buf |> Int.of_string)

let consume_string_literal t : Token.t =
  let buf = Buffer.create 16 in
  buffer_until t buf ~pred:(fun c -> Char.(c = '"'));
  advance t;
  Token.StringConstant (Buffer.contents buf)

let consume_symbol c : Token.t =
  match Symbol.of_char c with
  | Some s -> Token.Symbol s
  | None -> failwith ("Unexpected character: " ^ Char.to_string c)

let rec next_token t : Token.t option =
  match t.state with
  | EOF -> None
  | Begin ->
      advance t;
      next_token t
  | Char c ->
      if Char.is_whitespace c then (
        advance t;
        next_token t)
      else if Char.(c = '/') then (
        advance t;
        match t.state with
        | Begin -> failwith "Unexpected"
        | EOF -> failwith "Unexpected end of file"
        | Char c' ->
            if Char.(c' = '/') then (
              consume_inline_comment t;
              next_token t)
            else if Char.(c' = '*') then (
              consume_multiline_comment t;
              next_token t)
            else Some (consume_symbol c))
      else if Char.is_alpha c then Some (consume_identifier t c)
      else if Char.is_digit c then Some (consume_integer t c)
      else if Char.(c = '"') then Some (consume_string_literal t)
      else (
        advance t;
        Some (consume_symbol c))
