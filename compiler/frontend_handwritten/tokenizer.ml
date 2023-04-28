open Core
open Tokens

(* This is for fun/learning. Use ocamllex! *)

type state = Begin | Char of char | EOF
type t = { in_channel : In_channel.t; state : state }

let create in_channel : t = { in_channel; state = Begin }

let advance t =
  match In_channel.input_char t.in_channel with
  | None -> { t with state = EOF }
  | Some c -> { t with state = Char c }

let rec advance_until c t =
  let t = advance t in
  match t.state with
  | EOF | Begin -> t
  | Char c' -> if Char.(c = c') then t else advance_until c t

let rec buffer_until buf ~pred t =
  let t = advance t in
  match t.state with
  | Begin | EOF -> t
  | Char c' ->
      if pred c' then t
      else (
        Buffer.add_char buf c';
        buffer_until buf ~pred t)

let consume_inline_comment t = advance_until '\n' t |> advance

let consume_multiline_comment t =
  t |> advance_until '*' |> advance_until '*' |> advance_until '/' |> advance

let consume_identifier c t : Token.t option * t =
  let buf = Buffer.create 16 in
  Buffer.add_char buf c;
  let t = buffer_until buf ~pred:(fun c -> not (Char.is_alphanum c)) t in
  let string = Buffer.contents buf in
  match Keyword.of_string string with
  | Some k -> (Some (Token.Keyword k), t)
  | None -> (Some (Token.Identifier (Buffer.contents buf)), t)

let consume_integer c t : Token.t option * t =
  let buf = Buffer.create 16 in
  Buffer.add_char buf c;
  let t = buffer_until buf ~pred:(fun c -> not (Char.is_digit c)) t in
  (Some (Token.IntegerConstant (Buffer.contents buf |> Int.of_string)), t)

let consume_string_literal t : Token.t option * t =
  let buf = Buffer.create 16 in
  let t = t |> buffer_until buf ~pred:(fun c -> Char.(c = '"')) |> advance in
  (Some (Token.StringConstant (Buffer.contents buf)), t)

let consume_symbol c t : Token.t option * t =
  match Symbol.of_char c with
  | Some s -> (Some (Token.Symbol s), t)
  | None -> failwith ("Unexpected character: " ^ Char.to_string c)

let rec next_token t : Token.t option * t =
  match t.state with
  | EOF -> (None, t)
  | Begin -> t |> advance |> next_token
  | Char c ->
      if Char.is_whitespace c then t |> advance |> next_token
      else if Char.(c = '/') then
        let t = advance t in
        match t.state with
        | Begin -> failwith "Unexpected"
        | EOF -> failwith "Unexpected end of file"
        | Char c' ->
            if Char.(c' = '/') then t |> consume_inline_comment |> next_token
            else if Char.(c' = '*') then
              t |> consume_multiline_comment |> next_token
            else consume_symbol c t
      else if Char.is_alpha c then consume_identifier c t
      else if Char.is_digit c then consume_integer c t
      else if Char.(c = '"') then consume_string_literal t
      else t |> advance |> consume_symbol c
