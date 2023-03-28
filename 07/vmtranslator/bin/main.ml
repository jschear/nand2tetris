open Core
open Vmtranslator

(* VM translator implementation *)
let trim_comment line =
  let comment_idx = String.substr_index line ~pattern:"//" in
  match comment_idx with
  | Some idx -> String.sub line ~pos:0 ~len:idx
  | None -> line

let construct_segment_value line ~constr =
  let parts = String.split line ~on:' ' in
  let segment = Vm.Segment.of_string (List.nth_exn parts 1) in
  let value = List.nth_exn parts 2 |> Int.of_string in
  constr segment value

let parse_line line =
  let first = String.split line ~on:' ' |> List.hd_exn in
  match first with
  | "push" ->
      construct_segment_value line ~constr:(fun x y -> Vm.Expr.Push (x, y))
  | "pop" ->
      construct_segment_value line ~constr:(fun x y -> Vm.Expr.Pop (x, y))
  | "add" -> Vm.Expr.Add
  | "sub" -> Vm.Expr.Sub
  | "neg" -> Vm.Expr.Neg
  | "eq" -> Vm.Expr.Eq
  | "gt" -> Vm.Expr.Gt
  | "lt" -> Vm.Expr.Lt
  | "and" -> Vm.Expr.And
  | "or" -> Vm.Expr.Or
  | "not" -> Vm.Expr.Not
  | _ -> raise (Failure "Not implemented")

let parse (bytecode : string list) : Vm.Expr.t list =
  List.filter_map bytecode ~f:(fun line ->
      let trimmed = trim_comment line |> String.strip in
      match trimmed with "" -> None | x -> Some (parse_line x))

let asm_of_bytecode idx filename expr =
  let comment = Asm.Expr.Comment (Vm.Expr.to_string expr) in
  let asm =
    match expr with
    | Vm.Expr.Push (Constant, value) -> Asmgen.push_constant value
    | Vm.Expr.Pop (Constant, _) -> failwith "Cannot pop to constant segment"
    | Vm.Expr.Push ((Local as segment), value)
    | Vm.Expr.Push ((Argument as segment), value)
    | Vm.Expr.Push ((This as segment), value)
    | Vm.Expr.Push ((That as segment), value) ->
        Asmgen.push_segment segment value
    | Vm.Expr.Pop ((Local as segment), value)
    | Vm.Expr.Pop ((Argument as segment), value)
    | Vm.Expr.Pop ((This as segment), value)
    | Vm.Expr.Pop ((That as segment), value) ->
        Asmgen.pop_segment segment value
    | Vm.Expr.Push (Static, value) -> Asmgen.push_static filename value
    | Vm.Expr.Pop (Static, value) -> Asmgen.pop_static filename value
    | Vm.Expr.Push (Pointer, value) -> Asmgen.push_pointer value
    | Vm.Expr.Pop (Pointer, value) -> Asmgen.pop_pointer value
    | Vm.Expr.Push (Temp, value) -> Asmgen.push_temp value
    | Vm.Expr.Pop (Temp, value) -> Asmgen.pop_temp value
    | Vm.Expr.Add -> Asmgen.add
    | Vm.Expr.Sub -> Asmgen.sub
    | Vm.Expr.Neg -> Asmgen.neg
    | Vm.Expr.Eq | Gt | Lt -> Asmgen.boolean_op expr idx
    | Vm.Expr.And -> Asmgen.and_op
    | Vm.Expr.Or -> Asmgen.or_op
    | Vm.Expr.Not -> Asmgen.not
  in
  comment :: asm

let translate filename bytecode : Asm.Expr.t list =
  List.concat_mapi bytecode ~f:(fun idx bytecode ->
      asm_of_bytecode idx filename bytecode)

let write filename program =
  List.map program ~f:Asm.Expr.to_string |> String.concat ~sep:"\n"
  |> fun data -> Out_channel.write_all filename ~data

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
