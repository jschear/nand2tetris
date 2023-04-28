(* Modules for serializing the AST to XML in the format nand2tetris desires.

   TODO: Try out ppx_protocol_conv instead of handwriting these?
       - Add [@@deriving ...] to the AST types
       - Shadow the conversion functions to add the extra tokens nand2tetris wants
*)

open Ast
open Core

module type Xmlable = sig
  type t

  val write : t -> Out_channel.t -> unit
end

module XmlableIdentifier : Xmlable with type t := Identifier.t = struct
  let write t oc = fprintf oc "<identifier> %s </identifier>\n" t
end

module XmlableIdentifierList : Xmlable with type t := Identifier.t list = struct
  let write t oc =
    match t with
    | [] -> assert false
    | [ name ] -> XmlableIdentifier.write name oc
    | name :: names ->
        XmlableIdentifier.write name oc;
        List.iter names ~f:(fun name ->
            fprintf oc "<symbol> , </symbol>\n";
            XmlableIdentifier.write name oc)
end

module XmlableType : Xmlable with type t := Type.t = struct
  let write t oc =
    match t with
    | Type.Int -> fprintf oc "<keyword> int </keyword>\n"
    | Type.Char -> fprintf oc "<keyword> char </keyword>\n"
    | Type.Boolean -> fprintf oc "<keyword> boolean </keyword>\n"
    | Type.ClassName name -> XmlableIdentifier.write name oc
end

module XmlableClassVars : Xmlable with type t := ClassVar.t list = struct
  let write (t : ClassVar.t list) oc =
    List.iter t ~f:(fun class_var ->
        fprintf oc "<classVarDec>\n";
        let kind =
          match class_var.kind with
          | ClassVar.Field -> "field"
          | ClassVar.Static -> "static"
        in
        fprintf oc "<keyword> %s </keyword>\n" kind;
        XmlableType.write class_var.typ oc;
        XmlableIdentifierList.write class_var.names oc;
        fprintf oc "<symbol> ; </symbol>\n";
        fprintf oc "</classVarDec>\n")
end

module XmlableParameterList :
  Xmlable with type t := (Type.t * Identifier.t) list = struct
  let write (t : (Type.t * Identifier.t) list) oc =
    fprintf oc "<parameterList>\n";
    let rec write_parameters = function
      | [] -> ()
      | [ (typ, name) ] ->
          XmlableType.write typ oc;
          XmlableIdentifier.write name oc
      | (typ, name) :: rest ->
          XmlableType.write typ oc;
          XmlableIdentifier.write name oc;
          fprintf oc "<symbol> , </symbol>\n";
          write_parameters rest
    in
    write_parameters t;
    fprintf oc "</parameterList>\n"
end

module rec XmlableTerm : (Xmlable with type t := Term.t) = struct
  let rec write (t : Term.t) oc =
    fprintf oc "<term>\n";

    (match t with
    | Term.IntegerConstant i ->
        fprintf oc "<integerConstant> %d </integerConstant>\n" i
    | Term.StringConstant s ->
        fprintf oc "<stringConstant> %s </stringConstant>\n" s
    | Term.KeywordConstant k -> (
        match k with
        | Term.True -> fprintf oc "<keyword> true </keyword>\n"
        | Term.False -> fprintf oc "<keyword> false </keyword>\n"
        | Term.Null -> fprintf oc "<keyword> null </keyword>\n"
        | Term.This -> fprintf oc "<keyword> this </keyword>\n")
    | Term.VarName name -> XmlableIdentifier.write name oc
    | Term.SubroutineCall { target; subroutine_name; arguments } ->
        (match target with
        | None -> ()
        | Some target ->
            XmlableIdentifier.write target oc;
            fprintf oc "<symbol> . </symbol>\n");
        XmlableIdentifier.write subroutine_name oc;
        fprintf oc "<symbol> ( </symbol>\n";
        XmlableExpressionList.write arguments oc;
        fprintf oc "<symbol> ) </symbol>\n"
    | Term.Parenthesized expr ->
        fprintf oc "<symbol> ( </symbol>\n";
        XmlableExpression.write expr oc;
        fprintf oc "<symbol> ) </symbol>\n"
    | Term.UnaryOp { op; term } ->
        (match op with
        | Term.Minus -> fprintf oc "<symbol> - </symbol>\n"
        | Term.Not -> fprintf oc "<symbol> ~ </symbol>\n");
        write term oc
    | Term.ArrayAccess { name; index } ->
        XmlableIdentifier.write name oc;
        fprintf oc "<symbol> [ </symbol>\n";
        XmlableExpression.write index oc;
        fprintf oc "<symbol> ] </symbol>\n");

    fprintf oc "</term>\n"
end

and XmlableExpression : (Xmlable with type t := Expression.t) = struct
  let write (t : Expression.t) oc =
    fprintf oc "<expression>\n";
    XmlableTerm.write t.term oc;
    List.iter t.op_terms ~f:(fun (op, term) ->
        (match op with
        | Term.Plus -> fprintf oc "<symbol> + </symbol>\n"
        | Term.Minus -> fprintf oc "<symbol> - </symbol>\n"
        | Term.Times -> fprintf oc "<symbol> * </symbol>\n"
        | Term.Divide -> fprintf oc "<symbol> / </symbol>\n"
        | Term.And -> fprintf oc "<symbol> &amp; </symbol>\n"
        | Term.Or -> fprintf oc "<symbol> | </symbol>\n"
        | Term.Lt -> fprintf oc "<symbol> &lt; </symbol>\n"
        | Term.Gt -> fprintf oc "<symbol> &gt; </symbol>\n"
        | Term.Eq -> fprintf oc "<symbol> = </symbol>\n");
        XmlableTerm.write term oc);
    fprintf oc "</expression>\n"
end

and XmlableStatements : (Xmlable with type t := Statement.t list) = struct
  let write (t : Statement.t list) oc =
    fprintf oc "<statements>\n";
    List.iter t ~f:(fun statement -> XmlableStatement.write statement oc);
    fprintf oc "</statements>\n"
end

and XmlableSubroutineCall : (Xmlable with type t := Term.subroutine_call) =
struct
  let write (t : Term.subroutine_call) oc =
    (match t.target with
    | None -> ()
    | Some target ->
        XmlableIdentifier.write target oc;
        fprintf oc "<symbol> . </symbol>\n");
    XmlableIdentifier.write t.subroutine_name oc;
    fprintf oc "<symbol> ( </symbol>\n";
    XmlableExpressionList.write t.arguments oc;
    fprintf oc "<symbol> ) </symbol>\n"
end

and XmlableStatement : (Xmlable with type t := Statement.t) = struct
  let write (t : Statement.t) oc =
    match t with
    | Let let_statement ->
        fprintf oc "<letStatement>\n";
        fprintf oc "<keyword> let </keyword>\n";
        XmlableIdentifier.write let_statement.var_name oc;
        (match let_statement.index with
        | None -> ()
        | Some index ->
            fprintf oc "<symbol> [ </symbol>\n";
            XmlableExpression.write index oc;
            fprintf oc "<symbol> ] </symbol>\n");
        fprintf oc "<symbol> = </symbol>\n";
        XmlableExpression.write let_statement.value oc;
        fprintf oc "<symbol> ; </symbol>\n";
        fprintf oc "</letStatement>\n"
    | If if_statement ->
        fprintf oc "<ifStatement>\n";
        fprintf oc "<keyword> if </keyword>\n";
        fprintf oc "<symbol> ( </symbol>\n";
        XmlableExpression.write if_statement.condition oc;
        fprintf oc "<symbol> ) </symbol>\n";
        fprintf oc "<symbol> { </symbol>\n";
        XmlableStatements.write if_statement.then_statements oc;
        fprintf oc "<symbol> } </symbol>\n";
        (match if_statement.else_statements with
        | None -> ()
        | Some else_body ->
            fprintf oc "<keyword> else </keyword>\n";
            fprintf oc "<symbol> { </symbol>\n";
            XmlableStatements.write else_body oc;
            fprintf oc "<symbol> } </symbol>\n");
        fprintf oc "</ifStatement>\n"
    | While while_statement ->
        fprintf oc "<whileStatement>\n";
        fprintf oc "<keyword> while </keyword>\n";
        fprintf oc "<symbol> ( </symbol>\n";
        XmlableExpression.write while_statement.condition oc;
        fprintf oc "<symbol> ) </symbol>\n";
        fprintf oc "<symbol> { </symbol>\n";
        XmlableStatements.write while_statement.statements oc;
        fprintf oc "<symbol> } </symbol>\n";
        fprintf oc "</whileStatement>\n"
    | Do do_statement ->
        fprintf oc "<doStatement>\n";
        fprintf oc "<keyword> do </keyword>\n";
        XmlableSubroutineCall.write do_statement.call oc;
        fprintf oc "<symbol> ; </symbol>\n";
        fprintf oc "</doStatement>\n"
    | Return return_statement ->
        fprintf oc "<returnStatement>\n";
        fprintf oc "<keyword> return </keyword>\n";
        (match return_statement.value with
        | None -> ()
        | Some value -> XmlableExpression.write value oc);
        fprintf oc "<symbol> ; </symbol>\n";
        fprintf oc "</returnStatement>\n"
end

and XmlableExpressionList : (Xmlable with type t := Expression.t list) = struct
  let write (t : Expression.t list) oc =
    fprintf oc "<expressionList>\n";
    let rec write_expressions = function
      | [] -> ()
      | [ expr ] -> XmlableExpression.write expr oc
      | expr :: expressions ->
          XmlableExpression.write expr oc;
          fprintf oc "<symbol> , </symbol>\n";
          write_expressions expressions
    in
    write_expressions t;
    fprintf oc "</expressionList>\n"
end

module XmlableVarDec : Xmlable with type t := Subroutine.var_dec = struct
  let write (t : Subroutine.var_dec) oc =
    fprintf oc "<varDec>\n";
    fprintf oc "<keyword> var </keyword>\n";
    XmlableType.write t.typ oc;
    XmlableIdentifierList.write t.names oc;
    fprintf oc "<symbol> ; </symbol>\n";
    fprintf oc "</varDec>\n"
end

module XmlableSubroutineBody : Xmlable with type t := Subroutine.body = struct
  let write (t : Subroutine.body) oc =
    fprintf oc "<subroutineBody>\n";
    fprintf oc "<symbol> { </symbol>\n";
    List.iter t.vars ~f:(fun var ->
        fprintf oc "<varDec>\n";
        fprintf oc "<keyword> var </keyword>\n";
        XmlableType.write var.typ oc;
        XmlableIdentifierList.write var.names oc;
        fprintf oc "<symbol> ; </symbol>\n";
        fprintf oc "</varDec>\n");
    XmlableStatements.write t.statements oc;
    fprintf oc "<symbol> } </symbol>\n";
    fprintf oc "</subroutineBody>\n"
end

module XmlableSubroutines : Xmlable with type t := Subroutine.t list = struct
  let write (t : Subroutine.t list) oc =
    List.iter t ~f:(fun subroutine ->
        fprintf oc "<subroutineDec>\n";
        let kind =
          match subroutine.kind with
          | Subroutine.Constructor -> "constructor"
          | Subroutine.Function -> "function"
          | Subroutine.Method -> "method"
        in
        fprintf oc "<keyword> %s </keyword>\n" kind;
        let () =
          match subroutine.return_type with
          | Void -> fprintf oc "<keyword> void </keyword>\n"
          | Some typ -> XmlableType.write typ oc
        in
        XmlableIdentifier.write subroutine.name oc;
        fprintf oc "<symbol> ( </symbol>\n";
        XmlableParameterList.write subroutine.parameters oc;
        fprintf oc "<symbol> ) </symbol>\n";
        XmlableSubroutineBody.write subroutine.body oc;
        fprintf oc "</subroutineDec>\n")
end

module XmlableClass : Xmlable with type t := Class.t = struct
  let write (t : Class.t) oc =
    fprintf oc "<class>\n";
    fprintf oc "<keyword> class </keyword>\n";
    XmlableIdentifier.write t.name oc;
    fprintf oc "<symbol> { </symbol>\n";
    XmlableClassVars.write t.class_vars oc;
    XmlableSubroutines.write t.subroutines oc;
    fprintf oc "<symbol> } </symbol>\n";
    fprintf oc "</class>\n"
end
