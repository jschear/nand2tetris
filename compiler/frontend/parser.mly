%token CLASS
%token CONSTRUCTOR FUNCTION METHOD
%token FIELD
%token STATIC
%token VAR
%token INT CHAR BOOLEAN
%token VOID
%token TRUE FALSE
%token NULL
%token THIS
%token LET
%token DO
%token IF ELSE
%token WHILE
%token RETURN

%token LEFT_BRACE RIGHT_BRACE
%token LEFT_PAREN RIGHT_PAREN
%token LEFT_BRACKET RIGHT_BRACKET
%token DOT COMMA SEMICOLON
%token PLUS MINUS STAR SLASH AMPERSAND VERTICAL_BAR
%token LESS_THAN GREATER_THAN EQUAL
%token TILDE

%token <int> INTEGER_CONSTANT
%token <string> STRING_CONSTANT
%token <string> IDENTIFIER

%token EOF
%start <Ast.Class.t> classDecEof

%%

(* Program structure *)
classDecEof:
  | e = classDec; EOF { e }

classDec:
  | CLASS; name = className; LEFT_BRACE; class_vars = list(classVarDec); subroutines = list(subroutineDec); RIGHT_BRACE;
    { { name; class_vars; subroutines } : Ast.Class.t }

classVarDec:
  | kind = classVarDecKind; typ = typ; names = separated_nonempty_list(COMMA, varName); SEMICOLON;
    { { kind; typ; names } : Ast.ClassVar.t }

classVarDecKind:
  | STATIC; { Ast.ClassVar.Static }
  | FIELD { Ast.ClassVar.Field }

subroutineDec:
  | kind = subroutineKind; return_type = subroutineReturnType; name = subroutineName; LEFT_PAREN; parameters = parameterList; RIGHT_PAREN; body = subroutineBody;
    { { name; kind; return_type; parameters; body } : Ast.Subroutine.t }

subroutineKind:
  | CONSTRUCTOR { Ast.Subroutine.Constructor }
  | FUNCTION { Ast.Subroutine.Function }
  | METHOD { Ast.Subroutine.Method }

subroutineReturnType:
  | VOID { Ast.Subroutine.Void }
  | typ = typ { Ast.Subroutine.Some typ }

parameterList:
  | parameters = separated_list(COMMA, parameter) { parameters }

parameter:
  | typ = typ; name = varName
  { typ, name }

subroutineBody:
  | LEFT_BRACE; vars = list(varDec); statements = list(statement); RIGHT_BRACE;
    { { vars; statements } : Ast.Subroutine.body }

varDec:
  | VAR; typ = typ; names = separated_nonempty_list(COMMA, varName); SEMICOLON;
    { { typ; names} : Ast.Subroutine.var_dec }

typ:
  | INT { Ast.Type.Int }
  | CHAR { Ast.Type.Char }
  | BOOLEAN { Ast.Type.Boolean }
  | id = IDENTIFIER { Ast.Type.ClassName id }

className:
  | name = IDENTIFIER { name }

subroutineName:
  | name = IDENTIFIER { name }

varName:
  | name = IDENTIFIER { name }

(* Statements *)
statement:
  | LET; var_name = varName; EQUAL; value = expression; SEMICOLON;
    { Ast.Statement.Let { var_name; index = None; value; } }
  | LET; var_name = varName; LEFT_BRACKET; index = expression; RIGHT_BRACKET; EQUAL; value = expression; SEMICOLON;
    { Ast.Statement.Let { var_name; index = Some index; value } }
  | IF; LEFT_PAREN; condition = expression; RIGHT_PAREN; LEFT_BRACE; then_statements = list(statement); RIGHT_BRACE; else_statements = elseBranch?;
    { Ast.Statement.If { condition; then_statements; else_statements } }
  | WHILE; LEFT_PAREN; condition = expression; RIGHT_PAREN; LEFT_BRACE; statements = list(statement); RIGHT_BRACE;
    { Ast.Statement.While { condition; statements } }
  | DO; call = subroutineCall; SEMICOLON;
    { Ast.Statement.Do { call } }
  | RETURN; value = expression?; SEMICOLON;
    { Ast.Statement.Return { value } }

elseBranch:
  | ELSE; LEFT_BRACE; statements = list(statement); RIGHT_BRACE;
    { statements : Ast.Statement.t list }

(* Expressions *)
expression:
  | term = term; op_terms = list(opTerm);
    { { term; op_terms } }

opTerm:
  | op = op; term = term;
    { op, term }

term:
  | i = INTEGER_CONSTANT { Ast.Term.IntegerConstant i }
  | s = STRING_CONSTANT { Ast.Term.StringConstant s }
  | k = keywordConstant { Ast.Term.KeywordConstant k }
  | var_name = varName { Ast.Term.VarName var_name }
  | name = varName; LEFT_BRACKET; index = expression; RIGHT_BRACKET;
   { Ast.Term.ArrayAccess { name; index } }
  | call = subroutineCall;
    { Ast.Term.SubroutineCall call }
  | LEFT_PAREN; e = expression; RIGHT_PAREN;
    { Ast.Term.Parenthesized e }
  | op = unaryOp; term = term;
    { Ast.Term.UnaryOp { op; term } }

unaryOp:
  | MINUS { Ast.Term.Minus }
  | TILDE { Ast.Term.Not }

op:
  | PLUS { Ast.Term.Plus }
  | MINUS { Ast.Term.Minus }
  | STAR { Ast.Term.Times }
  | SLASH { Ast.Term.Divide }
  | AMPERSAND { Ast.Term.And }
  | VERTICAL_BAR { Ast.Term.Or }
  | LESS_THAN { Ast.Term.Lt }
  | GREATER_THAN { Ast.Term.Gt }
  | EQUAL { Ast.Term.Eq }

keywordConstant:
  | TRUE { Ast.Term.True }
  | FALSE { Ast.Term.False }
  | NULL { Ast.Term.Null }
  | THIS { Ast.Term.This }

subroutineCall:
  | subroutine_name = subroutineName; LEFT_PAREN; arguments = separated_list(COMMA, expression); RIGHT_PAREN;
    { { target = None; subroutine_name; arguments } : Ast.Term.subroutine_call }
  | target = varName; DOT; subroutine_name = subroutineName; LEFT_PAREN; arguments = separated_list(COMMA, expression); RIGHT_PAREN;
    { { target = Some target; subroutine_name; arguments } : Ast.Term.subroutine_call }

