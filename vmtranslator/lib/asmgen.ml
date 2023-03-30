open Asm.Expr
open Asm.Dest
open Asm.Value

(* Assembly code generation *)
let sp = Variable "SP"

(* RAM[SP] = D; SP++ *)
let push_from_d_register =
  [
    (* @SP *)
    A sp;
    (* A=M *)
    C (a, M, NULL);
    (* M=D *)
    C (m, D, NULL);
    (* @SP *)
    A sp;
    (* M=M+1 *)
    C (m, M_plus_one, NULL);
  ]

(* SP--; D = RAM[SP] *)
let pop_to_d_register =
  [
    (* @SP *)
    A sp;
    (* AM=M-1 *)
    C (am, M_minus_one, NULL);
    (* D=M *)
    C (d, M, NULL);
  ]

let push_constant value =
  [ (* @i *) A (Literal value); (* D=A *) C (d, A, NULL) ]
  @ push_from_d_register

(* addr ← LCL + i *)
let calculate_addr segment value =
  [
    (* @LCL etc. *)
    A (Variable (Vm.Segment.variable_for segment));
    (* D=M *)
    C (d, M, NULL);
    (* @i *)
    A (Literal value);
    (* A=D+A *)
    C (a, D_plus_A, NULL);
  ]

(* RAM[SP] ← RAM[addr]; SP++ *)
let push_from_addr = (* D=M *) C (d, M, NULL) :: push_from_d_register

(* SP--; RAM[addr] ← RAM[SP] *)
let pop_to_addr =
  let temp_register = "R13" in
  List.concat
    [
      (* Save the value of A in R13 *)
      [
        (* D=A *)
        C (d, A, NULL);
        (* @R13 *)
        A (Variable temp_register);
        (* M=D *)
        C (m, D, NULL);
      ];
      pop_to_d_register;
      (* Store the popped value in the addr pointed to by R13 *)
      [
        (* @R13 *)
        A (Variable temp_register);
        (* A=M *)
        C (a, M, NULL);
        (* M=D *)
        C (m, D, NULL);
      ];
    ]

let push_segment segment value = calculate_addr segment value @ push_from_addr
let pop_segment segment value = calculate_addr segment value @ pop_to_addr

let static_variable_for filename value =
  Asm.Value.Variable (Printf.sprintf "%s.%i" filename value)

let push_static classname value =
  (* @classname.i *)
  A (static_variable_for classname value) :: push_from_addr

let pop_static classname value =
  (* @classname.i *)
  A (static_variable_for classname value) :: pop_to_addr

let push_temp value =
  [
    (* @5 *)
    A (Literal 5);
    (* D=A *)
    C (d, A, NULL);
    (* @i *)
    A (Literal value);
    (* A=D+A *)
    C (a, D_plus_A, NULL);
  ]
  @ push_from_addr

let pop_temp value =
  [
    (* @5 *)
    A (Literal 5);
    (* D=A *)
    C (d, A, NULL);
    (* @i *)
    A (Literal value);
    (* A=D+A *)
    C (a, D_plus_A, NULL);
  ]
  @ pop_to_addr

let var_for_pointer_value value =
  let addr =
    match value with
    | 0 -> Vm.Segment.This
    | 1 -> Vm.Segment.That
    | _ -> failwith "Unexpected value in 'pointer x'"
  in
  Vm.Segment.variable_for addr

let push_pointer value =
  (* @THIS or @THAT *)
  A (Variable (var_for_pointer_value value)) :: push_from_addr

let pop_pointer value =
  (* @THIS or @THAT *)
  A (Variable (var_for_pointer_value value)) :: pop_to_addr

(* Arithmetic *)
let add =
  pop_to_d_register
  @ [ (* A=A-1 *) C (a, A_minus_one, NULL); (* M=D+M *) C (m, D_plus_M, NULL) ]

let sub =
  pop_to_d_register
  @ [ (* A=A-1 *) C (a, A_minus_one, NULL); (* M=M-D *) C (m, M_minus_D, NULL) ]

let neg = [ A sp; C (a, M_minus_one, NULL); C (m, Neg_M, NULL) ]
let not = [ A sp; C (a, M_minus_one, NULL); C (m, Not_M, NULL) ]

let and_op =
  pop_to_d_register
  @ [ (* A=A-1 *) C (a, A_minus_one, NULL); (* M=D&M *) C (m, D_and_M, NULL) ]

let or_op =
  pop_to_d_register
  @ [ (* A=A-1 *) C (a, A_minus_one, NULL); (* M=D|M *) C (m, D_or_M, NULL) ]

let boolean_op expr idx =
  let true_label = Printf.sprintf "TRUE_%i" idx in
  let end_label = Printf.sprintf "END_%i" idx in
  let jmp =
    match expr with
    | Vm.Expr.Eq -> Asm.Jmp.JEQ
    | Vm.Expr.Gt -> Asm.Jmp.JLT
    | Vm.Expr.Lt -> Asm.Jmp.JGT
    | _ -> failwith "invalid boolean op"
  in
  pop_to_d_register
  @ [
      (* A=A-1 *)
      C (a, A_minus_one, NULL);
      (* D=M-D *)
      C (d, D_minus_M, NULL);
      (* @TRUE_x *)
      A (Variable true_label);
      (* D;JEQ / JLT / JGT *)
      C (none, D, jmp);
      (* @SP *)
      A sp;
      (* A=M-1 *)
      C (a, M_minus_one, NULL);
      (* M=0 *)
      C (m, Zero, NULL);
      (* @END_x *)
      A (Variable end_label);
      (* 0;JMP *)
      C (none, Zero, JMP);
      (* (TRUE_x) *)
      Label true_label;
      (* @SP *)
      A sp;
      (* A=M-1 *)
      C (a, M_minus_one, NULL);
      (* M=-1 *)
      C (m, Neg_one, NULL);
      (* (END_x) *)
      Label end_label;
    ]

let label label = [ Label (Printf.sprintf "LABEL_%s" label) ]

let goto label =
  [ A (Variable (Printf.sprintf "LABEL_%s" label)); C (none, Zero, JMP) ]

let if_goto label =
  pop_to_d_register
  @ [ A (Variable (Printf.sprintf "LABEL_%s" label)); C (none, D, JNE) ]
