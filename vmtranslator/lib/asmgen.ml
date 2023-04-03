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

let function_label name = Printf.sprintf "FUNCTION_%s" name

let fn name num_locals =
  let label = function_label name in
  let rec zero_locals acc = function
    | 0 -> acc
    | n ->
        zero_locals
          (acc
          @ [
              (* @SP *)
              A sp;
              (* A=M+1 *)
              C (a, M_plus_one, NULL);
              (* M=0 *)
              C (m, Zero, NULL);
            ])
          (n - 1)
  in
  Label label :: zero_locals [] num_locals

let push_segment_pointer segment =
  [
    (* @segment *)
    A (Variable (Vm.Segment.variable_for segment));
    (* D=M *)
    C (d, M, NULL);
  ]
  @ push_from_d_register

let call name num_args classname idx =
  let return_label = Printf.sprintf "%s$ret.%i" classname idx in
  [ (* @returnAddress *) A (Variable return_label); (* D=A *) C (d, A, NULL) ]
  @ push_from_d_register
  @ push_segment_pointer Vm.Segment.Local
  @ push_segment_pointer Vm.Segment.Argument
  @ push_segment_pointer Vm.Segment.This
  @ push_segment_pointer Vm.Segment.That
  @ [
      (* ARG = SP - 5 - nArgs *)
      A sp;
      (* D=M *)
      C (d, M, NULL);
      (* @5 *)
      A (Literal 5);
      (* D=D-A *)
      C (d, D_minus_A, NULL);
      (* @nArgs *)
      A (Literal num_args);
      (* D=D-A *)
      C (d, D_minus_A, NULL);
      (* @ARG *)
      A (Variable (Vm.Segment.variable_for Vm.Segment.Argument));
      (* M=D *)
      C (m, D, NULL);
      (* @SP *)
      A sp;
      (* D=M *)
      C (d, M, NULL);
      (* @LCL *)
      A (Variable (Vm.Segment.variable_for Vm.Segment.Local));
      (* M=D *)
      C (m, D, NULL);
      (* @function *)
      A (Variable (function_label name));
      (* 0;JMP *)
      C (none, Zero, JMP);
      (* (returnAddress) *)
      Label return_label;
    ]

let restore_segment_pointer segment =
  [
    (* @R14 *)
    A (Variable "R14");
    (* AM=M-1 *)
    C (am, M_minus_one, NULL);
    (* D=M *)
    C (d, M, NULL);
    (* @segment *)
    A (Variable (Vm.Segment.variable_for segment));
    (* M=D *)
    C (m, D, NULL);
  ]

let return =
  pop_to_d_register
  @ [
      (* @ARG *)
      A (Variable (Vm.Segment.variable_for Vm.Segment.Argument));
      (* A=M *)
      C (a, M, NULL);
      (* M=D *)
      C (m, D, NULL);
      (* @ARG *)
      A (Variable (Vm.Segment.variable_for Vm.Segment.Argument));
      (* D=M *)
      C (d, M, NULL);
      (* @SP *)
      A sp;
      (* M=D+1 *)
      C (m, D_plus_one, NULL);
      (* @LCL *)
      A (Variable (Vm.Segment.variable_for Vm.Segment.Local));
      (* D=M *)
      C (d, M, NULL);
      (* @R14 *)
      A (Variable "R14");
      (* M=D *)
      C (m, D, NULL);
    ]
  @ restore_segment_pointer Vm.Segment.That
  @ restore_segment_pointer Vm.Segment.This
  @ restore_segment_pointer Vm.Segment.Argument
  @ restore_segment_pointer Vm.Segment.Local
  @ [
      (* @R14 *)
      A (Variable "R14");
      (* A=M-1 *)
      C (a, M_minus_one, NULL);
      (* A=M *)
      C (a, M, NULL);
      (* 0;JMP *)
      C (none, Zero, JMP);
    ]
