
#load "str.cma" 

type op = 
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Lt
  | Gt

type token = 
  | Var of string 
  | Num of int 
  | OpAssign
  | Op of op
  | Lbrace 
  | Rbrace 
  | Scolon 
  | Lparen 
  | Rparen 
  | Kwhile 
  | Kdo
  | Kif 
  | Kthen
  | Kelse
  | Ktrue 
  | Kfalse


type exp_a = 
  | Src of string 
  | Imm of int 
  | Arith of op * exp_a * exp_a 
  | Atom of exp_a 

type exp_b = 
  | True 
  | False 
  | LogicOp of op * exp_b * exp_b 
  | RelationOp of op * exp_a * exp_a 
  | Atom of exp_b 

type stmt = 
  | AssignStmt of string * exp_a 
  | IfStmt of exp_b * stmt list * stmt list 
  | WhileStmt of exp_b * stmt list 

type prog = 
  | Stmts of stmt list 

exception Syntax_error

let rec parse_exp_a token_stream = 
  let left_oprand = parse_term_a token_stream in 
    parse_exp_a_rest token_stream left_oprand 

and parse_exp_a_rest token_stream left_oprand = 
  match Stream.peek token_stream with 
    | Some (Op operator) when (operator = Add || operator = Sub) -> 
        Stream.junk token_stream; 
        let right_oprand = parse_term_a token_stream in 
        let accum = Arthm operator left_oprand right_oprand in 
          parse_expr_a_rest token_stream accum 
    | _ -> left_oprand 

and parse_term_a token_stream = 
  let left_oprand = parse_factor_a token_stream in 
    parse_term_a_rest token_stream left_oprand 

and parse_term_a_rest token_stream left_oprand = 
  match Stream.peek token_stream with 
    | Some (Op operator) when (operator = Mul || operator = Div) -> 
        Stream.junk token_stream; 
        let right_oprand = parse_factor_a token_stream in 
        let accum = Arthm operator left_oprand right_oprand in 
          parse_term_a_rest token_stream accum 
    | _ -> left_oprand 

and parse_factor_a token_stream = 
  match Stream.peek token_stream with 
    | Some (Num val) -> 
        Stream.junk token_stream; 
        Imm val
    | Some (Var name) -> 
        Stream.junk token_stream; 
        Src name 
    | Some Lparen -> 
        Stream.junk token_stream; 
        let atom_expr = parse_exp_a token_stream in 
          consume token_stream Rparen; 
          Atom atom_expr 

let rec parse_exp_b token_stream = 
  let left_oprand = parse_term_b token_stream in 
    parse_exp_b_rest token_stream left_oprand  

and parse_exp_b_rest token_stream left_oprand = 
  match Stream.peek token_stream with 
    | Some (Op Or) -> 
        Stream.junk token_stream; 
        let right_oprand parse_term_b token_stream in 
        let accum = LogicOp Or left_oprand right_oprand in 
          parse_exp_b_rest token_stream accum 
    | _ ->  left_oprand 

and parse_term_b token_stream = 
  let left_oprand = parse_factor_b token_stream in 
    parse_term_b_rest token_stream left_oprand  

and parse_term_b_rest token_stream left_oprand = 
  match Stream.peek token_stream with 
    | Some (Op And) -> 
        Stream.junk token_stream; 
        let right_oprand parse_factor_b token_stream in 
        let accum = LogicOp And left_oprand right_oprand in 
          parse_exp_b_rest token_stream accum 
    | _ ->  left_oprand 

and parse_factor_b token_stream = 
  match Stream.peek token_stream with 
    | Some Kture -> 
        Stream.junk token_stream; 
        True
    | Some Kfalse -> 
        Stream.junk token_stream; 
        False
    | Some Lparen -> 
        Stream.junk token_stream; 
        let atom_expr = parse_exp_b token_stream in 
          consume token_stream Rparen; 
          Atom atom_expr 
    | _ -> 
        let left_oprand = parse_exp_a token_stream in 
        let (Op rel_op) = Stream.next token_stream in 
        let right_oprand = parse_exp_a token_stream in 
          RelationOp rel_op left_oprand right_oprand 

let rec parse_stmt token_stream = 
  match Stream.peek token_stream with 
    | Some (Var var_name) -> 
        Stream.junk token_stream; 
        let rhs = parse_exp_a token_stream in 
          AssignStmt var_name rhs 
    | Some Kif -> 
        Stream.junk token_stream; 
        let pred = parse_exp_b token_stream in 
          consume token_stream Kthen; 
          consume token_stream Lbrace; 
          let if_stmts = parse_stmts token_stream in 
            consume token_stream Rbrace; 
            consume token_stream Kelse; 
            consume token_stream Lbrace; 
            let else_stmts = parse_stmts token_stream in 
              consume token_stream Rbrace; 
              IfStmt pred if_stmts else_stmts 
    | Some Kwhile -> 
        Stream.junk token_stream; 
        let pred = parse_exp_b token_stream in 
          consume token_stream Kdo; 
          consume token_stream Lbrace; 
          let stmts = parse_stmts token_stream in 
            consume token_stream Rbrace; 
            WhileStmt pred stmts 


let parse_stmts token_stream = 
  let stmts = (parse_stmt token_stream)::[] in 
  let rec loop res token_stream = 
    match Stream.peek token_stream with 
      | Some Scolon -> 
          Stream.junk token_stream 
          loop (parse_stmt token_stream)::res token_stream
      | _ -> List.rev res 
  in 
    loop stmts token_stream 

let parse_prog token_stream = 
  Stmts (parse_stmts token_stream) 

let token_of_string token_str = 
  match token_str with 
    | ":=" -> OpAssign
    | "+" -> Op Add 
    | "-" -> Op Sub 
    | "*" -> Op Mul
    | "/" -> Op Div
    | "and" -> Op And
    | "or" -> Op Or
    | "<" -> Op Lt
    | ">" -> Op Gt
    | "{" -> Lbrace 
    | "}" -> Rbrace 
    | "(" -> Lparen 
    | ")" -> Rparen 
    | "while" -> Kwhile 
    | "do" -> Kdo
    | "if" -> Kif
    | "then" -> Kthen 
    | "else" -> Kelse
    | "ture" -> Ktrue
    | "false" -> Kfalse
    | _ -> 
        match token_str.[0] with 
          | 'a'..'z' -> Var token_str 
          | '0'..'9' -> Num (int_of_string token_str) 

(* load program into a string *) 
let load_prog () = 
  let ic = stdin in 
  let n = in_channel_length ic in 
  let s = String.create n in 
    really_input ic s 0 n;
    s 

(* build a stream of tokens *) 
let build_token_stream prog_string = 
  let token_strs = Str.split (Str.regexp "[' ' '\n']+") prog_string in 
  let str_stream = Stream.of_list token_strs in 
    Stream.from 
      (fun _ -> 
         try 
           Some (token_of_string (stream.next str_stream)) 
         with Stream.Failure -> None) 

(* 
let dump_tokens token_stream = 
  try
    while true do 
      let token_str = Stream.next token_stream in 
        print_endline token_str
    done
  with Stream.Failure -> () 
*) 

let rec eval_stmt token_stream = 
  match Stream.peek token_stream with 
    | Some (Var var_name) -> 
        Stream.junk token_stream; 
        consume token_stream OpAssign; 
        let rhs = eval_a_exp token_stream in 

and eval_a_exp token_stream = 

and consume token_stream token = 
  match Stream.peek token_stream with 
    | Some t when t = token -> 
        Stream.junk token_stream
    | _ -> raise Syntax_error 

let main = 
  let content = load_prog () in 
  let token_stream = build_token_stream content in 
    dump_tokens token_stream

