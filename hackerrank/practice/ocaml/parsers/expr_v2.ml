type token = 
  | Plus
  | Minus
  | Mul
  | Div
  | Lparen
  | Rparen
  | Num of int

exception Syntax_error

let m = 1000000007

let expr_str = input_line stdin

let rec lex_stream_of_string expr_str = 
  let chars_stream = Stream.of_string expr_str in 
    Stream.from (fun _ -> parse_lex chars_stream) 

and parse_lex chars_stream = 
  try 
    match (Stream.next chars_stream) with 
      | ' ' -> parse_lex chars_stream 
      | '0'..'9' as d -> 
          let buf = Buffer.create 10 in 
            Buffer.add_char buf d;
            Some (extract_num chars_stream buf) 
      | '(' -> Some Lparen
      | ')' -> Some Rparen 
      | '+' -> Some Plus
      | '-' -> Some Minus
      | '*' -> Some Mul
      | '/' -> Some Div
      | ch -> raise Syntax_error
      
  with Stream.Failure -> None 

and extract_num stream buf = 
  match Stream.peek stream with 
    | Some ('0'..'9' as d) -> 
        Stream.junk stream;
        Buffer.add_char buf d; 
        extract_num stream buf 
    | _ -> Num (buf |> Buffer.contents |> int_of_string) 

let dump_lex () = 
  let lex_stream = lex_stream_of_string expr_str in 
    try 
      while true do 
        let lex = Stream.next lex_stream in 
          match lex with 
            | Num n -> 
                print_int n; 
                print_endline "" 
            | Lparen -> 
                print_endline "(" 
            | Rparen -> 
                print_endline ")" 
            | Plus -> 
                print_endline "+" 
            | Minus -> 
                print_endline "-" 
            | Mul -> 
                print_endline "*" 
            | Div -> 
                print_endline "/" 
      done 
    with Stream.Failure -> 
      () 

let rec parse_expr token_stream = 
  let lhs = parse_term token_stream in 
    match Stream.peek token_stream with 
      | Some Plus -> 
          Stream.junk token_stream; 
          let rhs = parse_expr token_stream in 
            (lhs + rhs + m) mod m
      | Some Minus -> 
          Stream.junk token_stream; 
          let rhs = parse_expr token_stream in 
            (lhs - rhs + m) mod m 
      | _ -> lhs 

and ( ** ) x y =
  let rec loop prod a b =
    match b with
      | 0 -> 
          prod
      | _ when b land 1 <> 0 -> 
          loop (prod * a mod m) (a * a mod m) (b / 2) 
      | _ -> loop prod (a * a mod m) (b / 2)
  in
    loop 1 x y

and parse_term token_stream = 
  let lhs = parse_factor token_stream in 
    match Stream.peek token_stream with 
      | Some Mul -> 
          Stream.junk token_stream;
          let rhs = parse_term token_stream in
            (lhs * rhs) mod m 
      | Some Div -> 
          Stream.junk token_stream;
          let rhs = (parse_term token_stream) ** (m - 2) in
            ((lhs * rhs) mod m + m) mod m 
      | _ -> lhs 

and parse_factor token_stream = 
  match Stream.peek token_stream with 
    | Some (Num n) -> 
        Stream.junk token_stream;
        n mod m
    | Some Plus -> 
        Stream.junk token_stream; 
        parse_factor token_stream
    | Some Minus -> 
        Stream.junk token_stream; 
        ~-(parse_factor token_stream) 
    | Some Lparen -> 
        Stream.junk token_stream;
        let x = parse_expr token_stream in
          consume token_stream Rparen;
          x
    | _ -> raise Syntax_error 

and consume token_stream token = 
  match Stream.peek token_stream with 
    | Some t when t = token -> 
        Stream.junk token_stream
    | _ -> raise Syntax_error 


let main = 
  let lex_stream = lex_stream_of_string expr_str in 
  let res = parse_expr lex_stream in 
    print_endline (string_of_int res) 

