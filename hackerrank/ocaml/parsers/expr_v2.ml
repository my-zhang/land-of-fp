
let lex_stream_of_channel channel = 
  let chars_stream = Stream.of_channel channel in 
    Stream.from (fun _ -> parse_lex chars_stream) 

and parse_lex chars_stream = 
  try 
    match (Stream.next chars_stream) with 
      | ' ' -> parse_lex chars_stream 
      | '0'..'9' -> 
          let buf = Buffer.create 10 in 
            parse_num (chars_stream, buf) 
      
  with e -> None 

and parse_num stream buf = 
  match Stream.peek stream with 
    | ('0'..'9' as d) -> 
        Stream.junk stream;
        Buffer.add_char buf d; 
        parse_num stream buf 
    | _ -> NUM (buf |> Buffer.contents |> int_of_string) 

let main = 
  let lex_stream = lex_stream_of_channel stdin in 
    try 
      while true do 
        let lex = Stream.next lex_stream in 
          print_endline lex
      done 
    with Stream.Failure -> 
      () 

