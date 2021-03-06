{
  let reservedWords = [
    (* Keywords in the alphaberical order *)
    ("else", Parser.ELSE);
    ("false", Parser.FALSE);
    ("fun", Parser.FUN);
    ("if", Parser.IF);
    ("in", Parser.IN);
    ("let", Parser.LET);
    ("then", Parser.THEN);
    ("true", Parser.TRUE);
    ("rec", Parser.REC);
  ]
}

rule main = parse
    (* ignore spacing and newline characters *)
    [' ' '\009' '\012' '\n']+    { main lexbuf }
  | "(*" { comment lexbuf; main lexbuf }
  | "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | "(" { Parser.LPAREN }
  | ")" { Parser.RPAREN }
  | ";;" { Parser.SEMISEMI }
  | "+" { Parser.PLUS }
  | "*" { Parser.MULT }
  | "<" { Parser.LT }
  | "&&" { Parser.AND }
  | "||" { Parser.OR }
  | "=" { Parser.EQ }
  | "->" { Parser.RARROW }
  | ['a'-'z'] ['a'-'z' '0'-'9' '_' ''']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
    }
  | eof { exit 0 }


and comment = parse
  | "(*" { comment lexbuf; comment lexbuf }
  | "*)" { () }
  | eof { () }
  | _ { comment lexbuf }
