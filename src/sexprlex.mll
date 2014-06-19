{
  open Sexprparse
}

let digit=['0'-'9']
let alpha=['a'-'z']
rule token=parse
	 | '\n' {NEWLINE}
	 |[' ' '\t'] {token lexbuf}
	 |digit+ as num {NUM (int_of_string num)}
	 |'(' {LPAREN}
	 |')' {RPAREN}
	 |alpha+ as id {ID id}
	 |eof {raise End_of_file}
