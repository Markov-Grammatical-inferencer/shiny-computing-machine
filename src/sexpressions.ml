open Sexprparse;;
open Sexprlex;;
let string_to_contents (s:string)=
  let i=ref [None] in 
  (
  try
    let lexbuf=Lexing.from_string s in 
    i:=Sexprparse.input Sexprlex.token lexbuf;
  with End_of_file->());
  !i;;
