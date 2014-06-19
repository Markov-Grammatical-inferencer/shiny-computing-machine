open Sexprparse;;
open Sexprlex;;
let rec content_to_string (co:content)=
  let i=ref co in 
  let j=ref ["("] in
  while not (!i = None) do
    (match !i with 
     | Number (x,y)->
	j:=(!j)@[(string_of_int x)];
	i:=y
     | Id (x,y)->
	j:=(!j)@[x];
	i:=y
     |Sub (x,y)->
       j:=(!j)@[(content_to_string x)];
       i:=y
     |None->());
  done;
  j:=!j@[")"];
  String.concat " " !j;;

let string_to_contents (s:string)=
  let i=ref [None] in 
  (
  try
    let lexbuf=Lexing.from_string s in 
    i:=Sexprparse.input Sexprlex.token lexbuf;
  with End_of_file->());
  !i;;
