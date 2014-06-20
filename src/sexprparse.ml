type token =
  | NUM of (int)
  | ID of (string)
  | LPAREN
  | RPAREN
  | NEWLINE

open Parsing;;
let _ = parse_error;;
# 2 "sexprparse.mly"
  type content=Number of int*content
	      |Id of string*content
	      |Sub of content*content
	      |None;;
  let parse_error s=
    print_endline s;
    flush stdout;;

  let rec print_content (c:content)=
    match c with 
      Number (x,y)->
      print_endline (string_of_int x);
      print_content y
    |Id (x,y)->
      print_endline x;
      print_content y
    |Sub (x,y)->
      print_endline "entering sub_expression";
      print_content x;
      print_endline "Exiting sub_expression";
      print_content y
    |None->print_endline "Done";;
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
let getNth (c:content) (i:int)=
  let r=ref i in 
  let j=ref c in 
  while (!r)>0 do
   (match !j with 
      Number (a,b)->
      j:=b
    |Sub (a,b)->
      j:=b
    |Id (a,b)->
      j:=b
    |None->
      raise (Invalid_argument "Exceeded bounds of s-expression"));
   r:=!r-1;
  done;
  !j;;


# 70 "sexprparse.ml"
let yytransl_const = [|
  259 (* LPAREN *);
  260 (* RPAREN *);
  261 (* NEWLINE *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\004\000\004\000\002\000\003\000\003\000\003\000\
\003\000\000\000"

let yylen = "\002\000\
\000\000\002\000\003\000\001\000\003\000\002\000\002\000\002\000\
\000\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\004\000\000\000\002\000\000\000\
\000\000\000\000\000\000\000\000\007\000\006\000\008\000\005\000\
\003\000"

let yydgoto = "\002\000\
\003\000\010\000\011\000\007\000"

let yysindex = "\006\000\
\000\000\000\000\005\255\000\255\000\000\006\255\000\000\000\255\
\000\255\000\255\008\255\005\255\000\000\000\000\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\013\000\010\255\000\000\000\000\000\000\010\255\
\010\255\010\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\253\255\252\255\003\000"

let yytablesize = 15
let yytable = "\006\000\
\008\000\009\000\004\000\013\000\014\000\015\000\001\000\004\000\
\006\000\005\000\012\000\016\000\010\000\009\000\017\000"

let yycheck = "\003\000\
\001\001\002\001\003\001\008\000\009\000\010\000\001\000\003\001\
\012\000\005\001\005\001\004\001\000\000\004\001\012\000"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  NEWLINE\000\
  "

let yynames_block = "\
  NUM\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "sexprparse.mly"
      ([None])
# 137 "sexprparse.ml"
               : content list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : content list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'line) in
    Obj.repr(
# 73 "sexprparse.mly"
                 (_2)
# 145 "sexprparse.ml"
               : content list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : content) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'line) in
    Obj.repr(
# 74 "sexprparse.mly"
                       ([_1]@_3 )
# 153 "sexprparse.ml"
               : 'line))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "sexprparse.mly"
            ([None])
# 159 "sexprparse.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : content) in
    Obj.repr(
# 76 "sexprparse.mly"
                         (_2)
# 166 "sexprparse.ml"
               : content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : content) in
    Obj.repr(
# 77 "sexprparse.mly"
              ( Id (_1,_2))
# 174 "sexprparse.ml"
               : content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : content) in
    Obj.repr(
# 78 "sexprparse.mly"
               ( Number (_1,_2))
# 182 "sexprparse.ml"
               : content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : content) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : content) in
    Obj.repr(
# 79 "sexprparse.mly"
                (Sub (_1,_2))
# 190 "sexprparse.ml"
               : content))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "sexprparse.mly"
     ( None)
# 196 "sexprparse.ml"
               : content))
(* Entry input *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let input (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : content list)
;;
