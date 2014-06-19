%{
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
%}
%token <int> NUM
%token <string> ID
%token LPAREN
%token RPAREN
%token NEWLINE
%start input
%type <content list> input
%type <content> sexp
%type <content> cont

%%
input:{[None]}
    |input line {$2};
line:sexp NEWLINE line {[$1]@$3 } 
    |NEWLINE{[None]};
sexp: LPAREN cont RPAREN {$2};
cont: ID cont { Id ($1,$2)}
    | NUM cont { Number ($1,$2)}
    | sexp cont {Sub ($1,$2)}
    |{ None};
%%
