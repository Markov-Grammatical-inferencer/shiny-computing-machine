(*
Machine Learning algorithm based on Hidden Markov Models
Compute the probability of the appearence of a word given N adjacent words?
*)

(* GLR Parser *)

(*
#load "lablglut.cma";;
#load "lablgl.cma";;
*)

open Scm_util;;

type 'a tree = Node of ('a * ('a tree list));;

(* Node("S",[Node("NP",[Node("NN",[Node("I",[])])]);Node("VP",[Node("VBZ",[Node("am",[])])])]);;*)

module type TokenStream =
sig
type t
type elt
val get : t -> int -> elt
(* may need to_string, unsure at the moment *)
end;;


type 'a symbol = Terminal of 'a | Nonterminal of string;;
type 'a production = 'a symbol * 'a symbol list;; (* (lhs, rhs), not handling super-CFGs *)
(*
(Nonterminal "VP",[Nonterminal "VBZ"] : 'a production);;
(Nonterminal "VBZ",[Terminal "wrote"] : 'a production);;
*)

let balanced_paren_grammer = [(Nonterminal "S",[Terminal "(";Nonterminal "S";Terminal ")"]);(Nonterminal "S",[])];; (* Very verbose way of saying S -> "(" S ")" | "" *)

module Parse_table =
struct
type t = (int * int, string list) Hashtbl.t
let get_entry : ((int * 'a, 'a symbol list) Hashtbl.t -> int -> 'a -> 'a symbol list) = fun table state lookahead -> Hashtbl.find table (state,lookahead)
end;;

(* example invokations:
module WordParser = Glr_parser.Make(struct include Array type t = string array type elt = string end);;
module CharParser = Glr_parser.Make(struct include String type elt = char end);;
*)
module Make = functor (TS : TokenStream) ->
struct
type t = TS.t
type elt = TS.elt
(* let make_table (production list -> production array array) = fun prods -> *)
    
end;;
