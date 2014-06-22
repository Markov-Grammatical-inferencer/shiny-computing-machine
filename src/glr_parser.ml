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
val compare : elt -> elt -> int
val string_of : elt -> string
end;;

module StringArray =
struct
include Array
type t = string array
type elt = string
let get = Array.get
let compare = String.compare
let string_of = identity
end;;
let simple_tokenize = string_map identity;;

type 'a symbol = Start_symbol | Terminal of 'a | Nonterminal of string;;
type 'a production = 'a symbol * 'a symbol list;; (* (lhs, rhs), not handling super-CFGs *)
type 'a lr_item = 'a symbol * 'a symbol list * 'a symbol list;; (* (lhs, rhs_before_dot, rhs_after_dot) *)
type 'a parse_action = Shift of int * 'a symbol | Reduce of 'a production | Accept;;
type 'a grammar = 'a production list;;

(* semantic type : cur_state * lookahead -> action_to_perform *)
type 'a ptable = (int * 'a symbol, 'a parse_action) Hashtbl.t
let get_ptable_entry table state lookahead = Hashtbl.find table (state,lookahead)

let balanced_paren_grammar = [(Nonterminal "S",[Terminal "(";Nonterminal "S";Terminal ")"]);(Nonterminal "S",[])];; (* Very verbose way of saying S -> "(" S ")" | "" *)

(* from Programming Language Pragmatics - third edition , page 88 *)
let simple_imperative_grammar =
[
Start_symbol, [Nonterminal "program"];
Nonterminal "program", [Nonterminal "stmt_list"; Terminal "$$"];
Nonterminal "stmt_list", [Nonterminal "stmt_list"; Nonterminal "stmt"];
Nonterminal "stmt_list", [Nonterminal "stmt"];
Nonterminal "stmt", [Terminal "id"; Terminal ":="; Nonterminal "expr"];
Nonterminal "stmt", [Terminal "read"; Terminal "id"];
Nonterminal "stmt", [Terminal "write"; Nonterminal "expr"];
Nonterminal "expr", [Nonterminal "term"];
Nonterminal "expr", [Nonterminal "expr"; Nonterminal "add_op"; Nonterminal "term"];
Nonterminal "term", [Nonterminal "factor"];
Nonterminal "term", [Nonterminal "term"; Nonterminal "mult_op"; Nonterminal "factor"];
Nonterminal "factor", [Terminal "("; Nonterminal "expr"; Terminal ")"];
Nonterminal "factor", [Terminal "id"];
Nonterminal "factor", [Terminal "number"];
Nonterminal "add_op", [Terminal "+"];
Nonterminal "add_op", [Terminal "-"];
Nonterminal "mult_op", [Terminal "*"];
Nonterminal "mult_op", [Terminal "/"];
];;

(* http://en.wikipedia.org/wiki/LR_parser *)
let simple_operator_grammar =
[
Start_symbol, [Nonterminal "E"];
Nonterminal "E", [Nonterminal "E"; Terminal "*"; Nonterminal "B"];
Nonterminal "E", [Nonterminal "E"; Terminal "+"; Nonterminal "B"];
Nonterminal "E", [Nonterminal "B"];
Nonterminal "B", [Terminal "0"];
Nonterminal "B", [Terminal "1"];
];;

let lritem_of_production (lhs,rhs) = (lhs,[],rhs);;

(* example invokations:
module WordParser = Glr_parser.Make(StringArray);;
module CharParser = Glr_parser.Make(struct include String type elt = char end);;
*)
module Make = functor (TS : TokenStream) ->
struct
type t = TS.t
type elt = TS.elt
module TokenSet = Set.Make(struct type t = elt let compare = TS.compare end)
module LRItemSet = ExtendSet(Set.Make(struct type t = elt lr_item let compare = compare end))
(* type parser_automaton_state = PA_State of (elt symbol, parser_automaton_state) Hashtbl.t *)

let lritems_starting_with gram sym = List.map lritem_of_production (List.find_all (fun (prod_lhs,_) -> prod_lhs = sym) gram)

let grammatical_closure : (elt grammar -> LRItemSet.t -> LRItemSet.t) = fun gram ->
    LRItemSet.set_closure (fun (lhs, rhs1, rhs2) -> match rhs2 with
        | Nonterminal(nt) :: _ -> Printf.printf "nt: %s\n%!" nt; lritems_starting_with gram (Nonterminal(nt))
        | _ -> Printf.printf "other path\n%!"; [(lhs, rhs1, rhs2)])

let closure_of_list gram l = grammatical_closure gram (LRItemSet.of_list l)

let flatten_state s = List.map (fun (x,y) -> (x, LRItemSet.elements y)) (Hashtbl.list_of s)

let make_parser_state gram set =
    (* let automaton_state : parser_automaton_state = PA_State(Hashtbl.create 0) in *)
    let h : (elt symbol, LRItemSet.t) Hashtbl.t = Hashtbl.create 0 in
    let hget key = try Hashtbl.find h key with Not_found -> LRItemSet.empty in
    LRItemSet.iter (fun (lhs, rhs1, rhs2) -> match rhs2 with
        | rhs2hd :: rhs2tl -> 
            let process sym =
                let newval = (LRItemSet.union (hget sym) (LRItemSet.of_list [(lhs, rhs1@[rhs2hd], rhs2tl)])) in
                Hashtbl.remove h sym; (* ocaml's Hashtbl appears to retain keys, at least as observed by adding the same key twice and then folding over the table *)
                Hashtbl.add h sym newval
            in
            (match rhs2hd with
            | Nonterminal(nt) as sym -> process sym
            | Terminal(t) as sym -> process sym
            | Start_symbol -> ())
        | [] -> ()
    ) set;
    Hashtbl.map (fun k v -> (k, (grammatical_closure gram v))) h

let make_initial_parser_state gram = 
    let start_set = closure_of_list gram (lritems_starting_with gram Start_symbol) in
    make_parser_state gram start_set

(* let make_parser : (elt grammar -> (t -> elt tree)) = fun gram -> *)
end;;

(*
#load "scm_util.cmo";;
#load "glr_parser.cmo";;
open Scm_util;;
open Glr_parser;;
module P = Make(StringArray);;
open P;;

*)

(*
#load "scm_util.cmo";;
#load "glr_parser.cmo";;
open Scm_util;;
open Glr_parser;;
module P = Make(StringArray);;
module LRSet = P.LRItemSet;;
let f = P.grammatical_closure simple_imperative_grammar;;
let a = LRSet.of_list [lritem_of_production (List.hd simple_imperative_grammar)];;
let b = f a;;
LRSet.elements a;;
LRSet.elements b;;

*)
