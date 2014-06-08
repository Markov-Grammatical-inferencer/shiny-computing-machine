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

type 'a symbol = Terminal of 'a | Nonterminal of string;;
type 'a production = 'a symbol * 'a symbol list;; (* (lhs, rhs), not handling super-CFGs *)
type 'a lr_item = 'a symbol * 'a symbol list * 'a symbol list;; (* (lhs, rhs_before_dot, rhs_after_dot) *)
type 'a parse_action = Shift of int * 'a symbol | Reduce of 'a production | Accept;;
type 'a grammar = 'a production list;;

(* semantic type : cur_state * lookahead -> action_to_perform *)
type 'a ptable = (int * 'a symbol, 'a parse_action) Hashtbl.t
let get_ptable_entry table state lookahead = Hashtbl.find table (state,lookahead)

let balanced_paren_grammar = [(Nonterminal "S",[Terminal "(";Nonterminal "S";Terminal ")"]);(Nonterminal "S",[])];; (* Very verbose way of saying S -> "(" S ")" | "" *)

module ExtendSet = functor (SetModule : Set.S) ->
struct
include SetModule
let of_list l = let sr = ref empty in List.iter (fun elem -> inplace (add elem) sr) l; !sr
let map fn set =
    let sr = ref empty in
    iter (fun elem -> inplace (add (fn elem)) sr) set;
    !sr

let map_multi (fn : elt -> elt list) set =
    let sr = ref empty in
    List.iter (fun lst -> List.iter (fun elem -> inplace (add elem) sr) lst) (List.map fn (elements set));
    !sr

(* apply fn to each elem of set, add the results into the set, until there are no new items to add *)
(* WARNING: not guarenteed to terminate. Among other things, attempting to find the closure of {0} via the function ((+) 1) will have a countably infinite runtime (sort of by definition).*)
let rec set_closure (fn : elt -> elt list) set =
    let elems_to_add = map_multi fn set in
    let new_set = union set elems_to_add in
    if (compare set new_set) = 0 then new_set else set_closure fn new_set
    
end;;

let lritem_of_production (lhs,rhs) = (lhs,rhs,[]);;

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
let grammatical_closure : (elt grammar -> LRItemSet.t -> LRItemSet.t) = fun gram ->
    LRItemSet.set_closure (fun (lhs, rhs1, rhs2) ->
        match rhs2 with
        | Nonterminal(nt) :: _ -> List.map lritem_of_production (List.find_all (fun (prod_lhs,_) -> prod_lhs = Nonterminal(nt)) gram)
        | _ -> [(lhs, rhs1, rhs2)])
        
(* let make_table (production list -> elt Parse_table.t) = fun prods -> *)
    
end;;
