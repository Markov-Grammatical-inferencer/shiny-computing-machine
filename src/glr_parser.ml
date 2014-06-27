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
val length : t -> int
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

type 'a symbol = Start_symbol | Terminal of 'a | Nonterminal of string | End_of_input;;
type 'a production = 'a symbol * 'a symbol list;; (* (lhs, rhs), not handling super-CFGs *)
type 'a lr_item = 'a symbol * 'a symbol list * 'a symbol list;; (* (lhs, rhs_before_dot, rhs_after_dot) *)
type 'a grammar = 'a production list;;

(* semantic type : cur_state * lookahead -> action_to_perform *)
(* type 'a ptable = (int * 'a symbol, 'a parse_action) Hashtbl.t *)
(* let get_ptable_entry table state lookahead = Hashtbl.find table (state,lookahead) *)

let string_of_symbol string_of_type = function
    | Start_symbol -> "Start"
    | Terminal(t) -> "Terminal(" ^ (string_of_type t) ^ ")"
    | Nonterminal(nt) -> "Nonterminal(" ^ nt ^ ")"
    | End_of_input -> "$$";;

let balanced_paren_grammar = [(Nonterminal "S",[Terminal "(";Nonterminal "S";Terminal ")"]);(Nonterminal "S",[])];; (* Very verbose way of saying S -> "(" S ")" | "" *)

(* from Programming Language Pragmatics - third edition , page 88 *)
let simple_imperative_grammar =
[
Start_symbol, [Nonterminal "program"];
Nonterminal "program", [Nonterminal "stmt_list"; End_of_input];
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
(* module TokenSet = Set.Make(struct type t = elt let compare = TS.compare end) *)
module LRItemSet = ExtendSet(Set.Make(struct type t = elt lr_item let compare = compare end))
module LRItemSetSet = ExtendSet(Set.Make(LRItemSet))
type 'a parse_action = Shift of LRItemSet.t * 'a symbol | Reduce of 'a production | Accept | Reject;;
module ParseActionSet = ExtendSet(Set.Make(struct type t = elt parse_action let compare = compare end))
(* type parser_automaton_state = PA_State of (elt symbol, parser_automaton_state) Hashtbl.t *)

(* LRItemSets are used directly instead of integers representing an index (unlike both wikipedia and the textbook); this may need to be optimized later *)
(* note the similarity between curried functions and the shape of the transition_table type (right associative), this is deliberate:
   transition_table is basically a lookup function with 2 keys, and it's cleaner to manipulate when curried (as opposed to tupled) *)
type transition_table = (LRItemSet.t, (elt symbol, LRItemSet.t) Hashtbl.t) Hashtbl.t (* may need to extend return type here to SetSet for GLR? currently unsure *)
type parse_state = LRItemSet.t * elt symbol
type action_table = (parse_state, ParseActionSet.t) Hashtbl.t
type goto_table = (parse_state, LRItemSetSet.t) Hashtbl.t
type parse_stack_entry = PSE of parse_state * (parse_stack_entry option) (* state, parent_pointer (which is None for the initial state) *)

let get_state_number =
    let counter = ref (-1) in
    let tbl = Hashtbl.create 0 in
    (fun gram set ->
        try Hashtbl.find tbl (gram, set)
        with Not_found ->
            counter += 1;
            Hashtbl.add tbl (gram, set) !counter;
            !counter)

let string_of_symbol = string_of_symbol TS.string_of
let string_of_production (lhs, rhs) = Printf.sprintf "|%s -> %s|" (string_of_symbol lhs) (List.string_of string_of_symbol rhs)
let string_of_lritem (lhs, rhs1, rhs2) = Printf.sprintf "|%s -> %s <.> %s|" (string_of_symbol lhs) (List.string_of string_of_symbol rhs1) (List.string_of string_of_symbol rhs2)

let string_of_action act =
    let spf = Printf.sprintf in 
    let symstr = string_of_symbol in
    match act with
    | Shift(_, sym) -> spf "Shift %s " (symstr sym)
    | Reduce(prod) -> spf "Reduce by %s" (string_of_production prod)
    | Accept -> "Accept"
    | Reject -> "Reject"

let rec string_of_pse gram (PSE((set, sym), parent)) =
    let cur = Printf.sprintf "((%d, %s)" (get_state_number gram set) (string_of_symbol sym) in
    (match parent with
    | Some(p) -> Printf.sprintf "%s, %s)" cur (string_of_pse gram p)
    | None -> Printf.sprintf "%s)" cur)

let (string_of_action_table, string_of_goto_table) =
    let string_of_ag_table string_of_result gram tbl =
        let result = ref "State\tSymbol\tResult\n" in
        Hashtbl.iter (fun (set, sym) res ->
            result ^= Printf.sprintf "%d\t%s\t%s\n" (get_state_number gram set) (string_of_symbol sym) (string_of_result res)
        ) tbl; !result in
    (string_of_ag_table (ParseActionSet.string_of string_of_action), string_of_ag_table (LRItemSetSet.string_of (LRItemSet.string_of string_of_lritem)))

let lritems_starting_with gram sym = List.map lritem_of_production (List.find_all (fun (prod_lhs,_) -> prod_lhs = sym) gram)

let grammatical_closure : (elt grammar -> LRItemSet.t -> LRItemSet.t) = fun gram ->
    LRItemSet.set_closure (fun (lhs, rhs1, rhs2) -> match rhs2 with
        | Nonterminal(nt) :: _ -> lritems_starting_with gram (Nonterminal(nt))
        | _ -> [(lhs, rhs1, rhs2)])

let closure_of_list gram l = grammatical_closure gram (LRItemSet.of_list l)

(* let flatten_state s = List.map (fun (x,y) -> (x, LRItemSet.elements y)) (Hashtbl.list_of s) *)

let transitions_from_state gram set =
    let h : (elt symbol, LRItemSet.t) Hashtbl.t = Hashtbl.create 0 in
    let hget = Hashtbl.find_default LRItemSet.empty h in
    LRItemSet.iter (fun (lhs, rhs1, rhs2) -> match rhs2 with
        | rhs2hd :: rhs2tl -> 
            let process sym =
                let newval = (LRItemSet.union (hget sym) (LRItemSet.of_list [(lhs, rhs1@[rhs2hd], rhs2tl)])) in
                Hashtbl.replace h sym newval
            in
            (match rhs2hd with
            | Nonterminal(nt) as sym -> process sym
            | Terminal(t) as sym -> process sym
            | Start_symbol -> ()
            | End_of_input -> ())
        | [] -> ()
    ) set;
    Hashtbl.map (fun k v -> (k, (grammatical_closure gram v))) h

let get_initial_state gram = (closure_of_list gram (lritems_starting_with gram Start_symbol))

let make_transitions_table gram = 
    let transitions : transition_table = Hashtbl.create 0 in
    let dfs_stack = Stack.create () in
    Stack.push (get_initial_state gram) dfs_stack;
    while (not (Stack.is_empty dfs_stack)) do
        let cur_set = (Stack.pop dfs_stack) in
        if Hashtbl.contains_key cur_set transitions then () else (* guard clause to prevent cycles in search graph *)
        let transitions_from_cur = transitions_from_state gram cur_set in
        Hashtbl.add transitions cur_set transitions_from_cur;
        Hashtbl.iter (fun k v -> Stack.push v dfs_stack) transitions_from_cur
    done;
    transitions

let make_action_and_goto_tables gram trans_table =
    let atbl : action_table = Hashtbl.create 0 in
    let gtbl : goto_table = Hashtbl.create 0 in
    let aget = Hashtbl.find_default ParseActionSet.empty atbl in
    let gget = Hashtbl.find_default LRItemSetSet.empty gtbl in
    let aadd k v = Hashtbl.replace atbl k (ParseActionSet.add v (aget k)) in
    let gadd k v = Hashtbl.replace gtbl k (LRItemSetSet.add v (gget k)) in
    Hashtbl.iter (fun oldset sym_to_newset ->
        let reduces = LRItemSet.fold (fun elem acc ->
            match elem with
            | (lhs, rhs, []) -> (Reduce((lhs, rhs))) :: acc
            | _ -> acc
        ) oldset [] in
        Hashtbl.iter (fun sym newset ->
            (match sym with
            | Nonterminal(nt) -> gadd (oldset, sym) newset
            | Terminal(t) -> aadd (oldset, sym) (Shift(newset, sym))
            | Start_symbol -> ()
            | End_of_input -> ());
            List.iter (aadd (oldset, sym)) reduces
        ) sym_to_newset
    ) trans_table;
    (atbl, gtbl)

let get_token tokstream pos = if (pos < (TS.length tokstream)) then Terminal(TS.get tokstream pos) else End_of_input

let make_parser : (elt grammar -> (t -> elt tree list)) = fun gram ->
    let action_table, goto_table = make_action_and_goto_tables gram (make_transitions_table gram) in
    fun input ->
    let input_pos = ref 0 in
    let initial_state = PSE(((get_initial_state gram), (get_token input 0)), None) in
    let result_trees = ref [] in
    let parse_stacks = Queue.create () in
    Queue.add initial_state parse_stacks;
    let unravel_stack_to_tree stack = Node(TS.get input 0,[]) in (* replace this with actual linked-list traversal later *)
    while (not (Queue.is_empty parse_stacks)) do
        let current_pse = Queue.pop parse_stacks in
        let PSE((cur_state, _), parent) = current_pse in
        let cur_sym = get_token input !input_pos in
        input_pos += 1;
        Printf.printf "Current parse_stack_entry: %s\n%!" (string_of_pse gram current_pse);
        Printf.printf "Read token: %s\n%!" (string_of_symbol cur_sym);
        let actions = Hashtbl.find_default (ParseActionSet.of_list [Reject]) action_table (cur_state, cur_sym) in 
        Printf.printf "About to perform: [%!";
        ParseActionSet.iter (fun elem -> Printf.printf "%s; %!" (string_of_action elem)) actions;
        Printf.printf "]\n%!";
        ParseActionSet.iter (fun action -> match action with
            | Shift((state, sym)) -> Queue.push (PSE((state, sym), Some(current_pse))) parse_stacks
            | Reduce(prod) -> LRItemSetSet.iter (fun gotostate -> ()) (Hashtbl.find_default LRItemSetSet.empty goto_table (cur_state, cur_sym))
            | Accept -> List.push (unravel_stack_to_tree current_pse) result_trees
            | Reject -> ()
        ) actions
    done;
    !result_trees


(* This is for visualizing structures for debugging, *not* for processing. *)
let flatten_transitions_table (tbl : transition_table) =
    let (states, state_transitions) = List.unzip (Hashtbl.list_of tbl) in
    let (syms, destinations) = List.unzip (List.map List.unzip (List.map Hashtbl.list_of state_transitions)) in
    let (readable_states, readable_dests) = ((List.map LRItemSet.elements states), (List.map (List.map LRItemSet.elements) destinations)) in
    (readable_states, syms, readable_dests)

end;;

let main () =
    let module P = Make(StringArray) in
    let p = P.make_parser simple_operator_grammar in
    p [|"1";"+";"1"|];;
(*
#load "scm_util.cmo";;
#load "glr_parser.cmo";;
open Scm_util;;
open Glr_parser;;
module P = Make(StringArray);;
open P;;

let a = make_transitions_table simple_operator_grammar;;
let (atbl, gtbl) = make_action_and_goto_tables simple_operator_grammar a;;
let (aprint, gprint) = (string_of_action_table simple_operator_grammar atbl, string_of_goto_table simple_operator_grammar gtbl);;
Printf.printf "%s" aprint;;
(* Hashtbl.list_of (Hashtbl.map (fun (k1, k2) v -> ((LRItemSet.elements k1, k2), ParseActionSet.elements v)) atbl);; *)

let p = make_parser simple_operator_grammar;;
p [|"0";"+";"0"|];;
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
