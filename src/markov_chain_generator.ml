open Scm_util;;
(*
#load "scm_util.cmo";;
#load "str.cma";;
#load "glr_parser.cmo";;
#load "markov_chain_generator.cmo";;
open Scm_util;;
open Markov_chain_generator;;
*)

let tokenize = Glr_parser.tokenize_via_whitespace;;

let tokens_of_file = compose tokenize string_of_file;;

(* a queue that only maintains that last n items, dropping the least recent if more are appended *)
class ['a] finite_queue length init =
object(self)
val state : 'a array = Array.create length init
method get = Array.copy state
method set arr = Array.iteri (Array.set state) arr
method push elt = Array.shift_left elt state;
end;;

(* window_length * (token array -> token -> occurrence count) *)
type occurrence_data = int * (string array, (string, int) Hashtbl.t) Hashtbl.t

let generate_markov_counts window tokens : occurrence_data =
    let q = new finite_queue window "" in
    let table = Hashtbl.create 0 in
    let subtable ctx =
        try Hashtbl.find table ctx with Not_found ->
        (let tmp = Hashtbl.create 0 in Hashtbl.add table ctx tmp; tmp) in
    let do_count tbl x = Hashtbl.inplace_key tbl x ((+) 1) 0 in
    List.iter (fun tok ->
        do_count (subtable q#get) tok;
        q#push tok;
    ) tokens;
    (window, table);;

let generate_randomish_string ((window, markov_table) : occurrence_data) length =
    let ctx = new finite_queue window "" in
    let random_word c =
        let subtbl = Hashtbl.find_default (Hashtbl.create 0) markov_table c in
        let total = Hashtbl.fold (fun k v acc -> acc + v) subtbl 0 in
        let idx = Random.int total in
        snd $ Hashtbl.fold (fun k v (i, x) -> if i < 0 then (i, x) else (i - v, k)) subtbl (idx, "")
    in
    let buf = Buffer.create 0 in
    for i=0 to length do
        let w = random_word ctx#get in
        Buffer.add_string buf w;
        Buffer.add_char buf ' ';
        ctx#push w
    done;
    Buffer.contents buf;;

let main filename window length =
    let tokens = tokenize $ string_of_file filename in
    let occurrences = generate_markov_counts window tokens in
    generate_randomish_string occurrences length;;
