open Scm_util;;
(*
#load "scm_util.cmo";;
#load "str.cma";;
#load "glr_parser.cmo";;
#load "markov_chain_generator.cmo";;
open Scm_util;;
open Markov_chain_generator;;

let a = tokenize_characterwise $ string_of_file "pg1661_the_adventures_of_sherlock_holmes.txt";;
let b = generate_markov_counts 5 a;;
let c = generate_randomish_string b 1000;;
Printf.printf "%s\n" c;;
*)

let string_of_char c = let b = Buffer.create 0 in Buffer.add_char b c; Buffer.contents b;;
let tokenize_characterwise s = Array.to_list $ string_map string_of_char s;;
let tokenize s = List.rev (List.rev_map (fun s -> s^" ") $ Glr_parser.tokenize_via_whitespace s);;

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
        (* index into an "unrolled roulette wheel", subtracting the bucket widths until i is negative *)
        snd $ Hashtbl.fold (fun k v (i, x) -> if i < 0 then (i, x) else (i - v, k)) subtbl (idx, "")
    in
    let buf = Buffer.create 0 in
    for i=0 to length do
        let w = random_word ctx#get in
        Buffer.add_string buf w;
        (* Buffer.add_char buf ' '; *)
        ctx#push w
    done;
    Buffer.contents buf;;

let main filename window length =
    let tokens = tokenize_characterwise $ string_of_file filename in
    let occurrences = generate_markov_counts window tokens in
    generate_randomish_string occurrences length;;
