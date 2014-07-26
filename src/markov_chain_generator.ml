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

let d = tokenize_characterwise c;;
p_tokens_given_model d b;;

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
    Random.self_init ();
    let tokens = tokenize_characterwise $ string_of_file filename in
    let occurrences = generate_markov_counts window tokens in
    generate_randomish_string occurrences length;;

let escape_whitespace s = Str.global_replace (Str.regexp "\n") "\\n" $ Str.global_replace (Str.regexp "\r") "\\r" (Str.global_replace (Str.regexp "\t") "\\t" s);;
let pad_to_length n s =
    let pad_len = n - String.length s in
    let padding = String.create pad_len in
    String.fill padding 0 pad_len ' ';
    padding ^ s

let p_tokens_given_model tokens ((window, markov_table) : occurrence_data) : float =
    (snd $ List.fold_left (fun (queue, prob) token ->
        let subtbl = Hashtbl.find_default (Hashtbl.create 0) markov_table queue#get in
        let total = float_of_int $ Hashtbl.fold (fun k v acc -> acc + v) subtbl 0 in
        let occs_of_token = float_of_int $ Hashtbl.find_default 0 subtbl token in (* consider 1 as the default, as in Laplace's rule of succession? *)
        (* average the probabilities instead of multiplying them, since multiplying moves them towards zero for an arbitrarily long string, even if most of them match *)
        let newprob = prob +. (occs_of_token /. total) in
        (* Printf.printf "token: %s\tcontext: %s\toccurrences: (%d / %d)\trunning_probability %f\n%!" (escape_whitespace token) (Array.string_of (compose (pad_to_length 2) escape_whitespace) queue#get) (int_of_float occs_of_token) (int_of_float total) newprob; *)
        queue#push token;
        (queue, newprob)
    ) (new finite_queue window "", 1.) tokens) /. (float_of_int $ List.length tokens)
