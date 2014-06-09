(* Utilities for the "Shiny computing machine" project. Name chosen randomly by GitHub. *)

(* let pi = 4.0 *. atan 1.0;; *)
let tau = 2.0 *. 4.0 *. atan 1.0;;

let deg_to_rad x = x *. tau /. 360.;;
let rad_to_deg x = x /. tau *. 360.;;

let next_pow2 x =
    let rec helper y acc =
        if y = 0 then acc else helper (y/2) (acc*2) in
    helper (x-1) 1;;
let clamp lo hi x = max lo (min x hi);;
let inplace fn xref = xref := (fn !xref);;

let (+=) x a = inplace ((+) a) x;;
let (+.=) x a = inplace ((+.) a) x;;

let identity x = x;;
let compose f g x = f (g x);;

let string_map f str =
    let arr = Array.create (String.length str) (f 'a') in
    let index = ref 0 in
    String.iter (fun ch ->
        Array.set arr !index (f ch);
        index += 1
        ) str;
    arr;;

let int3_of_rgb col =
let get_r c = Int32.to_int (Int32.shift_right (Int32.logand (Int32.of_int c) (Int32.of_int 0xFF0000)) 16) in
let get_g c = Int32.to_int (Int32.shift_right (Int32.logand (Int32.of_int c) (Int32.of_int 0x00FF00)) 8) in
let get_b c = Int32.to_int                    (Int32.logand (Int32.of_int c) (Int32.of_int 0x0000FF)) in
    (get_r col, get_g col, get_b col);;

module List =
struct
include List
let contains x = List.exists (fun elt -> elt = x)
let zip l1 l2 = List.rev (List.fold_left2 (fun acc e1 e2 -> (e1,e2) :: acc) [] l1 l2);;
let unzip l =
    let (rev_x, rev_y) =
        List.fold_left (fun (acc_x,acc_y) (elem_x,elem_y) ->
            (elem_x :: acc_x),(elem_y :: acc_y)
        ) ([],[]) l in
    (List.rev rev_x,List.rev rev_y)
end;;

module ExtendSet = functor (SetModule : Set.S) ->
struct
include SetModule
let with_setref fn = let sr = ref empty in fn sr; !sr
let of_list l = with_setref (fun sr -> List.iter (fun elem -> inplace (add elem) sr) l)
let map fn set = with_setref (fun sr -> iter (fun elem -> inplace (add (fn elem)) sr) set)

let map_multi (fn : elt -> elt list) set =
    with_setref (fun sr -> List.iter (fun lst -> List.iter (fun elem -> inplace (add elem) sr) lst) (List.map fn (elements set)))

(* apply fn to each elem of set, add the results into the set, until there are no new items to add *)
(* WARNING: not guarenteed to terminate. Among other things, attempting to find the closure of {0} via the function ((+) 1) will have a countably infinite runtime (sort of by definition).*)
let rec set_closure (fn : elt -> elt list) set =
    let elems_to_add = map_multi fn set in
    let new_set = union set elems_to_add in
    if (compare set new_set) = 0 then new_set else set_closure fn new_set
    
end;;
(*
#load "scm_util.cmo";;
open Scm_util;;
module SS = ExtendSet(Set.Make(String));;
let show = SS.elements;;
let a = SS.of_list ["a";"b";"c";"d"];;
let b = SS.map (fun e -> "prefix_" ^ e) a;;
let c = SS.map_multi (fun e -> [e^"0";e^"1";e^"2"]) b;;
*)
