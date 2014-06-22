(* Utilities for the "Shiny computing machine" project. Name chosen randomly by GitHub. *)

(* let pi = 4.0 *. atan 1.0;; *)
let tau = 2.0 *. 4.0 *. atan 1.0;;

let deg_to_rad x = x *. tau /. 360.;;
let rad_to_deg x = x /. tau *. 360.;;
let apply_polar_movement radius theta (x,y) = ((x +. (radius *. (cos theta))),(y +. (radius *. (sin theta))));;

let ipow b e =
    let rec helper b e a =
        if e = 0 then a else helper b (e-1) (b*a) in
    helper b e 1;;
let next_pow2 x =
    let rec helper y acc =
        if y = 0 then acc else helper (y/2) (acc*2) in
    helper (x-1) 1;;

let clamp lo hi x = max lo (min x hi);;
let inplace fn xref = xref := (fn !xref);;
let inplace2 fn (xr1,xr2) = let (a,b) = fn (!xr1,!xr2) in xr1 := a; xr2 := b;;

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

module Array =
struct
include Array
let contains x = Array.fold_left (fun acc y -> (x = y) || acc) false
end;;

let curry f (x,y) = f x y;;

module List =
struct
include List
let rev2 l1 l2 = (rev l1, rev l2)
let contains x = List.exists (fun elt -> elt = x)
let zip l1 l2 = List.rev (List.fold_left2 (fun acc e1 e2 -> (e1,e2) :: acc) [] l1 l2);;
let unzip l =
    curry rev2 (List.fold_left (fun (acc_x,acc_y) (elem_x,elem_y) ->
            (elem_x :: acc_x),(elem_y :: acc_y)
        ) ([],[]) l)
end;;

module Hashtbl =
struct
include Hashtbl
let map fn tbl = fold (fun k v acc -> curry (add acc) (fn k v); acc) tbl (Hashtbl.create 0)

let list_of tbl = Hashtbl.fold (fun k v acc -> (k,v) :: acc) tbl []
end;;

module ExtendSet = functor (SetModule : Set.S) ->
struct
include SetModule
let of_list = List.fold_left (fun acc elem -> add elem acc) empty
let map fn set = fold (fun elem -> add (fn elem)) set empty

(* TODO: efficiency-test imperative vs functional versions of map_multi *)
let with_setref fn = let sr = ref empty in fn sr; !sr
let map_multi_imp (fn : elt -> elt list) set = with_setref (fun sr -> List.iter (fun lst -> List.iter (fun elem -> inplace (add elem) sr) lst) (List.map fn (elements set)))
let map_multi fn set = fold (fun elem -> union (of_list (fn elem))) set empty

(* apply fn to each elem of set, add the results into the set, until there are no new items to add *)
(* WARNING: not guarenteed to terminate. Among other things, attempting to find the closure of {0} via the function ((+) 1) will have a countably infinite runtime (sort of by definition).*)
let set_closure (fn : elt -> elt list) set =
    let rec helper set acc =
        let elems_to_add = map_multi fn set in
        let new_set = union elems_to_add acc in
        if (compare acc new_set) = 0 then new_set else helper elems_to_add new_set
    in helper set set
let set_closure_imp (fn : elt -> elt list) set =
    let rec helper set acc =
        let elems_to_add = map_multi_imp fn set in
        let new_set = union elems_to_add acc in
        if (compare acc new_set) = 0 then new_set else helper elems_to_add new_set
    in helper set set
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

(**
Contains the accumulator module and associated types.
 *)
module Accumulator=
struct

module type Compare=
  sig
    type t
    val compare: t * t -> bool (**Whatever values are given by this, it is correct to assume that it will require the first argument to be  more exactly what predicate you are testing.*)
  end;;
module type Accum= (** The signature of the accumulator class*)
  sig
    type elem
    type acc
    val accumulate:elem->unit
    val empty:elem->acc
  end;;

module Make (E1:Compare)=(** A module that implements accumulator for the type (E1)*)
  struct
    type elem=E1.t
    type acc=Non of elem|None
    let accumulate (a:elem) (ac:acc)=
      match ac with 
	Non (b)->
	if (E1.compare (a, b)) then
	  Non (b)
	else
	  Non (a)
      |None->
	Non (a)

    let empty ()=
      None
  end;;
      
					    
    
end;;

