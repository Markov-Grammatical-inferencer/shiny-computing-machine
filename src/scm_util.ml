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

let swapargs fn x y = fn y x;;
let clamp lo hi x = max lo (min x hi);;
let inplace fn xref = xref := (fn !xref);;
let inplace2 fn (xr1,xr2) = let (a,b) = fn (!xr1,!xr2) in xr1 := a; xr2 := b;;

let (+=) x a = inplace ((+) a) x;;
let (+.=) x a = inplace ((+.) a) x;;
let (^=) x a = inplace ((swapargs (^)) a) x;;
let (@=) x a = inplace ((swapargs (@)) a) x;;

let identity x = x;;
let compose f g x = f (g x);;
let ($) a b = a b;;
let ($.) = compose;;
let (!!) a = a ();;
let uncurry f (x,y) = f x y;;

let string_map f str =
    let arr = Array.create (String.length str) (f 'a') in
    let index = ref 0 in
    String.iter (fun ch ->
        Array.set arr !index (f ch);
        index += 1
        ) str;
    arr;;

let string_of_array arr =
    let rv = String.create $ Array.length arr in
    Array.iteri (String.unsafe_set rv) arr;
    rv;;

let string_of_file fname =
    let file = open_in fname in
    let len = in_channel_length file - 1 in
    let buf = Buffer.create 0 in
    for i = 0 to len do Buffer.add_char buf $ input_char file done;
    Buffer.contents buf;;

let int3_of_rgb col =
let get_r c = Int32.to_int (Int32.shift_right (Int32.logand (Int32.of_int c) (Int32.of_int 0xFF0000)) 16) in
let get_g c = Int32.to_int (Int32.shift_right (Int32.logand (Int32.of_int c) (Int32.of_int 0x00FF00)) 8) in
let get_b c = Int32.to_int                    (Int32.logand (Int32.of_int c) (Int32.of_int 0x0000FF)) in
    (get_r col, get_g col, get_b col);;

module Array =
struct
include Array
let contains x = Array.fold_left (fun acc y -> (x = y) || acc) false
let shift_left new_rightmost arr =
    let last = (length arr)-1 in
    iteri (fun idx elt ->
        let next_val = if idx = last then new_rightmost else get arr (idx+1) in
        set arr idx next_val
    ) arr;;
let string_of string_of_t l = Printf.sprintf "%s|]" (fold_left (fun acc elem -> acc ^ elem ^ ";") "[|" (map string_of_t l))
end;;

(* type ('a, 'b) either = T1 of 'a | T2 of 'b;; *)
(* let either_adapt fn1 fn2 x = match x with T1 (y) -> fn1 y | T2 (z) -> fn2 z;; *)
(* let apply_tuple2 fn1 fn2 (x,y) = (fn1 x, fn2 y);; *)

module List =
struct
include List
let rev2 l1 l2 = (rev l1, rev l2)
let contains x = List.exists (fun elt -> elt = x)
let zip l1 l2 = List.rev (List.fold_left2 (fun acc e1 e2 -> (e1,e2) :: acc) [] l1 l2);;
let unzip l =
    uncurry rev2 (List.fold_left (fun (acc_x,acc_y) (elem_x,elem_y) ->
            (elem_x :: acc_x),(elem_y :: acc_y)
        ) ([],[]) l)
let car = hd
let cdr = tl
let cons x y = x :: y
let push x = inplace (cons x)
let push_opt x lr = match x with Some(y) -> inplace (cons y) lr | None -> ()
let pop lr = let x = car !lr in inplace cdr lr; x
let string_of string_of_t l = Printf.sprintf "%s]" (List.fold_left (fun acc elem -> acc ^ elem ^ ";") "[" (List.map string_of_t l))
let of_option = function | Some(x) -> [x] | None -> []
let mapcan f l = List.fold_left List.append [] (List.map f l)
let rec pad_to_same_length pad1 pad2 l1 l2 =
    if      List.length l1 < List.length l2 then pad_to_same_length pad1 pad2 (l1 @ [pad1]) l2
    else if List.length l2 < List.length l1 then pad_to_same_length pad1 pad2 l1 (l2 @ [pad2])
    else (l1, l2)
let remove_first pred l = List.rev (snd (List.fold_left (fun (found, acc) elem -> if found then (found, elem::acc) else if pred elem then (true, acc) else (found, elem::acc)) (false, []) l))
let extract pred lr =
    let result = find pred !lr in
    inplace (remove_first pred) lr;
    result
let first_n n l = rev (snd $ fold_left (fun (i,a) e -> if i < n then (i+1, e::a) else (i, a)) (0,[]) l);;
end;;

module Hashtbl =
struct
include Hashtbl
let map fn tbl = fold (fun k v acc -> uncurry (add acc) (fn k v); acc) tbl (Hashtbl.create 0)
let exists fn tbl = fold (fun k v acc -> if (fn k v) then true else acc) tbl false
let contains_key key = exists (fun k v -> k = key)
let list_of tbl = Hashtbl.fold (fun k v acc -> (k,v) :: acc) tbl []
let of_list l = List.fold_left (fun tbl (k, v) -> Hashtbl.add tbl k v; tbl) (Hashtbl.create 0) l;;
let find_default default tbl k = try find tbl k with Not_found -> default
let uncurry_find tbl k1 k2 = find (find tbl k1) k2
(* makes a new table with all the keys of t1 and t2, with potential
 default values if the corresponding keys don't exist *)
let zip t1 t2 dv1 dv2 =
    let newtbl = create 0 in
    iter (fun k v -> replace newtbl k (v, find_default dv2 t2 k)) t1;
    iter (fun k v -> replace newtbl k (find_default dv1 t1 k, v)) t2;
    newtbl
let inplace_key tbl k fn default = replace tbl k $ fn (find_default default tbl k);;
let extrema_by_value (<|>) tbl =
    Hashtbl.fold (fun key value acc ->
        match acc with
        | Some(k, v) -> if value <|> v then Some(key, value) else acc
        | None -> Some(key, value)
    ) tbl None;;
let min_by_value tbl = extrema_by_value (<) tbl
let max_by_value tbl = extrema_by_value (>) tbl
let keys tbl = fst $. List.unzip $. list_of $ tbl;;
end;;

module ExtendSet = functor (SetModule : Set.S) ->
struct
include SetModule
let of_list = List.fold_left (fun acc elem -> add elem acc) empty
let map fn set = fold (fun elem -> add (fn elem)) set empty
let contains value = exists (fun elem -> elem = value)

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

let string_of string_of_t set = List.string_of string_of_t (elements set)
end;;

(* despite the name, use lists internally, because of mutability concerns *)
module MakeStackSet = functor (T : sig type t end) -> ExtendSet(Set.Make(struct type t = T.t list let compare=compare end));;
(*
#load "scm_util.cmo";;
open Scm_util;;
module SS = ExtendSet(Set.Make(String));;
let show = SS.elements;;
let a = SS.of_list ["a";"b";"c";"d"];;
let b = SS.map (fun e -> "prefix_" ^ e) a;;
let c = SS.map_multi (fun e -> [e^"0";e^"1";e^"2"]) b;;
*)
