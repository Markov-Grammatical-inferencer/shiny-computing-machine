open Scm_util;;

module SS = ExtendSet(Set.Make(String));;

let a = SS.of_list ["a"];;

(* let num_limit = ipow 2 29;; *)
(* let num_limit = ipow 2 14;; *)
Printf.printf "%s\n%!" Sys.argv.(1);;
let num_limit = int_of_string (Sys.argv).(1);;

let f x = if (String.length x) < num_limit then [x^"A"] else [];;

let b = if (Array.contains "--imp" Sys.argv)
    then SS.set_closure_imp f a
    else SS.set_closure f a;;
