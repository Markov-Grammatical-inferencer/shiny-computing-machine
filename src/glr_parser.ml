(*
Machine Learning algorithm based on Hidden Markov Models
Compute the probability of the appearence of a word given N adjacent words?
*)

(* GLR Parser *)

open Scm_util;;

type 'a tree = Node of ('a * ('a tree list));;

(*
#load "lablglut.cma";;
#load "lablgl.cma";;
*)

(* Node("S",[Node("NP",[Node("NN",[Node("I",[])])]);Node("VP",[Node("VBZ",[Node("am",[])])])]);;*)
