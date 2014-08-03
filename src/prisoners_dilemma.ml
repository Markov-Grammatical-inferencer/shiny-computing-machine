(*#load "scm_util.cmo";;
#load "prisoners_dilemma.cmo";;
open Prisoners_dilemma;;*)
open Scm_util
open QLearning
module PD_types = 
struct
type action = Cooperate | Defect
type state = int
end
open PD_types
(*Numbers taken from Wikipedia example on Prisoner's 
Dilemma but reversed (4-x)  so a lower sentence corresponds to
a greater reward*)
let calculatescore (action1, action2) = 
    match action1 with 
    | Cooperate -> (match action2 with Cooperate -> (3,3) | Defect -> (1,4))
    | Defect -> (match action2 with Cooperate -> (4,1) | Defect -> (2,2))
    
let random_move () = List.random_element [Cooperate; Defect]

