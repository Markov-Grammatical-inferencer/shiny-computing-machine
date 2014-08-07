(*
#load "scm_util.cmo";;
#load "machine_learning_util.cmo";;
#load "qLearning.cmo";;
#load "prisoners_dilemma.cmo";;
open Prisoners_dilemma;;
open Scm_util;;
open PD_types;;
*)
open Scm_util
open QLearning
module PD_types = 
struct
type action = Cooperate | Defect
type state = (action * action) list
end;;
open PD_types
module PDAgent = Agent (PD_types);;
(*Numbers taken from Wikipedia example on Prisoner's 
Dilemma but reversed (4-x)  so a lower sentence corresponds to
a greater reward*)
let calculatescore (action1, action2) = 
    match action1 with 
    | Cooperate -> (match action2 with Cooperate -> (3,3) | Defect -> (1,4))
    | Defect -> (match action2 with Cooperate -> (4,1) | Defect -> (2,2));;    
let random_move () = List.random_element [Cooperate; Defect]
(*let choose = new Agent*)
let randomTraining numgames gamelen = 
let agent = new PDAgent.qLearningAgent (fun s -> [Cooperate; Defect]) in
for i = 0 to numgames do
let currentState = ref [] in
for j = 0 to gamelen do 
let agentMove = random_move() in 
let opponentMove = random_move() in
let nextState = (agentMove, opponentMove) :: !currentState in
currentState := nextState;
agent#update !currentState agentMove nextState (float_of_int $. fst $. calculatescore $ (agentMove, opponentMove))
done;
done;
agent;;
(*
let a = randomTraining 100 100;;
let b =  a#getQStates#table;;
let tag_to_char k = match k with Cooperate -> 'c' | Defect -> 'd';;
let convert_states st's = List.map (fun (x,y) -> (tag_to_char x, tag_to_char y)) st's;;
let c = Hashtbl.map (fun (s,a) v -> ((convert_states s,tag_to_char a),v)) b;;
Hashtbl.list_of c;;
*)
class ['a] gameState = 
object(self) 
val s : 'a list ref= ref []
method value = !s
method update x = s := x :: !s
end;;

let runrandgame (agent:PDAgent.qLearningAgent) gamelen = 
let agentScore = ref (0) in
let opponentScore = ref (0) in
let currentState = new gameState in 
for i = 0 to gamelen do
    let agentMove_opt = agent#getAction currentState#value in 
    let agentMove = match agentMove_opt with 
        | Some(x) -> x 
        | None -> Cooperate in
    let opponentMove = random_move() in 
    let moves = agentMove, opponentMove in 
    currentState#update moves; 
    let (a,b) = calculatescore $ moves in 
    agentScore += a;
    opponentScore += b;
done;    
(!agentScore, !opponentScore);;

let runcontrolgame gamelen = 
let r1Score = ref (0) in
let r2Score = ref (0) in
let currentState = new gameState in
for i = 0 to gamelen do
    let r1Move = random_move() in
    let r2Move = random_move() in
    let moves = r1Move, r2Move in
    currentState#update moves;
    let (a,b) = calculatescore $ moves in
    r1Score += a;
    r2Score += b;
done;
(!r1Score, !r2Score);;
