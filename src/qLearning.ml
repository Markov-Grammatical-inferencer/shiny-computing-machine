open Machine_learning_util
module type TemplateSig = sig 
type state = int
type action = int
(*type qfunction = state * action -> float*)
end;;
let list_random_element l = 
     List.nth l (Random.int (List.length l))       
module Agent (T : TemplateSig) = 
struct  
let numTraining = 100
let epsilon = 0.5 (*exploration rate*)
let alpha = 0.5 (*learning rate*)
let gamma = 1. (*discount factor*)
type actionfn_sig = T.state -> T.action list 
class qLearningAgent (legalactions_of_state : actionfn_sig) = 
object(self) 
    val qstates : (T.state * T.action) counter = new counter
    val episodesSoFar = ref 0
    val accumTrainRewards = ref 0.0
    val accumTestRewards = ref 0.0
method getQValue s a =
    Hashtbl.find qstates#table (s, a)
method computeValueFromQValues s = 
    let temp =
    List.fold_left 
        (fun topQval a -> 
        if self#getQValue s a > topQval 
        then self#getQValue s a 
        else topQval) neg_infinity (legalactions_of_state s) 
        in
        if temp == neg_infinity then 0.0 else temp
method computeActionFromQValues s = 
    let bestActions = ref [] in
    let bestValue = ref neg_infinity in
    List.iter (fun a ->
        if self#getQValue s a > !bestValue
        then (bestValue := self#getQValue s a ;
        bestActions := [a])
        else if self#getQValue s a == !bestValue
        then bestActions := a::!bestActions
    ) (legalactions_of_state s);
    if List.length !bestActions != 0 
    then Some (list_random_element !bestActions) 
    else None 
method getAction s = 
    let legalActions = legalactions_of_state s in
    if flipcoin epsilon
    then Some(list_random_element legalActions) 
    else self#computeActionFromQValues s
end
end;;

