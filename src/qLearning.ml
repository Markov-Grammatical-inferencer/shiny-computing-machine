module type TemplateSig = sig 
type state = int
type action = int
(*type qfunction = state * action -> float*)
end;;
module Agent (T : TemplateSig) = struct  
let numTraining = 100
let epsilon = 0.5 (*exploration rate*)
let alpha = 0.5 (*learning rate*)
let gamma = 1. (*discount factor*)
type actionfn_sig = state -> action list 
class qLearningAgent (actions_of_state : actionfn_sig) = 
object(self) 
    val qstates : (state * action) counter = new counter
    val episodesSoFar = ref 0
    val accumTrainRewards = ref 0.0
    val accumTestRewards = ref 0.0
method getQValue s a =
    Hashtbl.find qstates#table (s, a)
method computeValueFromQValues s = 
    let temp =
    list.fold_left 
        (fun topQval elem -> 
        if self#getQValue s a > topQval 
        then self#getQValue s a 
        else topQval) neg_infinity (actions_of_state s) 
        in
        if topQval == neg_infinity then 0.0 else topQval
end
end;;

