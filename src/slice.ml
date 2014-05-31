(* curried-style function of 4 parameters, a function, an accumulator, a number (limit), and a list (implicit in match construct)
 * it left-folds the function onto the list for at most n elements, and returns the accumulated value of the fold and the remainder of the list as a tuple
 *)
let rec fold_until f acc n = function
  | [] -> (acc, [])
  | h :: t as l -> if n = 0 then (acc, l)
                   else fold_until f (f acc h) (n-1) t

(* returns a sublist of the list between the first and second indicies *)				   
let slice list (i:int) (k:int) =
  let _, list = fold_until (fun _ _ -> []) [] i list in
  let taken, _ = fold_until (fun acc h -> h :: acc) [] (k - i + 1) list in
  List.rev taken;;
