(*import sys
import inspect
import heapq, random
import cStringIO*)

open Scm_util;;

(*"""
 Data structures useful for implementing SearchAgents
"""*)

class ['a]  stack =
(*A container with a last-in-first-out (LIFO) queuing policy*)
object (self)
    val mutable list = ( [] : 'a list )
    method push x =
        list <- x :: list
    method pop =
        let result = List.hd list in
        list <- List.tl list;
        result
    method is_empty =
         List.length list == 0
end;;

class ['a] queue =
    (*A container with a first-in-first-out (FIFO) queuing policy.*)
    object (self)
    val q  : 'a Queue.t = Queue.create () 
    method push x =
        Queue.push x q
    method pop =
        Queue.pop q
    method is_empty =
        Queue.is_empty q
    end;;

module PriorityQueue =
struct 
type priority = int
type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
exception Queue_is_empty
let rec insert queue prio elt = 
    match queue with
        Empty -> Node(prio, elt, Empty, Empty)
    | Node(p, e, left, right) -> 
        if prio <= p
        then Node(prio, elt, insert right p e, left)
        else Node(p, e, insert right prio elt, left)
let rec remove_top = function
    Empty -> raise Queue_is_empty
    | Node(prio, elt, left, Empty) -> left
    | Node(prio, elt, Empty, right) -> right
    | Node(prio, elt, (Node(lprio, lelt, _, _) as left),
                        (Node(rprio, relt, _, _) as right)) ->
        if lprio <= rprio
        then Node(lprio, lelt, remove_top left, right)
        else Node(rprio, relt, left, remove_top right)
let top q = match q with 
    Empty -> raise Queue_is_empty
    | Node(prio, elt, _, _) -> (prio, elt)
let method_get internal_queue = internal_queue
let method_top internal_queue = top !internal_queue
let method_insert internal_queue prio elt = inplace (fun q -> insert q prio elt) internal_queue
let method_remove_top internal_queue = inplace remove_top internal_queue
let method_extract internal_queue = let rv = method_top internal_queue in method_remove_top internal_queue; rv
class ['a] priority_queue = 
    object(self)
        val internal_queue : 'a queue ref = ref Empty
        method insert = method_insert internal_queue
        method remove_top = method_remove_top internal_queue
        method extract = method_extract internal_queue
        method get = method_get internal_queue
        method top = method_top internal_queue
    end
class ['a] priority_queue_with_function fn = 
    object(self)
        val internal_queue : 'a queue ref = ref Empty
        method insert elt = method_insert internal_queue (fn elt) elt
        method remove_top = method_remove_top internal_queue
        method extract = method_extract internal_queue
        method get = method_get internal_queue
        method top = method_top internal_queue
    end
end;;

let manhattanDistance (x1,y1) (x2,y2) = 
    (*Returns the Manhattan distance between points xy1 and xy2*)
    abs( x1 - x2 ) + abs( y1 - y2 );;

class ['k] counter =
object (self)
    val mutable internal_table = Hashtbl.create 0
method get (idx : 'k)  = Hashtbl.find_default 0. internal_table idx
method increment_all keylist amount = List.iter (fun key -> Hashtbl.add internal_table key (amount+.(self#get key)) ) keylist
method kvmax = Hashtbl.fold
    (fun k v acc ->
        match acc with
        | Some(ak,av) -> if v > av then Some(k,v) else Some(ak,av)
        | None -> Some(k,v)
    ) internal_table None
method argmax = match self#kvmax with | Some(k,v) -> Some(k) | None -> None
method table = internal_table
method sortedKeys = 
    let items = Hashtbl.list_of internal_table in 
    let cmp (k1, v1) (k2, v2) = compare v1 v2 in
    let sortedItems = List.sort cmp items in
    fst $ List.unzip sortedItems

method keys = fst $ List.unzip (Hashtbl.list_of internal_table)
method values = snd $ List.unzip (Hashtbl.list_of internal_table)
method table = internal_table
method totalCount = List.fold_left (+.) 0. self#values
method normalize = self#divideAll self#totalCount
method divideAll divisor =
    internal_table <- Hashtbl.map
        (fun k v -> (k, (v /. divisor))) internal_table
    
method copy = {<internal_table = Hashtbl.map (fun k v -> (k,v)) internal_table >} 
method mul (other: 'k counter) =
    let mixed_table = Hashtbl.zip internal_table other#table 0. 0. in
    let vals = snd $ List.unzip (Hashtbl.list_of mixed_table) in
    let prods = List.map (fun (x,y) -> x*.y) vals in
    List.fold_left (+.) 0. prods
method increment (other: 'k counter) = 
    Hashtbl.iter (fun k v -> 
        Hashtbl.replace internal_table k ((self#get k)+. v)
        ) other#table
method decrement (other: 'k counter) =
     self#increment other#negate
method add (other: 'k counter) =
    let temp = self#copy in
    temp#increment other; 
    temp
method negate = 
    {<internal_table = Hashtbl.map (fun k v -> (k , (~-. v))) internal_table >}
method sub (other: 'k counter) = 
  let temp = self#copy in
    temp#decrement other;
    temp  
end;;

(* def normalize(vectorOrCounter): *)
(*     (* *)
(*     normalize a vector or counter by dividing each value by the sum of all values *)
(*     *) *)
(*     normalizedCounter = Counter() *)
(*     if type(vectorOrCounter) == type(normalizedCounter): *)
(*         counter = vectorOrCounter *)
(*         total = float(counter.totalCount()) *)
(*         if total == 0: return counter *)
(*         for key in counter.keys(): *)
(*             value = counter[key] *)
(*             normalizedCounter[key] = value / total *)
(*         return normalizedCounter *)
(*     else: *)
(*         vector = vectorOrCounter *)
(*         s = float(sum(vector)) *)
(*         if s == 0: return vector *)
(*         return [el / s for el in vector] *)
(*  *)
(* def nSample(distribution, values, n): *)
(*     if sum(distribution) != 1: *)
(*         distribution = normalize(distribution) *)
(*     rand = [random.random() for i in range(n)] *)
(*     rand.sort() *)
(*     samples = [] *)
(*     samplePos, distPos, cdf = 0,0, distribution[0] *)
(*     while samplePos < n: *)
(*         if rand[samplePos] < cdf: *)
(*             samplePos += 1 *)
(*             samples.append(values[distPos]) *)
(*         else: *)
(*             distPos += 1 *)
(*             cdf += distribution[distPos] *)
(*     return samples *)
(*  *)
(* def sample(distribution, values = None): *)
(*     if type(distribution) == Counter: *)
(*         items = distribution.items() *)
(*         distribution = [i[1] for i in items] *)
(*         values = [i[0] for i in items] *)
(*     if sum(distribution) != 1: *)
(*         distribution = normalize(distribution) *)
(*     choice = random.random() *)
(*     i, total= 0, distribution[0] *)
(*     while choice > total: *)
(*         i += 1 *)
(*         total += distribution[i] *)
(*     return values[i] *)
(*  *)
(* def sampleFromCounter(ctr): *)
(*     items = ctr.items() *)
(*     return sample([v for k,v in items], [k for k,v in items]) *)
(*  *)
(* def getProbability(value, distribution, values): *)
(*     (* *)
(*       Gives the probability of a value under a discrete distribution *)
(*       defined by (distributions, values). *)
(*     *) *)
(*     total = 0.0 *)
(*     for prob, val in zip(distribution, values): *)
(*         if val == value: *)
(*             total += prob *)
(*     return total *)
(*  *)
(* def flipCoin( p ): *)
(*     r = random.random() *)
(*     return r < p *)
(*  *)
let flipcoin p =
    let r = Random.float 1. in
    r < p
(* def chooseFromDistribution( distribution ): *)
(*     "Takes either a counter or a list of (prob, key) pairs and samples" *)
(*     if type(distribution) == dict or type(distribution) == Counter: *)
(*         return sample(distribution) *)
(*     r = random.random() *)
(*     base = 0.0 *)
(*     for prob, element in distribution: *)
(*         base += prob *)
(*         if r <= base: return element *)
(*  *)
(* def nearestPoint( pos ): *)
(*     (* *)
(*     Finds the nearest grid point to a position (discretizes). *)
(*     *) *)
(*     ( current_row, current_col ) = pos *)
(*  *)
(*     grid_row = int( current_row + 0.5 ) *)
(*     grid_col = int( current_col + 0.5 ) *)
(*     return ( grid_row, grid_col ) *)
(*  *)
(* (* Returns 1 or -1 depending on the sign of x *) *)
(* let sign x = if( x >= 0 ) then  1 else -1 ;; *)
(*  *)
(* def arrayInvert(array): *)
(*     (* *)
(*     Inverts a matrix stored as a list of lists. *)
(*     *) *)
(*     result = [[] for i in array] *)
(*     for outer in array: *)
(*         for inner in range(len(outer)): *)
(*             result[inner].append(outer[inner]) *)
(*     return result *)
(*  *)
(* def matrixAsList( matrix, value = True ): *)
(*     (* *)
(*     Turns a matrix into a list of coordinates matching the specified value *)
(*     *) *)
(*     rows, cols = len( matrix ), len( matrix[0] ) *)
(*     cells = [] *)
(*     for row in range( rows ): *)
(*         for col in range( cols ): *)
(*             if matrix[row][col] == value: *)
(*                 cells.append( ( row, col ) ) *)
(*     return cells *)
(*  *)
(* def lookup(name, namespace): *)
(*     (* *)
(*     Get a method or class from any imported module from its name. *)
(*     Usage: lookup(functionName, globals()) *)
(*     *) *)
(*     dots = name.count('.') *)
(*     if dots > 0: *)
(*         moduleName, objName = '.'.join(name.split('.')[:-1]), name.split('.')[-1] *)
(*         module = __import__(moduleName) *)
(*         return getattr(module, objName) *)
(*     else: *)
(*         modules = [obj for obj in namespace.values() if str(type(obj)) == "<type 'module'>"] *)
(*         options = [getattr(module, name) for module in modules if name in dir(module)] *)
(*         options += [obj[1] for obj in namespace.items() if obj[0] == name ] *)
(*         if len(options) == 1: return options[0] *)
(*         if len(options) > 1: raise Exception, 'Name conflict for %s' *)
(*         raise Exception, '%s not found as a method or class' % name *)
(*  *)
(* def pause(): *)
(*     (* *)
(*     Pauses the output stream awaiting user feedback. *)
(*     *) *)
(*     print "<Press enter/return to continue>" *)
(*     raw_input() *)
(*  *)
(*  *)
(* # code to handle timeouts *)
(* # *)
(* # FIXME *)
(* # NOTE: TimeoutFuncton is NOT reentrant.  Later timeouts will silently *)
(* # disable earlier timeouts.  Could be solved by maintaining a global list *)
(* # of active time outs.  Currently, questions which have test cases calling *)
(* # this have all student code so wrapped. *)
(* # *)
(* import signal *)
(* import time *)
(* class TimeoutFunctionException(Exception): *)
(*     (*Exception to raise on a timeout*) *)
(*     pass *)
(*  *)
(*  *)
(* class TimeoutFunction: *)
(*     def __init__(self, function, timeout): *)
(*         self.timeout = timeout *)
(*         self.function = function *)
(*  *)
(*     def handle_timeout(self, signum, frame): *)
(*         raise TimeoutFunctionException() *)
(*  *)
(*     def __call__(self, *args, **keyArgs): *)
(*         # If we have SIGALRM signal, use it to cause an exception if and *)
(*         # when this function runs too long.  Otherwise check the time taken *)
(*         # after the method has returned, and throw an exception then. *)
(*         if hasattr(signal, 'SIGALRM'): *)
(*             old = signal.signal(signal.SIGALRM, self.handle_timeout) *)
(*             signal.alarm(self.timeout) *)
(*             try: *)
(*                 result = self.function( *args, **keyArgs) *)
(*             finally: *)
(*                 signal.signal(signal.SIGALRM, old) *)
(*             signal.alarm(0) *)
(*         else: *)
(*             startTime = time.time() *)
(*             result = self.function( *args, **keyArgs) *)
(*             timeElapsed = time.time() - startTime *)
(*             if timeElapsed >= self.timeout: *)
(*                 self.handle_timeout(None, None) *)
(*         return result *)
(*  *)
(*  *)
(*  *)
(* _ORIGINAL_STDOUT = None *)
(* _ORIGINAL_STDERR = None *)
(* _MUTED = False *)
(*  *)
(* class WritableNull: *)
(*     def write(self, string): *)
(*         pass *)
(*  *)
(* def mutePrint(): *)
(*     global _ORIGINAL_STDOUT, _ORIGINAL_STDERR, _MUTED *)
(*     if _MUTED: *)
(*         return *)
(*     _MUTED = True *)
(*  *)
(*     _ORIGINAL_STDOUT = sys.stdout *)
(*     #_ORIGINAL_STDERR = sys.stderr *)
(*     sys.stdout = WritableNull() *)
(*     #sys.stderr = WritableNull() *)
(*  *)
(* def unmutePrint(): *)
(*     global _ORIGINAL_STDOUT, _ORIGINAL_STDERR, _MUTED *)
(*     if not _MUTED: *)
(*         return *)
(*     _MUTED = False *)
(*  *)
(*     sys.stdout = _ORIGINAL_STDOUT *)
(*     #sys.stderr = _ORIGINAL_STDERR *)
(*  *)
