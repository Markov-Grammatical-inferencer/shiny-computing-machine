(*
#load "slice.cmo";;
#load "str.cma";;
 *)

(*
Provides exceedingly basic spell checking support. 
Probably not quite what was in mind...

Also thanks to Peter Norvig's "How to Write a Spelling Corrector" for detailing the algorithm which is used here more or less effectively.
*)
type feature=string * string;;
module Stringset=Set.Make(String);;
let load_test_file name=
  let j=ref[] in 
  let i=open_in name in 
  let red_file=(fun fil->
    try
      while true do
	j:=[String.lowercase (input_line fil)]@(!j);
      done;
    with End_of_file->
      ()) in 
  let get_entries=(fun x->
		   if Str.string_match (Str.regexp "[a-z]+") x 0 then
		     Str.matched_string x
		   else ""		  ) in 
  red_file i;
  List.map get_entries !j;;
  

let nwords: Stringset.t ref=ref Stringset.empty;;
let train features=
	  let models=Hashtbl.create 2 in 
	  List.iter (
	      fun x->
	      match x with
	      | n when not (Hashtbl.mem models n)->Hashtbl.replace models x 1;
					     nwords:=Stringset.add x !nwords
	      | n ->let i=Hashtbl.find models n in 
		    Hashtbl.replace models n (i+1)) features;
	  models;;

let modelling:(string,int)Hashtbl.t =Hashtbl.create 10;;

let load_and_train file=
  let i=load_test_file file in 
  let j=train i in 
  Hashtbl.iter (fun x y->
		Hashtbl.replace modelling x y) j;;

let alphabet="abcdefghijklmnopqrstuvwxyz";;
let deletes (things:(string*string) list)=
  List.map (fun (c:string *string)->
	    let x,y=c in
	    x^(String.sub y 1 ((String.length y)-1))) things;;
let transposes (things:(string*string) list)=
  List.map (fun (c:(string*string))->
	    let x,y=c in
	    if (String.length y)>1 then
	      x^(String.sub y 1 1)^(String.sub y 0 1)^(String.sub y 2 ((String.length y)-2))
	    else
	      "") things;;
let inserts (things: (string*string) list)=
  let j=ref[] in 
  for i=0 to 25 do 
    j:=!j@List.map (fun c->
		    let x,y=c in
		    x^(String.sub alphabet i 1)^y) things;
  done;
  (!j);;

let replaces (things:(string * string) list)=
  let j=ref[] in
  for i=0 to 25 do 
    j:=!j@List.map (fun c->
		    let x,y=c in
		    x^(String.sub alphabet i 1)^(String.sub y 1 ((String.length y)-1))) things;
  done;
  (!j);;

let edits word=
  let s=ref[] in 
  for i=0 to ((String.length word)-1) do
    s:=[(String.sub word 0 i),(String.sub word i ((String.length word)-i))]@(!s);
  done;
  let i=(replaces !s)@(inserts !s)@(transposes !s)@(deletes !s) in 
  let j=ref Stringset.empty in 
  List.iter (fun x ->
	     j:=Stringset.add  x !j) i;
  (!j);;

let known_edits word=
  let q=ref Stringset.empty in 
  let j=edits word in 
  let inner=(fun y->
	     if Stringset.mem  y !nwords then
	       q:=Stringset.add y !q;
	     ()) in 

  let outer=(fun x->
	     let i=edits x in
	     Stringset.iter inner i) in
  Stringset.iter outer j;
  !q;;

let known words=
  let i=ref Stringset.empty in 
  List.iter (fun x->
	     if Stringset.mem x !nwords then 
	       i:=Stringset.add x !i) words;
  !i;;

let serialize file=
  (*Serialize the dictionary, so that it may be used more quickly in the future*)
  let out=open_out file in 
  Hashtbl.iter (fun x y->
		Printf.fprintf out "%s %d\n" x y) modelling;
  print_endline "Succeeded in serializing output";
  close_out out;;

let deserialize file=
  (*unserialize a dictionary so that it may be used immediately*)
  let i=open_in file in 
  let lines=ref[] in 
  let get_contents=
    (fun ()->
    try
      while true do
	lines:=[input_line i]@(!lines);
      done;
    with End_of_file->close_in i) in 
  get_contents ();
  let extract_info=(fun x->
		     ignore(Str.string_match (Str.regexp "\\([a-z]+\\) \\([0-9]+\\)") x 0);
		     (Str.matched_group 1 x),(int_of_string (Str.matched_group 2 x))) in 
  let values=List.map extract_info !lines in 
  List.iter (fun c->
	     let x,y=c in 
	     nwords:=Stringset.add x !nwords;
	     Hashtbl.add modelling x y) values;;

let get_candidates word=
  (*Return all of the candidates which are shown to be in the dictionary*)
   let a=(Stringset.union (known (Stringset.elements (edits word))) (known_edits word)) in 
  let b=(Stringset.union (known_edits word) a) in 
  let c=Stringset.remove "" (Stringset.add word b) in 
  let i=known (Stringset.elements c) in 
  i;;

let correct word=
  (*Get the most likely word for the correction*)
  let a=(Stringset.union (known (Stringset.elements (edits word))) (known_edits word)) in 
  let b=(Stringset.union (known_edits word) a) in 
  let c=Stringset.remove "" (Stringset.add word b) in 
  let mx=ref 0 in 
  let candid=ref "No word found" in
  print_endline (string_of_int !mx);
  Stringset.iter (fun x->
		  if Hashtbl.mem modelling x then 
		    begin
		      let j=Hashtbl.find modelling x in 
		      if j>(!mx) then 
			begin 
			  print_endline (string_of_int j);
			  print_endline x;
			  mx:=j;
			  candid:=x;
			end
		    end) c;
  !candid;;
  
