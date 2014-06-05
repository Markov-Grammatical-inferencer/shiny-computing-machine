(*
#load "slice.cmo";;
#load "str.cma";;
 *)
open Slice;;
type feature=string * string;;
module Stringset=Set.Make(String);;
let load_test_file name=
  let j=ref[] in 
  let i=open_in name in 
  let red_file=(fun fil->
    try
      while true do
	j:=[input_line fil]@(!j);
      done;
    with End_of_file->
      ()) in 
  let get_entries=(fun x->
		   Str.string_match (Str.regexp "\\([a-z]+\\)    \\([a-z]+\\)") x 0;
		   (Str.matched_group 1 x),(Str.matched_group 2 x)) in 
  red_file i;
  List.map get_entries !j;;
  
let nwords: Stringset.t ref=ref Stringset.empty;;
let modelling:(string,int)Hashtbl.t =Hashtbl.create 1;;
let train features=
	  let models=Hashtbl.create 2 in 
	  List.iter (
	      fun x-> 
	      match x with
	      | n when Hashtbl.mem models n->Hashtbl.replace models x 1
	      | n ->let i=Hashtbl.find models n in 
		    Hashtbl.replace models n (i+1)) features;
	  models;;
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
  for i=0 to ((String.length word)+1) do
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

let correct word=
  let a=(Stringset.union (known (Stringset.elements (edits word))) (known [word])) in 
  let b=(Stringset.union (known_edits word) a) in 
  let c=Stringset.add word b in 
  let mx=ref 0 in 
  let candid=ref "" in
  Stringset.iter (fun x->let j=Hashtbl.find modelling x in 
		    if j>(!mx) then 
		      begin 
			mx:=j;
			candid:=x;
		      end;) c;
  !candid;;
  
