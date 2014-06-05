(*
#load "graphics.cma";;
#load "dismambiguator.cma";;
 *)
open Graphics;;
open Disambiguator;;
type m_stack=B of int * m_stack
	    |NIL of int;;
let display_ngram (t:string) (d:disambiguator)=
  open_graph ":0";
  let i=ref 10 in 
  let j=d#get_word_context t in 
  let f=List.map d#get_word j.prec in 
  let g=List.map d#get_word j.follow in
  let c=Array.make ((List.length j.prec)+(List.length j.follow)) 0 in 
  let counter=ref 0 in 
  (*Draw the words before the center*)
  List.iter (fun x->
	    moveto !i 30;
	    draw_string x;
	    counter:=!counter+1;
	    Array.set c !counter (!i + (5*(String.length x)));
	    i:=!i+10*(String.length x)+10) f;
  set_color (rgb 255 0 0);
  moveto !i 30;
  draw_string t;
  set_color (rgb 0 0 0 );
  let center = (!i +(5*(String.length t))) in 
  i:=!i+10*(String.length t)+10;
  (*Draw the words following the center*)
  List.iter (fun x->
	     moveto !i 30;
	     draw_string x;
	     counter:=!counter+1;
	     c.(!counter)<-(!i + (5*(String.length x)));
	     i:=!i+10*(String.length x)+10) g;
  (*Draw the curves going to the central word*)
  Array.iter (fun x->
	      moveto x 30;
	      curveto (x+10,50) (x+15,50) (center, 30)) c;;


