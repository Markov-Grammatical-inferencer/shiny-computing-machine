open Curses;;
type filename=Path of string|None;;
type editor_arguments={position:(int*int);
		       current_file:filename};;

class editor (e:editor_arguments)=
object(self)
	val window=initscr ()(*The screen*)
	val mutable size=(0,0)
	val mutable width=0
	val mutable height=0
	val mutable lines:(int, string) Hashtbl.t=Hashtbl.create 1
	val mutable position:(int*int)=(e.position)
	val mutable current_file:filename=e.current_file
	val mutable curmsg:string=""
	method draw_to_screen ()=
	  let accumulator=ref 0 in
	  for i=1 to (Hashtbl.length lines) do
	    String.iter (fun x->
			 ignore(addch (Char.code x))) (Hashtbl.find lines i);
	    ignore(move i 0);
	  done;
	  self#status_line_write curmsg;
	  refresh()
	method status_line_write (msg:string)=
	  curmsg<-msg;
	  move (height-1) 0;
	  ignore(printw  "%s" msg);
	  let cy,cx=position in 
	  ignore(move cy cx);(*Move back to where the user's cursor*)
	  ()
	method movecursor (i:int)=
	  let cy,cx=position in 
	  position<-(if i = 0 then 
		       (cy ,cx+1)
		     else if i = 1 then 
		       (cy-1,cx)
		     else if i = 2 then 
		       (cy,cx-1)
		     else if i = 3 then 
		       (cy+1,cx)
		     else (cy,cx));
	  self#status_line_write ((string_of_int cx)^(string_of_int cy));
	  let a,b=position in 
	  ignore(move a b);
	  ()


	method load_file ()=
	  match current_file with 
	  |  None->Hashtbl.replace lines 1 "";
		   ()
	  |Path (x)->
	    let istream=open_in x in 
	    let i=ref 1 in
	    let readit=
	      (fun ()->
	      try 
		while true do 
		  let nline=input_line istream in 
		  Hashtbl.replace lines !i nline;
		  i:=!i+1
		done
	      with End_of_file->
		self#status_line_write("File opened");(*Write to the status_line*)
	      ) in 
	    readit ();
	    ignore(self#draw_to_screen ())
		 

	method getaction (key:int)=
	  if key = Keys.right then 
	    0
	  else if key = Keys.up then 
	    1
	  else if key = Keys.left then 
	    2
	  else if key = Keys.down then 
	    3
	  else
	    4


	method edit_loop ()=
	  let cy,cx=position in
	  let cy=cy+1 in 
	  while true do 
	    let j=getch () in 
	    clear ();
	    
	     let matching =(fun ()-> match j with 
		x when x=Keys.enter->
		self#status_line_write "Newline";
		for index=(Hashtbl.length lines) downto (cy) do 
		  Hashtbl.replace lines (index+1) (Hashtbl.find lines index)
		done;
		Hashtbl.replace lines cy "";
		position<-(cy+1,0);
		let (a,b)=position in 
		ignore(move a b);
		()
	       |x when x=Keys.right->
		 self#movecursor 0
	       |x when x=Keys.up->
		 self#movecursor 1
	       |x when x=Keys.left->
		 self#movecursor 2
	       |x when x=Keys.down->
		 self#movecursor 3
	       |j->
		 let i=Hashtbl.find lines cy in 
		 if (cy = (String.length i) || (cy = (String.length i)-1)) then
		   Hashtbl.replace lines cy (i^(Char.escaped (Char.chr j)))
		 else if   not (i = "") then 
		   begin
		     try
		       let j=(String.sub i 0 (j-1))^(Char.escaped (Char.chr j))^(String.sub i (j+1) ((String.length i)-(j+1))) in 
		       Hashtbl.replace lines cy j;
		       self#movecursor 0
		     with x->
		       Hashtbl.replace lines cy (i^(Char.escaped (Char.chr j)))
		   end 
		 else
		   Hashtbl.replace lines cy (Char.escaped (Char.chr j))
			   ) in 
	     matching ();
	     ignore(self#draw_to_screen ())
	  done
	
	method addLine (s:string)=
	  Hashtbl.replace lines ((Hashtbl.length lines)+1) s

	initializer (size<-getmaxyx window;
		     let h,w=size in
		     width<-w;
		     height<-h;
		     self#load_file ();
		     ignore(keypad window true))

end;;
let ()=
  let i=new editor {position=(0,0);current_file=None} in
  ignore(noecho ());
  for j=0 to 10 do 
    i#addLine (string_of_int j)
  done;
  try
    i#edit_loop ()
  with x->
    endwin ();;
