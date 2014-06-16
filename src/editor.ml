open Curses;;
type direction=Up|Down|Left|Right;;
class virtual editor_mode=
	object(self)
		method virtual display_character:int->unit
		method virtual move_after:unit->unit
		method virtual get_mode_name:unit->string
		method move_relative (dy,dx)=
		  let (y,x)=getyx (stdscr ()) in 
		  move (y+dy) (x+dx)
	end;;
class text_mode=
object(self)
	inherit editor_mode
	method display_character (i:int)=
	  ignore(addch i)
	method move_after ()=
	  self#move_relative (0,1);
	  ()

	method get_mode_name ()=
	  "Text"
end;;

class funge_mode=
object(self)
	inherit editor_mode
	val mutable dx=1
	val mutable dy=0		 
	method display_character (i:int)=
	  if i=(Char.code ("v".[0])) then 
	    begin
	      attr_on 1;
	      dy<-1;
	      dx<-0
	    end;
	  if i=(Char.code (">".[0])) then
	    begin
	      attr_on 1;
	      dy<-0;
	      dx<-1;
	    end;
	  if i=(Char.code ("<".[0])) then 
	    begin
	      attr_on 1;
	      dy<-0;
	      dx<- -1;
	    end;
	  if i=(Char.code ("^".[0])) then
	    begin
	      attr_on 1;
	      dy<- -1;
	      dx<-0;
	    end;
	  ignore( addch i)

	method move_after ()=
	  ignore(self#move_relative (dy,dx))
	method get_mode_name ()="Befunge"
	initializer ignore(init_pair 1 Color.cyan Color.black;
			  init_pair 2 Color.yellow Color.black)
end;;
class editor=
object(self)
	val mutable contents:(int*int,char) Hashtbl.t=Hashtbl.create 10
	method position ()=
	  getyx (stdscr ())

	method movecursor (dir:direction)=
	  let cy,cx=self#position () in 
	  match dir with
	  |Right->move cy (cx+1)
	  |Left->move cy (cx-1)
	  |Up->move (cy-1) cx
	  |Down->move (cy+1) cx

	method private handlecursor (i:int)=
	  match i with 
	  |x when x= Keys.enter->
	     ignore(move (let y,_=self#position () in 
		   y+1) 0)
	  |x when x =Keys.right->
	    ignore(self#movecursor Right)
	  |x when x=Keys.left->
	    ignore(self#movecursor Left)
	  |x when x=Keys.up->
	    ignore(self#movecursor Up)
	  |x when x=Keys.down->
	    ignore(self#movecursor Down)
	  |x when x=Keys.backspace||
		    x=Keys.ic->
	    ignore(self#movecursor Left);
	    ignore(delch())
	  |x->try
	       if (Char.escaped (Char.chr x))="^?" then
		 self#handlecursor Keys.left
	       else
		 begin
		   Hashtbl.replace contents (self#position ()) (Char.chr x);
		   ignore(addch x)
		   end
	     with invalid_arg->
	       
	       ()

	method edit_loop ()=
	  let i=ref true in 
	  while !i do
	    let j=getch () in 
	    ignore(self#handlecursor j);
	    refresh ();
	  done;
	  ()
	    
	   
			    
end;;
let ()=
  initscr();
  let j=new editor in
  start_color ();
  keypad (stdscr ())  true;
  ignore(noecho ());
  try
    j#edit_loop ()
  with x->
    endwin ();
    exit 0;;
