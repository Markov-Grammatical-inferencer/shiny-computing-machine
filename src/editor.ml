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
	method get_mode_name ()=
	  "Befunge"
	initializer ignore(init_pair 1 Color.cyan Color.black;
			  init_pair 2 Color.yellow Color.black)
end;;
class editor=
object(self)
	val mutable contents=Array.create_matrix 0 0 (Char.code (" ".[0]))
	val mutable offset=0(*The offset for scrolling*)
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
	     ignore(self#movecursor Down);
	     let cy,_=self#position () in 
	     ignore(move cy 0)(*Move all the way to the left*)
	  |x when x =Keys.right->
	    ignore(self#movecursor Right)
	  |x when x=Keys.left->
	    ignore(self#movecursor Left)
	  |x when x=Keys.up->
	    ignore(self#movecursor Up)
	  |x when x=Keys.down->
	    let cy,cx=self#position () in 
	    let my,mx=getmaxyx (stdscr ()) in 
	    if (cy-1)=my then 
	      contents<-Array.append contents (Array.make_matrix 1 mx (Char.code (" ".[0])));
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




