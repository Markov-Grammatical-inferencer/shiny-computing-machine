open Curses;;
type direction=Up|Down|Left|Right;;
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
	    ignore(self#movecursor Left)
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
  keypad (stdscr ())  true;
  ignore(noecho ());
  try
    j#edit_loop ()
  with x->
    endwin ();
    exit 0;;
