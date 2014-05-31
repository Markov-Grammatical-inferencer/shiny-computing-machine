open Slice;;
module IntSet=Set.Make(struct 
			let compare=Pervasives.compare 
			type t=int
		      end);; 

type textual={prec:int list;follow:int list};;
type word={id:int;ctx:IntSet.t};;
class disambiguator=
object(self)
  val mutable reverse_words:(int, string) Hashtbl.t=Hashtbl.create 1
  val mutable context:(textual, int) Hashtbl.t= Hashtbl.create 2
  val mutable words:(string,word) Hashtbl.t= Hashtbl.create 2
  val mutable lastword:int=0
  method addword (w:string) =
    if not (Hashtbl.mem words w)then
      begin
        Hashtbl.add words w {id=lastword;ctx=IntSet.empty};
        Hashtbl.add reverse_words lastword w;
        lastword<-lastword+1;
        lastword-1
      end
    else
      self#getwordcode w
  method addcontext (c:textual) (w:string)=
    assert (Hashtbl.mem words w);(*All words should be added before use*)
    if not (Hashtbl.mem context c)then
      begin
        Hashtbl.add context c lastword;
	let i=Hashtbl.find words w in
        Hashtbl.replace words w {id=i.id;ctx=(IntSet.add lastword i.ctx)};(*Add the context to the word*)
        lastword<-lastword+1;
        lastword-1(*return the context identifier*)
      end
    else
      let index=(Hashtbl.find context c) in(*
                             the context exists, therefore, return it's number*)
      let wc=(Hashtbl.find words w) in
      if not (IntSet.mem index  wc.ctx)then
        Hashtbl.replace words w {id=wc.id;ctx=(IntSet.add index wc.ctx)};
      index

  method getwordcode (w:string)=
    (*
     * Return the last code, which
     * is the word as it was originally reported.
     * Should be constant time.
     *)
    let i=Hashtbl.find words w in
    i.id

  method search_for_potential_similar_words (threshold:float)=
    (*TODO Write this function*)
    let getkeys=fun h -> Hashtbl.fold (fun k v acc -> k :: acc) h [] in
    let minset=fun h c-> if (IntSet.cardinal h)>(IntSet.cardinal c) then 
			   IntSet.cardinal c 
			 else 
			   IntSet.cardinal h in 
    let dc=getkeys words in (*Obtain the words in the thingies*)
    for i =0 to (List.length dc) do
      for j=0 to (List.length dc) do
	if i<>j then
	  begin
	    let a=Hashtbl.find words (List.nth dc i) in 
	    let b=Hashtbl.find words (List.nth dc j) in 
	    let c=IntSet.inter a.ctx b.ctx in 
	    let d=float_of_int (IntSet.cardinal c) in 
	    let e=float_of_int (minset a.ctx b.ctx) in
	    if (d/.e)>=threshold then
	      begin
		Hashtbl.replace words (List.nth dc i) {id=a.id;ctx=(IntSet.union a.ctx b.ctx)};
		Hashtbl.replace words (List.nth dc j) {id=b.id;ctx=(IntSet.union a.ctx b.ctx)};
	      end;
	   end;
      done
    done;
    ()

  method doeswordappearin (c:textual) (w:string)=
    (*Returns whether or not a word appears in a specific context. *)
    let wc=(Hashtbl.find words w) in
    IntSet.mem (Hashtbl.find context c) wc.ctx
  
  method print_info=
    print_string "Number of unique contexts ";
    print_int (Hashtbl.length context);
    print_endline "\\n This is the number of unique words known";
    print_int (Hashtbl.length words)

  method print_context (c:textual)=
    print_endline "These are the preceeding words";
    List.iter (fun x->print_endline (Hashtbl.find reverse_words x)) c.prec;
    print_endline "These are the following words";
    List.iter (fun x->print_endline (Hashtbl.find reverse_words x)) c.follow;
    ()

  method generate_context (p:string list) (f:string list)=
    (*Generates a context by the list of words used.*)
    let p=List.map self#getwordcode p in
    let c=List.map self#getwordcode f in
    {prec=p;follow=c}

  method process_into_context (s:string list)=
    let v = (List.length s) in
    List.iter (fun x->ignore(self#addword x)) s;
    try
    if v>=3 then
      begin
	for i=3 to (v-3) do
	  for offset=1 to 3 do
	    let c=(slice s (i-offset) (i-1)) in
	    let d=(slice s (i+1) (i+offset))in
	    self#addcontext (self#generate_context c d) (List.nth s i)
	  done
	done;
      end
    else 
      begin
	for i=1 to (List.length s) do
	  let c=slice s i ((List.length s)-1) in 
	  self#addcontext (self#generate_context [] c) (List.nth s i);
	done
      end;
    with Failure msg->
      print_endline "this is broken"
  end;;

