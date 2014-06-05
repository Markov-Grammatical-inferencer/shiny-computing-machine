open Slice;;
module IntSet=Set.Make(struct 
			let compare=Pervasives.compare 
			type t=int
		      end);; 

type textual={prec:int list;follow:int list};;
type word={id:int;ctx:IntSet.t};;
(*
Generate sub contexts based upon the extant full one.

generate_sub_contexts t x generates a list of contexts with a minimum of x words to either side.
 *)
let rec generate_sub_contexts (ctx:textual) (minlen:int)=
  if (min (List.length ctx.prec) (List.length ctx.follow))>minlen then
    [{prec=(List.tl ctx.prec);follow=(List.tl ctx.follow)}]@(generate_sub_contexts {prec=(List.tl ctx.prec);follow=(List.tl ctx.follow)} minlen)
  else
    [];;
class disambiguator=
object(self)
  val mutable reverse_words:(int, string) Hashtbl.t=Hashtbl.create 1
  val mutable reverse_context:(int, textual) Hashtbl.t =Hashtbl.create 3
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
	Hashtbl.add reverse_context lastword c;
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

  method search_for_potential_similar_words ?d:(debug=false) (threshold:float) =
    (*TODO Write this function*)
    let getkeys=fun h -> Hashtbl.fold (fun k v acc -> k :: acc) h [] in
    let minset=fun h c-> if (IntSet.cardinal h)>(IntSet.cardinal c) then 
			   IntSet.cardinal c 
			 else 
			   IntSet.cardinal h in 
    let dc=getkeys words in (*Obtain the words in the thingies*)
   (* let counter=ref 0 in *)
    let counter=ref 0 in 
    let wordsret:(string *string) list ref= ref [] in 
    let map1func=
      (fun x->
      let map2func=
	(fun y->
	if x<>y then
	  begin
	    let a=Hashtbl.find words x in 
	    let b=Hashtbl.find words y in 
	    let c=IntSet.inter a.ctx b.ctx in 
	    let d=float_of_int (IntSet.cardinal c) in
	    let e=float_of_int (minset a.ctx b.ctx) in 
	    if (d/.e)>=threshold then 
	      begin
(*		let f=IntSet.union a.ctx b.ctx in 
		Hashtbl.replace words x {id=a.id;ctx=f};
		Hashtbl.replace words y {id=b.id;ctx=f};
*)
		if debug then
		    IntSet.iter (fun x ->self#print_context (Hashtbl.find reverse_context x)) c;
		wordsret:=!wordsret@[(x,y)];
		counter:=!counter+1
	      end
	  end) in 	
      List.iter map2func dc) in 
    List.iter map1func dc;
    if debug then print_endline (string_of_int !counter);
    !wordsret

  method doeswordappearin (c:textual) (w:string)=
    (*Returns whether or not a word appears in a specific context. *)
    let wc=(Hashtbl.find words w) in
    IntSet.mem (Hashtbl.find context c) wc.ctx
  
  method print_info=
    print_string "Number of unique contexts ";
    print_int (Hashtbl.length context);
    print_endline "\n This is the number of unique words known ";
    print_int (Hashtbl.length words);
    print_endline"";
    ()

  method print_context (c:textual)=
    print_endline "These are the preceeding words";
    List.iter (fun x->print_endline (Hashtbl.find reverse_words x)) c.prec;
    print_endline "These are the following words";
    List.iter (fun x->print_endline (Hashtbl.find reverse_words x)) c.follow;
    ()

  method get_word_context (s:string)=
    Hashtbl.find reverse_context (IntSet.min_elt (Hashtbl.find words s).ctx)

  method generate_context (p:string list) (f:string list)=
    (*Generates a context by the list of words used.*)
    let p=List.map self#getwordcode p in
    let c=List.map self#getwordcode f in
    {prec=p;follow=c}

  method get_word (i:int)=
    Hashtbl.find reverse_words i

  method process_into_context (s:string list)=
    let v = (List.length s) in
    List.iter (fun x->ignore(self#addword x)) s;
    let d=self#generate_context [] (slice s 1 ((List.length s)-1)) in
    if v>0 then
      ignore(self#addcontext d (List.hd s));
    for i=1 to (v-1) do
      let e=(self#generate_context (slice s 0 (i-1)) (slice s (i+1) (v-1))) in
      let f=(generate_sub_contexts e 3) in 
      List.iter (fun x->ignore(self#addcontext x (List.nth s i))) f;
      self#addcontext e (List.nth s i);
    done;
end;;
