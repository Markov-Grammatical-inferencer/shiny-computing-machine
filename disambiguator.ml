open Slice;;

type textual={prec:int list;follow:int list};;
class disambiguator=
object(self)
  val mutable reverse_words=Hashtbl.create 1
  val mutable context=Hashtbl.create 2
  val mutable words=Hashtbl.create 2
  val mutable lastword=0
  method addword (w:string) =
    if not (Hashtbl.mem words w)then
      begin
        Hashtbl.add words w lastword;
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
        Hashtbl.add words w lastword;(*Add the context to the word*)
        lastword<-lastword+1;
        lastword-1(*return the context identifier*)
      end
    else
      let index=(Hashtbl.find context c) in(*
                             the context exists, therefore, return it's number*)
      let wc=(Hashtbl.find_all words w) in
      if not (List.mem index wc)then
        Hashtbl.add words w index;
      index

  method getwordcode (w:string)=
    (*
     * Return the last code, which
     * is the word as it was originally reported.
     * Should be constant time.
     *)
    let i=Hashtbl.find_all words w in
    let c=List.length i in
    List.nth i (c-1)

  method doeswordappearin (c:textual) (w:string)=
    let wc=(Hashtbl.find_all words w) in
    List.mem (Hashtbl.find context c) wc
  
  method print_info=
    print_string "Number of unique contexts ";
    print_int (Hashtbl.length context);
    print_endline "";
    print_int (Hashtbl.length words)

  method print_context (c:textual)=
    print_endline "These is the preceeding words";
    List.iter (fun x->print_endline (Hashtbl.find reverse_words x)) c.prec;
    print_endline "These is the following words";
    List.iter (fun x->print_endline (Hashtbl.find reverse_words x)) c.follow;
    ()
  method generate_context (p:string list) (f:string list)=
    (*Generates a context by the list of words used.*)
    let p=List.map self#getwordcode p in
    let c=List.map self#getwordcode f in
    {prec=p;follow=c}

  method process_into_context (s:string list)=
    print_endline "This is in process context";
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
	for i=0 to (v-3) do
	  let c=slice s i (i+3) in 
	  self#addcontext (self#generate_context [] c) (List.nth s i);
	  done;
    with Failure msg->
      print_endline "this is broken"
  end;;

