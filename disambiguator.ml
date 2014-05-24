type textual={ prec: int list;follow: int list };;
(*
prec is the preceeding words, as given by integer identifiers(this should work as
there are not enough english words to overflow).

follow is a list of integer identifiers for words that have in the past followed it.
*)
let rec print_context (c:textual)=(*Debugging function*)
  if (c.prec!=[])then
    begin
      print_endline "These are the preceeding words";
      List.iter print_int c.prec;
    end;
  print_endline "These are the following words";
  List.iter print_int c.follow;;
class disambiguation=
object(self)
  val mutable context=Hashtbl.create 2
  val mutable words=Hashtbl.create 2
  val mutable lastword=0
  method addword (w:string) =
    if not (Hashtbl.mem words w)then
      begin
        Hashtbl.add words w lastword;
        lastword<-lastword+1;
        lastword-1
      end
    else
      Hashtbl.find words w
  method addcontext (c:textual) (w:string)=
    assert (Hashtbl.mem words w);
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

  method getWordCode (w:string)=
    (*
     * Return the last code, which
     * is the word as it was originally reported
     *)
    let i=Hashtbl.find_all words w in
    let c=List.length i in
    List.nth i c

  method doesWordAppearIn (c:textual) (w:string)=
    let wc=(Hashtbl.find_all words w) in
    List.mem (Hashtbl.find context c) wc

end;;
let i=new disambiguation;;
i#addword "hello"
