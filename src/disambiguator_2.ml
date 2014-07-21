module IntSet=Set.Make (struct 
                         let compare=Pervasives.compare 
                         type t=int
                       end);;

                
class context_word (p:string)=
object(self)
  val pos=p
  val mutable w:IntSet.t=IntSet.empty
  method add_word (s:int)=
    (**Add a word to the set and alter the set.
     *)
    w<-IntSet.add s w
  method get_words ()=
    (**
     * Return the set of words in this context
     *)
    w
end;;
class word_context_manager=
  (**
   * For each word a place, and for each place a word
   *)
object(self)
  val mutable plcmnts: context_word ref list=[]
  method context_number () = 
    List.length plcmnts
  method get_contexts ()=
    plcmnts
  method add_context (c:context_word ref)=
    plcmnts<-plcmnts@[c]
end;;
class disambiguator=
object(self)
  val mutable interned_strings:(string,int) Hashtbl.t=Hashtbl.create 1(** Refers to the interned strings which are used to allow faster comparison as well as smaller memory footprints.
                                                                       *)
  val mutable highest_number:int=0(** The highest number for the string which has been interned*)
  val mutable master_manager:(int,word_context_manager) Hashtbl.t =Hashtbl.create 1
  val mutable contexts:((string list)*(string list),(string,context_word)Hashtbl.t ) Hashtbl.t=Hashtbl.create 0
  method add_word (s:string)=
    Hashtbl.add interned_strings s highest_number ;
    Hashtbl.add master_manager highest_number (new word_context_manager);
    highest_number<-highest_number+1
  method resolve_intern (s:string)=
    if Hashtbl.mem interned_strings s then 
      Hashtbl.find interned_strings s
    else
     0-1
  method add_context (prec:string list) (s:string) (p:string) (follow:string list)=
    let wid=(self#resolve_intern s) in 
    let f= ref new word_context_manager in 
    if Hashtbl.mem master_manager wid then 
      f:=Hashtbl.find master_manager wid
    else
      Hashtbl.add master_manager wid !f;
    if Hashtbl.mem contexts (prec,follow) then 
      begin
        let g=Hashtbl.find contexts (prec,follow) in 
        let h = Hashtbl.find g p in 
        (!f)#add_context (ref h);
        h#add_word wid
      end
    else
      let j=Hashtbl.create 1 in 
      let h=new context_word p in
      (!f)#add_context (ref h);
      Hashtbl.add j p h;
      Hashtbl.add contexts (prec,follow) j

    method intersection ()=
        let 
end;;
