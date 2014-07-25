module IntSet=Set.Make (struct 
                         let compare=Pervasives.compare 
                         type t=int
                       end);;

module List=
    struct 
        include List
        let rec range (l: 'a list) (start:int) (last:int)=
            let c=ref [] in
            if start = 0 then
                for i=0 to start do
                    c:=(!c)@[(List.nth l i)]
                done
            else
                range (List.tl l) (start-1) last
    end;;
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
  val mutable master_manager:(int,word_context_manager) Hashtbl.t
  =Hashtbl.create 1 (** The manager collection which remembers which contexts
  go where and where things otherwise happen*)
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
    method exists_in_context (prec:string list) (s:string) (p:string)
    (follow:string list)=
        if not (Hashtbl.mem contexts (prec,follow))then
            false
        else
            let g=Hashtbl.find contexts (prec,follow) in
            let h=Hashtbl.find g p in 
            IntSet.mem (self#resolve_intern s) (h#get_words ())
            
    method intersection ()=
        (**
         * Iterate through each word and attempt to find a word which is
         * commonly shared amongst contexts which a word may appear in.
         * *)
        Hashtbl.iter (fun key word_manager->
            let c=word_manager#get_contexts () in 
            let h=(List.length c)/2 in 
            let j=Hashtbl.create 10 in 
            let qu=ref [] in 
            List.iter (fun context->
                let word_set=IntSet.remove key ((!context)#get_words ()) in 
                (* Remove the word from the context in order to ensure that it
                 * does not result in the word being added to its own
                 * contexts*)
                IntSet.iter (fun x->
                    if not (Hashtbl.mem j x) then 
                        Hashtbl.add j x 1(*Add an initial count of one*)
                    else
                        Hashtbl.replace j x (1+(Hashtbl.find j x))(*Else
                            increment the existing*)
            ) word_set) c;
            Hashtbl.iter (fun word num_occur->
                if num_occur>h then
                    qu:=(!qu)@[word]
                else
                    ()) j;
            List.iter (fun x->
                List.iter (fun y->(** Add the word to each context*)
                    (!x)#add_word y) !qu) c
            ) master_manager
   (* method fit_sentence_into_context (s:string list) probability_manager=
        let symbol_list=List.ite probability_manager#aggregate_prediction s in 
        (* attempt to produce a list of symbols based on probabilities
         * calculated by the probability manager
         *
         * Which does not exist*)
        List.for_all (fun x->x) (List.mapi (fun index item->
            self#exists_in_context (List.range symbol_list 0 (index-1)) item
            (List.range symbol_list (index+1) ((List.length
            symbol_list)-(index+1)))))*)

end;;
