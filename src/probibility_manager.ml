module Hashtbl=
struct
        include Hashtbl
        let find_or_init (tbl:('a,'b) t) (key:'a) (v:'b)=
            if mem tbl key then
                find tbl key
            else
                add tbl key v;
                find tbl key

        let value_accumulation_generic (tbl:(string,int)Hashtbl.t)
        (f:(string*int->string*int->bool)) (dval:int)=
            let mx=ref ("",dval) in
            Hashtbl.iter (fun x y->
                if (f (!mx) (x,y)) then
                    mx:=(x, y)) tbl;
            !mx

        let maximize (tbl:(string,int)Hashtbl.t)=
            (**
             * Return the largest key value pair in the Hashtbl.
             **)
            value_accumulation_generic tbl (fun x y->
                let a,b=x in 
                let c,d=y in 
                b<d) 0

        let minimize (tbl:(string,int) Hashtbl.t)=
            (**
             * Return the smallest key value pair in the Hashtbl.
             **)
            value_accumulation_generic tbl (fun x y->
                let a,b=x in 
                let c,d=y in 
                b>d) 10000
        let to_ratios (tbl:(string,int)Hashtbl.t) (total:int)=
            let c=float_of_int total in 
            Hashtbl.map (fun x y->
                let d=float_of_int y in 
                d/c) tbl
end;;
(** TODO rewrite to reflect structure as a Trie, which is required in order to
 * allow probibilities to be correctly calculated given some events but not
 * {b all} of them*)
class probability_manager=
object(self) 
        val mutable ptable:(int list*int*int
        list,(string,int)Hashtbl.t)Hashtbl.t=Hashtbl.create 3
        val mutable total:(int list*int*int list,
        int)Hashtbl.t=Hashtbl.create 1
        method increment_counter (b:(int list*int*int list))=
            if Hashtbl.mem total b then
                let q=Hashtbl.find total b in 
                Hashtbl.replace total b (q+1)
            else
                Hashtbl.replace total b 1
        method add_exposure (prec:int list) (w:int) (f:int list) (s:string)=
            if Hashtbl.mem ptable (prec,w,f) then
                begin
                    let pc=Hashtbl.find ptable (prec,w,f) in
                    if Hashtbl.mem pc s then
                        (let ch=Hashtbl.find pc s in 
                        Hashtbl.replace pc s (ch+1);
                        self#increment_counter (prec,w,f))
                    else
                        Hashtbl.replace pc s 1;
                        self#increment_counter (prec,w,f)
                end
            else
                Hashtbl.replace ptable (prec,w,f) (Hashtbl.create 1);
                Hashtbl.replace (Hashtbl.find ptable (prec,w,f)) s 1

    method most_probable (c:(int list*int*int list))=
        if Hashtbl.mem ptable c then
            Hashtbl.maximize (Hashtbl.find ptable c)
        else
            ("",0)

    method least_probable (c:(int list*int*int list))=
        if Hashtbl.mem ptable c then
            Hashtbl.minimize (Hashtbl.find ptable c )
        else
            ("",0)

    (*
     * Commented out because I don't know how to do this properly enough to not
     * break everything else.
     * method roll_a_die (c:(int list*int*int list))=
        let dc=ref 0.0 in
        if Hashtbl.mem ptable c then
            begin
                
                while dc>0.0 do
                    
                done
            end;
        else
            0*)
end;;


