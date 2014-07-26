module Hashtbl=
    struct
        include Hashtbl
        let find_or_init (tbl:('a,'b) t) (key:'a) (v:'b)=
            if mem tbl key then
                find tbl key
            else
                add tbl key v;
                find tbl key

        let maximize (tbl:(string,int)Hashtbl.t)=
            (**
             * Return the largest key value pair in the Hashtbl.
             **)
            let mx=ref ("",0) in 
            Hashtbl.iter (fun x y->
                match !mx with
                (a,b)->if y<b then
                    mx:=(x,y)) tbl;
            !mx
    end;;
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
end;;
