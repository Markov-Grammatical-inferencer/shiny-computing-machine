open Scm_util;;
class trie=
    object(self)
        val mutable pchildren:(int,trie)Hashtbl.t=Hashtbl.create 10
        val mutable fchildren:(int,trie)Hashtbl.t=Hashtbl.create 10
        val mutable probabilities:(int,(string,int)Hashtbl.t)Hashtbl.t=Hashtbl.create 10
        val mutable totals:(int,int)Hashtbl.t=Hashtbl.create 10
        method find_most_probable (prec:int list) (follow:int list) (w:int)
        (pos:string)=
            (**
             * Todo, write later*)
            ()
        method insert (prec:int list) (follow:int list) (w:int) (pos:string)=
            if ((List.length prec)>0) && ((List.length follow)>0) then
                ((
                    try
                        let pf=Hashtbl.find pchildren (List.hd prec) in
                        pf#insert (List.tl prec) [] w pos
                    with Not_found->
                        let pf=new trie in 
                        pf#insert (List.tl prec) [] w pos;
                        Hashtbl.add pchildren (List.hd prec) pf);
                (
                    try
                        let ff=Hashtbl.find fchildren (List.hd follow) in 
                        ff#insert [] (List.tl follow) w pos
                    with Not_found->
                        let ff=new trie in 
                        ff#insert [] (List.tl follow) w pos;
                        Hashtbl.add fchildren (List.hd follow) ff
                        ))
            else
                if (List.length prec)>0 then
                    (try
                        let pf=Hashtbl.find pchildren (List.hd prec) in
                        pf#insert (List.tl prec) [] w pos
                    with Not_found->
                        let pf = new trie in 
                        pf#insert (List.tl prec) [] w pos;
                        Hashtbl.add pchildren (List.hd prec) pf)
            else
                if (List.length follow)>0 then
                    (try
                        let pf=Hashtbl.find fchildren (List.hd follow) in
                        pf#insert [] (List.tl follow) w pos;
                    with Not_found->
                        let pf=new trie in
                        pf#insert [] (List.tl follow) w pos;
                            Hashtbl.add fchildren (List.hd follow) pf)
                else
                    if Hashtbl.mem probabilities w then
                        (let c=Hashtbl.find probabilities w in
                        if Hashtbl.mem c pos then
                            (let d=Hashtbl.find c pos in
                            Hashtbl.replace c pos (d+1)))
                else
                    let c=Hashtbl.create 1 in 
                    Hashtbl.replace c pos 1;
                            Hashtbl.replace probabilities w c
    end
