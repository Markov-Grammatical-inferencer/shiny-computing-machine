(*
Machine Learning algorithm based on Hidden Markov Models
Compute the probability of the appearence of a word given N adjacent words?
*)

(* GLR Parser *)

open Scm_util;;

type 'a tree = Node of ('a * ('a tree list));;

(*
#load "lablglut.cma";;
#load "lablgl.cma";;
*)

type 'a camera_data = { xpos : 'a; ypos : 'a; zpos : 'a; lrrot : 'a; udrot : 'a };;
(* input is a thunk so that it can initialize with a ref without aliasing issues *)
let make_camera_data init_func = {xpos = (init_func ()); ypos = (init_func ()); zpos = (init_func ()); lrrot = (init_func ()); udrot = (init_func ())};;

let apply_camera_data : (float ref camera_data -> unit) = fun cd ->
    GlMat.rotate ~angle: (rad_to_deg cd.udrot.contents) ~x: 1. ();
    GlMat.rotate ~angle: (rad_to_deg cd.lrrot.contents) ~y: 1. ();
    GlMat.translate ~x: (-.cd.xpos.contents) ~y: (-.cd.ypos.contents) ~z: (-.cd.zpos.contents) ();
    ();;

let key_press_handler_generator : ((int ref * int ref) -> (key:int -> x:int -> y:int -> unit)) = fun input_tuple ->
    fun ~key ~x ~y ->
        (*Printf.printf "Key %d, %d, %d\n%!" key x y;*)
        let (keyx, keyy) = input_tuple in
        if (key = Char.code 'a') then incr_i keyx (-1);
        if (key = Char.code 'd') then incr_i keyx 1;
        if (key = Char.code 'w') then incr_i keyy (-1);
        if (key = Char.code 's') then incr_i keyy 1;
        ();;

let move_delta = 0.1;;
let rotate_delta = 0.1;;

let move_in_direction dir cd =
    incr_f cd.xpos (move_delta *. (cos dir));
    incr_f cd.zpos (move_delta *. (sin dir));
    ();;

let camera_key_press_handler : ((float ref camera_data) -> (key:int -> x:int -> y:int -> unit)) = fun cd ->
    fun ~key ~x ~y ->
        if (key = Char.code 'a') then move_in_direction (cd.lrrot.contents +. (tau *. 0.50)) cd;
        if (key = Char.code 'd') then move_in_direction (cd.lrrot.contents +. (tau *. 0.00)) cd;
        if (key = Char.code 'w') then move_in_direction (cd.lrrot.contents +. (tau *. 0.25)) cd;
        if (key = Char.code 's') then move_in_direction (cd.lrrot.contents +. (tau *. 0.75)) cd;
        if (key = Char.code 'q') then incr_f cd.ypos (-1. *. move_delta);
        if (key = Char.code 'e') then incr_f cd.ypos (1. *. move_delta);

        if (key = Char.code 'i') then incr_f cd.udrot (1. *. rotate_delta);
        if (key = Char.code 'k') then incr_f cd.udrot (-1. *. rotate_delta);
        inplace (clamp ((-.tau) /. 4.) (tau /. 4.)) cd.udrot;
        if (key = Char.code 'j') then incr_f cd.lrrot (1. *. rotate_delta);
        if (key = Char.code 'l') then incr_f cd.lrrot (-1. *. rotate_delta);
        (* Printf.printf "%f,%f,%f,%f,%f\n%!" cd.xpos.contents cd.ypos.contents cd.zpos.contents cd.lrrot.contents cd.udrot.contents; *)
        ();;


let show_parse_tree = fun tr ->
    ignore( Glut.init Sys.argv );
    Glut.initDisplayMode ~double_buffer:true ();
    ignore (Glut.createWindow ~title:"Parse Tree");
    let cd = make_camera_data (fun () -> ref 0.) in
    let render () =
        GlClear.clear [ `color ];
        GlMat.load_identity ();
        apply_camera_data cd;
        (*GlDraw.begins `triangles;
        List.iter GlDraw.vertex2 [-1., -1.; 0., 1.; 1., -1.];
        GlDraw.ends ();*)
        Glut.wireTeapot 0.75;
        Glut.swapBuffers () in
    GlMat.mode `modelview;
    Glut.displayFunc ~cb:render;
    Glut.keyboardFunc (camera_key_press_handler cd);
    Glut.idleFunc ~cb:(Some Glut.postRedisplay);
        Glut.mainLoop ();;

show_parse_tree (Node("S",[Node("NP",[Node("NN",[Node("I",[])])]);Node("VP",[Node("VBZ",[Node("am",[])])])]));;
(* Node("S",[Node("NP",[Node("NN",[Node("I",[])])]);Node("VP",[Node("VBZ",[Node("am",[])])])]);;*)
