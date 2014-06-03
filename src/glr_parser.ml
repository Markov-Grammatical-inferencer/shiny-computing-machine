(*
Machine Learning algorithm based on Hidden Markov Models
Compute the probability of the appearence of a word given N adjacent words?
*)

(* GLR Parser *)

open Scm_util;;
open Font_wrapper;;

type 'a tree = Node of ('a * ('a tree list));;

(*
#load "lablglut.cma";;
#load "lablgl.cma";;
*)

type 'a camera_data = { xpos : 'a; ypos : 'a; zpos : 'a; lrrot : 'a; udrot : 'a };;
(* input is a thunk so that it can initialize with a ref without aliasing issues *)
let make_camera_data init_func = {xpos = (init_func ()); ypos = (init_func ()); zpos = (init_func ()); lrrot = (init_func ()); udrot = (init_func ())};;

let apply_camera_data : (float ref camera_data -> unit) = fun cd ->
    GlMat.rotate ~angle: (rad_to_deg cd.udrot.contents) ~x: (-.1.) ();
    GlMat.rotate ~angle: (rad_to_deg cd.lrrot.contents) ~y: (-.1.) ();
    GlMat.translate ~x: (-.cd.xpos.contents) ~y: (-.cd.ypos.contents) ~z: (cd.zpos.contents) ();
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

(* successful combination of openGL calls to enable textures translated from a previous project of mine: https://github.com/aweinstock314/correspondence_problem_demo/ *)
(*
glBindTexture(GL_TEXTURE_2D,texture_id);
glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT);
glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_REPEAT);
glTexParameterf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_REPLACE);
glTexImage2D(GL_TEXTURE_2D,0,4,texture_width,texture_height,0,GL_RGBA,GL_UNSIGNED_BYTE,data);

glClearColor(0,0,0,1);
glShadeModel(GL_SMOOTH);
glClearDepth(1.0f);
glEnable(GL_DEPTH_TEST);
glDepthFunc(GL_LEQUAL);
glEnable(GL_ALPHA_TEST);
glEnable(GL_BLEND);
glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
*)
let apply_texture img =
    let tex_id = GlTex.gen_texture () in
    GlTex.bind_texture ~target:`texture_2d tex_id;
    let par = GlTex.parameter ~target:`texture_2d in
    par (`min_filter `nearest);
    par (`mag_filter `nearest);
    par (`wrap_s `repeat);
    par (`wrap_t `repeat);
    GlTex.image2d ~proxy: false ~level: 0 ~internal:3 ~border:false img;;

let make_node_drawer str =
    let module Imgmake = Image_maker(GlTexImage) in
    let (w, h, x, y) = Imgmake.get_size_and_pos freemono_face str in
    let modifiable_img = GlTexImage.create w h in
    GlTexImage.fill_image modifiable_img (Graphics.rgb 0x40 0x40 0x40);
    Imgmake.draw_string_on_image modifiable_img freemono_face draw_rainbow str;
    let img = glpix_of_glteximage modifiable_img in
    (fun w h x y z ->
        apply_texture img;
        GlDraw.begins `quads;
        List.iter (fun (x,y,z,u,v) ->
            GlTex.coord2 (u,v);
            GlDraw.vertex3 (x,y,z);
        ) [(x,y,z,0.,1.);(x,y+.h,z,0.,0.);(x+.w,y+.h,z,1.,0.);(x+.w,y,z,1.,1.)];
        GlDraw.ends ();
    );;

let show_parse_tree = fun tr ->
    ignore( Glut.init Sys.argv );
    Glut.initDisplayMode ~double_buffer:true ();
    ignore (Glut.createWindow ~title:"Parse Tree");
    let cd = make_camera_data (fun () -> ref 0.) in
    cd.xpos := 2.5; cd.ypos := 0.5; cd.zpos := -1.;
    let f = make_node_drawer "Hello, world!" in
    let g = make_node_drawer "This string may or may not be, in fact, a string (but it is)." in
    let render () =
        GlClear.color ~alpha: 1. (0.,0.,0.);
        GlDraw.shade_model `smooth;
        GlClear.depth 1.;
        Gl.enable `depth_test;
        GlFunc.depth_func `lequal;
        Gl.enable `alpha_test;
        Gl.enable `blend;
        GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
        GlClear.clear [`color;`depth];
        Gl.enable `texture_2d;
        GlClear.clear [ `color ];
        GlMat.mode `projection;
        GlMat.load_identity ();
        GlMat.frustum ~x: (-1.,1.) ~y: (-1.,1.) ~z: (0.5,100.);
        GlMat.mode `modelview;
        GlMat.load_identity ();
        apply_camera_data cd;
        f 5.0 1. 0. 0. 0.;
        g 10.0 2. 0. 3. 0.;
        (* Glut.wireTeapot 0.50; *)
        Glut.swapBuffers () in
    GlMat.mode `modelview;
    Glut.displayFunc ~cb:render;
    Glut.keyboardFunc (camera_key_press_handler cd);
    Glut.idleFunc ~cb:(Some Glut.postRedisplay);
        Glut.mainLoop ();;

show_parse_tree (Node("S",[Node("NP",[Node("NN",[Node("I",[])])]);Node("VP",[Node("VBZ",[Node("am",[])])])]));;
(* Node("S",[Node("NP",[Node("NN",[Node("I",[])])]);Node("VP",[Node("VBZ",[Node("am",[])])])]);;*)
