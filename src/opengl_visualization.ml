open Scm_util;;
open Font_wrapper;;
open Glr_parser;;

(*
#load "graphics.cma";;
#load "lablgl.cma";;
#load "lablglut.cma";;
#load "camlimages_core.cma";;
#load "camlimages_freetype.cma";;
#load "scm_util.cmo";;
#load "font_wrapper.cmo";;
*)

type 'a camera_data = { xpos : 'a; ypos : 'a; zpos : 'a; lrrot : 'a; udrot : 'a };;
(* input is a thunk so that it can initialize with a ref without aliasing issues *)
let make_camera_data init_func = {xpos = (init_func ()); ypos = (init_func ()); zpos = (init_func ()); lrrot = (init_func ()); udrot = (init_func ())};;

let apply_camera_data : (float ref camera_data -> unit) = fun cd ->
    GlMat.rotate ~angle: (rad_to_deg cd.udrot.contents) ~x: (-.1.) ();
    GlMat.rotate ~angle: (rad_to_deg cd.lrrot.contents) ~y: (-.1.) ();
    GlMat.translate ~x: (-.cd.xpos.contents) ~y: (-.cd.ypos.contents) ~z: (cd.zpos.contents) ();
    ();;

let move_delta = 1.0;;
let rotate_delta = 0.1;;

let move_in_direction dir cd =
    cd.xpos +.= (move_delta *. (cos dir));
    cd.zpos +.= (move_delta *. (sin dir));
    ();;

let incdec_by_key converter amount varref deckey inckey key =
    if (converter deckey = key) then varref +.= (-.amount);
    if (converter inckey = key) then varref +.= amount;;

let camera_key_press_handler : ((float ref camera_data) -> (key:int -> x:int -> y:int -> unit)) = fun cd ->
    fun ~key ~x ~y ->
        if (key = Char.code 'a') then move_in_direction (cd.lrrot.contents +. (tau *. 0.50)) cd;
        if (key = Char.code 'd') then move_in_direction (cd.lrrot.contents +. (tau *. 0.00)) cd;
        if (key = Char.code 'w') then move_in_direction (cd.lrrot.contents +. (tau *. 0.25)) cd;
        if (key = Char.code 's') then move_in_direction (cd.lrrot.contents +. (tau *. 0.75)) cd;
        let f = incdec_by_key Char.code in
        f move_delta cd.ypos 'q' 'e' key;
        f rotate_delta cd.udrot 'k' 'i' key;
        inplace (clamp ((-.tau) /. 4.) (tau /. 4.)) cd.udrot;
        f rotate_delta cd.lrrot 'l' 'j' key;;

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
let setup_texture img =
    let tex_id = GlTex.gen_texture () in
    fun () ->
    GlTex.bind_texture ~target:`texture_2d tex_id;
    let par = GlTex.parameter ~target:`texture_2d in
    par (`min_filter `nearest);
    par (`mag_filter `nearest);
    par (`wrap_s `clamp);
    par (`wrap_t `clamp);
    GlTex.image2d ~proxy: false ~level: 0 ~internal:3 ~border:false img;;

let make_node_drawer str =
    let module Imgmake = Image_maker(GlTexImage) in
    let (w, h, x, y) = Imgmake.get_size_and_pos freemono_face str in
    let (w, h) = (next_pow2 w, next_pow2 h) in
    let modifiable_img = GlTexImage.create w h in
    GlTexImage.fill_image modifiable_img (Graphics.rgb 0x40 0x40 0x40);
    Imgmake.draw_string_on_image modifiable_img freemono_face draw_rainbow str;
    let img = glpix_of_glteximage modifiable_img in
    let apply_texture = setup_texture img in
    let scale_factor = 10. in
    let (w_f, h_f) = (float_of_int w) /. scale_factor, (float_of_int h) /. scale_factor in
    (fun w h x y z () ->
        apply_texture ();
        GlDraw.begins `quads;
        List.iter (fun (x,y,z,u,v) ->
            GlTex.coord2 (u,v);
            GlDraw.vertex3 (x,y,z);
        ) [(x,y,z,0.,1.);(x,y+.h,z,0.,0.);(x+.w,y+.h,z,1.,0.);(x+.w,y,z,1.,1.)];
        GlDraw.ends ();
    ) w_f h_f, w_f, h_f;;

let rec make_tree_drawer tr =
    let Node(node,subtrees) = tr in
    let (draw_cur_node, w, h) = make_node_drawer node in
    let (draw_subtree_list, subtree_widths) = List.unzip (List.map make_tree_drawer subtrees) in
    let total_subwidth = List.fold_left (max) 0. subtree_widths in
    (fun x y z () ->
        let offset = ref 0. in
        draw_cur_node x y z ();
        List.iter2 (fun draw_subtree subtree_width ->
            draw_subtree (x +. !offset) (y -. (2. *. h)) z ();
            (* Printf.printf "%f %f %f\n%!" subtree_width !offset w; *)
            offset +.= subtree_width
        ) draw_subtree_list subtree_widths;
    ), (1.+.(max w total_subwidth));;

let with_opengl_context drawfn =
    ignore( Glut.init Sys.argv );
    Glut.initDisplayMode ~double_buffer:true ();
    ignore (Glut.createWindow ~title:"Parse Tree");
    let cd = make_camera_data (fun () -> ref 0.) in
    cd.xpos := 2.5; cd.ypos := 0.5; cd.zpos := -1.;
    let render fn () =
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
        fn ();
        Glut.swapBuffers () in
    GlMat.mode `modelview;
    Glut.displayFunc ~cb:(render drawfn);
    Glut.keyboardFunc (camera_key_press_handler cd);
    Glut.idleFunc ~cb:(Some Glut.postRedisplay);
        Glut.mainLoop ();;


let show_parse_tree tr =
    let (draw_tree,_) = make_tree_drawer tr in
        with_opengl_context (draw_tree 0. 0. 0.);;

show_parse_tree (Node("S",[Node("NP",[Node("NN",[Node("I",[])])]);Node("VP",[Node("VBZ",[Node("am",[])])])]));;
(* with_opengl_context (fun () -> Glut.wireTeapot 1.);; *)
