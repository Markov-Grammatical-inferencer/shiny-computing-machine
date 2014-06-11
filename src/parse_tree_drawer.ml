open Scm_util;;
open Font_wrapper;;
open Opengl_visualization;;
open Glr_parser;;

let rec make_tree_drawer tr =
    let Node(node,subtrees) = tr in
    let (draw_cur_node, w, h) = make_textrect_drawer freemono_face draw_rainbow node in
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

let show_parse_tree tr =
    let (draw_tree,_) = make_tree_drawer tr in
        with_opengl_context "Parse Tree" (draw_tree 0. 0. 0.);;

show_parse_tree (Node("S",[Node("NP",[Node("NN",[Node("I",[])])]);Node("VP",[Node("VBZ",[Node("am",[])])])]));;
