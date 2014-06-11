open Scm_util;;
open Font_wrapper;;
open Opengl_visualization;;
open Glr_parser;;

let rec make_tree_drawer tr =
    let Node(node,subtrees) = tr in
    let (draw_cur_node, w, h) = make_textrect_drawer freemono_face draw_rainbow node in
    let (draw_subtree_list, widths_and_heights) = List.unzip (List.map make_tree_drawer subtrees) in
    let (subtree_widths, subtree_heights) = List.unzip widths_and_heights in
    let max_subwidth = List.fold_left (max) 0. subtree_widths in
    let max_subheight = List.fold_left (max) 0. subtree_heights in
    (fun x y z () ->
        let x_offset = ref 0. in
        draw_cur_node x y z ();
        List.iter2 (fun draw_subtree (subtree_width, subtree_height) ->
            draw_subtree (x +. !x_offset) (y -. (1.+.max_subheight)) z ();
            (* Printf.printf "%s\t%f\t%f\t\t%f\t%f\t\t%f\t%f\n%!" node subtree_width subtree_height !x_offset 0. w h; *)
            x_offset +.= subtree_width;
        ) draw_subtree_list (List.zip subtree_widths subtree_heights);
    ), ((1.+.(max w max_subwidth)), h);;

let show_parse_tree tr =
    let (draw_tree,_) = make_tree_drawer tr in
        with_opengl_context "Parse Tree" (draw_tree 0. 0. 0.);;

show_parse_tree (Node("S",[Node("NP",[Node("NN",[Node("I",[]);Node("inserting a large node here to test tree drawing",[])])]);Node("VP",[Node("VBZ",[Node("am",[])])])]));;
