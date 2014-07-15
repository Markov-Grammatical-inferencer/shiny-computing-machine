open Scm_util;;
open Font_wrapper;;
open Opengl_visualization;;
open Glr_parser;;

let rec make_tree_drawer tr =
    let Node(node,subtrees) = tr in
    let (draw_cur_node, w, h) = make_textrect_drawer freemono_face draw_rainbow node in
    let (draw_subtree_list, widths_and_heights) = List.unzip (List.map make_tree_drawer subtrees) in
    let (subtree_widths, subtree_heights) = List.unzip widths_and_heights in
    (* let total_subwidth = List.fold_left (+.) 0. subtree_widths in *)
    let max_subwidth = List.fold_left (max) 0. subtree_widths in
    let max_subheight = List.fold_left (max) 0. subtree_heights in
    (fun x y z () ->
        let x_offset = ref 0. in
        draw_cur_node (x -. (w/.2.)) y z ();
        List.iter2 (fun draw_subtree (subtree_width, subtree_height) ->
            let (x2,y2) = ((x +. !x_offset),(y -. (1.+.max_subheight))) in
            draw_rectangular_line 0.25 (x,y,z) (x2,y2,z) (1.,1.,1.,1.);
            draw_subtree x2 y2 z ();
            (* Printf.printf "%s\t%f\t%f\t\t%f\t%f\t\t%f\t%f\n%!" node subtree_width subtree_height !x_offset 0. w h; *)
            x_offset +.= max_subwidth;
        ) draw_subtree_list (List.zip subtree_widths subtree_heights);
    ), ((1.+.(max w max_subwidth)), h);;

let show_parse_tree tr =
    let (draw_tree,_) = make_tree_drawer tr in
        with_opengl_context "Parse Tree" (draw_tree 0. 0. 0.);;

(*
show_parse_tree
(Node("S",
    [Node("NP",
        [Node("NN",
            [Node("I",[]);
            Node(".",[])]
        )]
    );
    Node("VP",
        [Node("VBZ",
            [Node("am",[])]
        )]
    )]
));;
*)

module P = Make(StringArray);;
open P;;

(* let p = make_parser simple_imperative_grammar;; *)
let p = make_parser simple_operator_grammar;;
let cdr_argv = Array.of_list (List.tl (Array.to_list Sys.argv));;
(* let thing_to_parse = simple_operator_example;; *)
let thing_to_parse = cdr_argv;;
show_parse_tree (convert_tree (compose string_of_symbol StringArray.string_of) (List.hd (p thing_to_parse)));;
