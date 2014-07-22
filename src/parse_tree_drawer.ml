open Scm_util;;
open Font_wrapper;;
open Opengl_visualization;;
open Glr_parser;;


(* let drawer_of_stringarray = (compose (make_textrect_drawer freemono_face draw_rainbow) (compose sexpr_string_of_symbol StringArray.string_of));; *)
(* let drawertrees = List.map (convert_tree drawer_of_stringarray) trees;; *)

let rec make_tree_drawer tr =
    let Node(node,subtrees) = tr in
    let (draw_cur_node, w, h) = make_textrect_drawer freemono_face draw_yellow node in
    let (draw_subtree_list, widths_and_heights) = List.unzip (List.map make_tree_drawer subtrees) in
    let (subtree_widths, subtree_heights) = List.unzip widths_and_heights in
    (* let total_subwidth = List.fold_left (+.) 0. subtree_widths in *)
    let num_subtrees = float_of_int $ List.length draw_subtree_list in
    let total_subwidth = List.fold_left (+.) 0. subtree_widths in
    let total_subheight = List.fold_left (+.) 0. subtree_heights in
    (fun x y z () ->
        let x_index = ref 1. in
        (* let x_offset = ref 0. in *)
        let x_offset () = ((!x_index /. (num_subtrees +. 1.)) -. (num_subtrees /. 2.)) *. total_subwidth in
        draw_cur_node (x -. (w/.2.)) y z ();
        List.iter2 (fun draw_subtree (subtree_width, subtree_height) ->
            (* let (x2,y2) = ((x +. !x_offset),(y -. (1.+.total_subheight))) in *)
            let (x2,y2) = ((x +. !!x_offset),(y -. (1.+.total_subheight))) in
            draw_rectangular_line 0.25 (x,y,z) (x2,y2,z) (1.,1.,1.,1.);
            draw_subtree x2 y2 z ();
            (* Printf.printf "%s\t%f\t%f\t\t%f\t%f\t\t%f\t%f\n%!" node subtree_width subtree_height !x_offset 0. w h; *)
            (* x_offset +.= total_subwidth; *)
            x_index +.= 1.;
        ) draw_subtree_list (List.zip subtree_widths subtree_heights);
    ), ((1.+.(max w total_subwidth)), h);;

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
(* let thing_to_parse = simple_operator_example;; *)
(* let cdr_argv = Array.of_list (List.tl (Array.to_list Sys.argv));; *)
(* let thing_to_parse = cdr_argv;; *)
let thing_to_parse = Array.of_list (tokenize_via_whitespace (Array.get Sys.argv 1));;
let parse_tree = (convert_tree (compose sexpr_string_of_symbol StringArray.string_of) (List.hd (p thing_to_parse)));;
Printf.printf "%s\n%!" (sexpr_of_string_tree parse_tree);;
show_parse_tree parse_tree;;
