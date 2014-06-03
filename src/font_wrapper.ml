open Scm_util;;
(* let ftchar_of_char = compose Freetype.char_index_of_int Char.code;; *)
let ftchar_of_char face = compose (Freetype.get_char_index face) Char.code;;

let ftstring_of_string face = string_map (ftchar_of_char face);;

let freetype_init_token = Freetype.init ();;
let (freemono_face, freemono_face_info) = Freetype.new_face freetype_init_token "/usr/share/fonts/truetype/freefont/FreeMono.ttf" 0;;
Freetype.set_pixel_sizes freemono_face 32 48;;

(* Must be passed to Fttext.Make
module type T =
  sig
    type t
    type elt
    val create : int -> int -> t
    val destroy : t -> unit
    val get : t -> int -> int -> elt
    val set : t -> int -> int -> elt -> unit
    val unsafe_get : t -> int -> int -> elt
    val unsafe_set : t -> int -> int -> elt -> unit
  end
*)
(* module IntArrayImage = 
struct
    type t = int array * int (* use a tuple to tag the width, needed for indexing *)
    type elt = int
    let create w h = ((Array.create (w*h) 0),w)
    let destroy arr = ()
    let get (arr,w) x y = Array.get arr ((y*w)+x)
    let set (arr,w) x y value = Array.set arr ((y*w)+x) value
    let unsafe_get = get
    let unsafe_set = set
end;;*)
module GraphicImage =
struct
    type t = Graphics.color array array
    type elt = Graphics.color
    let create w h = Array.create_matrix h w (Graphics.rgb 0xFF 0xFF 0xFF)
    let destroy arr = ()
    let get arr x y = Array.get (Array.get arr y) x
    let set arr x y value = Array.set (Array.get arr y) x value
    let unsafe_get = get
    let unsafe_set = set
end;;

module GlTexImage =
struct
    type t = ([`ubyte] Raw.t) * int * int
    type elt = Graphics.color
    let create w h = (Raw.create `ubyte (3 * w * h), w, h)
    let destroy arr = ()
    let get_tuple (arr,w,h) x y = let g offset = Raw.get arr (offset + (3 * ((w * y) + x))) in (g 0, g 1, g 2)
    let set_tuple (arr,w,h) x y (v1, v2, v3) = let s offset v = Raw.set arr (offset + (3 * ((w * y) + x))) v in s 0 v1; s 1 v2; s 2 v3
    let get awh x y = let (r,g,b) = get_tuple awh x y in Graphics.rgb r g b;;
    let set awh x y v = let v_tuple = int3_of_rgb v in set_tuple awh x y v_tuple;;
    let unsafe_get = get
    let unsafe_set = set
end;;

let glpix_of_glteximage (img,w,h) = GlPix.of_raw img ~format: `rgb ~width: w ~height: h;;

(* Fttext.drawer is ('a -> int -> 'a), and it's applied elementwise on images (according to the output of Fttext.Make) (and according to putpixel in fttext.ml) *)
let draw_red x level = Graphics.rgb level 0 0;;
let draw_rainbow x level =
    let (r,g,b) = (Random.int 255,Random.int 255, Random.int 255) in
    if (level > 32) then Graphics.rgb r g b else x;;

module Image_maker = functor (T : Fttext.T) ->
struct
    module Fttext_instance = Fttext.Make(T)
    let image_of_string face draw_func str = 
        let glyphstring = (ftstring_of_string face str) in
        let (x1, y1, x2, y2) = Fttext.size_of_glyphs face glyphstring in
        (* "dim" adapted from libcamlimages-ocaml-doc/examples/ttfimg *)
        let dim fudge_factor d1 d2 = truncate d2 - truncate d1 + fudge_factor * 2 in 
        let (w, h) = (dim 1 x1 x2, dim 0 y1 y2) in
        let img_buf = T.create w h in
        Fttext_instance.draw_glyphs face draw_func img_buf (-int_of_float x1) (h+((-1)+int_of_float y1)) glyphstring;
        img_buf
end;;

let draw_string face draw_func str x y =
    let module M = Image_maker(GraphicImage) in
    let img_buf = M.image_of_string face draw_func str in
    Graphics.draw_image (Graphics.make_image img_buf) x y;;

(*
#load "graphics.cma";;
#load "camlimages_core.cma";;
#load "camlimages_freetype.cma";;
#load "lablgl.cma";;
#load "scm_util.cmo";;
#load "font_wrapper.cmo";;
open Font_wrapper;;
*)

(* example main *)
(*
try
    Graphics.open_graph "";
    (* draw_string freemono_face draw_rainbow "\"Is a sentence fragment\" is a sentence fragment." 0 0; *) (* ends up cut off with default window size, but is otherwise fine *)
    draw_string freemono_face draw_rainbow "Hello, world!" 0 0;
    ignore(Graphics.wait_next_event []);
    ()
with _ -> ();;
*)
