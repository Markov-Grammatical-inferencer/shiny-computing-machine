let compose f g x = f (g x);;
(* let ftchar_of_char = compose Freetype.char_index_of_int Char.code;; *)
let ftchar_of_char face = compose (Freetype.get_char_index face) Char.code;;

let string_map f str =
    let arr = Array.create (String.length str) (f 'a') in
    let index = ref 0 in
    String.iter (fun ch ->
        Array.set arr index.contents (f ch);
        index.contents <- index.contents + 1
        ) str;
    arr;;

let string_to_ftstring face = string_map (ftchar_of_char face);;

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
    let create w h = Array.create_matrix h w (Graphics.rgb 0 0 0)
    let destroy arr = ()
    let get arr x y = Array.get (Array.get arr y) x
    let set arr x y value = Array.set (Array.get arr y) x value
    let unsafe_get = get
    let unsafe_set = set
end;;


module Graphic_fttext = Fttext.Make(GraphicImage);;

(* Fttext.drawer is ('a -> int -> 'a), and it's applied elementwise on images (according to the output of Fttext.Make) (and according to putpixel in fttext.ml) *)
let draw_red x level = Graphics.rgb level 0 0;;
let draw_rainbow x level =
    let (r,g,b) = (Random.int 255,Random.int 255, Random.int 255) in
    if (level > 32) then Graphics.rgb r g b else Graphics.rgb 0 0 0;;
(*
let sample_image = GraphicImage.create 400 100;;
Fttext_instance.draw_glyphs face identity_drawer (sample_image,1024) 0 0 (string_to_ftstring "Hello");;
Freetype.set_pixel_sizes face 32 48;;
*)

let draw_string face draw_func str =
    let img_buf = GraphicImage.create 400 100 in
    Graphic_fttext.draw_glyphs face draw_func img_buf 100 50 (string_to_ftstring face str);
    Graphics.clear_graph ();
    Graphics.draw_image (Graphics.make_image img_buf) 100 200;;

(* example main *)
try
    Graphics.open_graph "";
    draw_string freemono_face draw_rainbow "Hello, world!";
    ignore(Graphics.wait_next_event []);
    ()
with _ -> ();;
