Random.init (int_of_float (Unix.time ()));;

let print_usage () =
    Printf.printf "Usage: markov_generator filename window_length output_length\n%!";;
try
    let filename = Sys.argv.(1) in
    let window = int_of_string Sys.argv.(2) in
    let length = int_of_string Sys.argv.(3) in
    Printf.printf "%s\n%!" (Markov_chain_generator.main filename window length)
with
Invalid_argument(x) ->
    if x = "Array.make" then
    Printf.printf "File is too large to process.\n%!"
    else print_usage ()
| _ -> print_usage ();;
