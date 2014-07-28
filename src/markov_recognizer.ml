let print_usage () =
    Printf.printf "Usage: markov_recognizer training_data other_file window_size\n%!";;
try
    let training_data = Sys.argv.(1) in
    let other_file = Sys.argv.(2) in
    let window = int_of_string Sys.argv.(3) in
	Printf.printf "%s\n%!" (Markov_chain_generator.recognizer_main training_data other_file window)
with
Invalid_argument(x) ->
    if x = "Array.make" then
    Printf.printf "File is too large to process.\n%!"
    else print_usage ()
| _ -> print_usage ();;
