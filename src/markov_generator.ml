let print_usage () =
    Printf.printf "Usage: markov_generator filename window_length output_length\n%!";;
try
    let filename = Sys.argv.(1) in
    let window = int_of_string Sys.argv.(2) in
    let length = int_of_string Sys.argv.(3) in
    Printf.printf "%s\n%!" (Markov_chain_generator.main filename window length)
with _ -> print_usage ();;
