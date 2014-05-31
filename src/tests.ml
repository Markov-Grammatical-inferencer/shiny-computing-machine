#load "str.cma"
#load "file_reader.cmo"
#load "string_splitter.cmo"
#load "slice.cmo"
open File_reader
open String_splitter
open Str;;
List.iter print_endline (read_file "test1.txt");;
let q=process_lines (read_file "test1.txt");;
List.iter (fun x->List.iter print_endline x) q;;
