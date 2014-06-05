open File_reader;;
open Disambiguator;;
open String_splitter;;
let ()=
  let i=new disambiguator in
  let d=read_file "test1.txt" in
  let q=List.map split_at_words d in 
  ignore(List.map (fun x->ignore(i#process_into_context x)) q);
  Gc.major ();
  print_endline "Prior to contextual linking";
  i#print_info;
  List.iter (fun x-> let i,b = x in print_endline (i^","^b)) (i#search_for_potential_similar_words 0.3);
  print_endline "After contextual linking";
  i#print_info;;
