open File_reader;;
open Disambiguator;;
open String_splitter;;
let ()=
  let i=new disambiguator in
  let d=read_file "test1.txt" in
  let q=List.map split_at_words d in 
  ignore(List.map (fun x->ignore(i#process_into_context x)) q);
  i#print_info;;
