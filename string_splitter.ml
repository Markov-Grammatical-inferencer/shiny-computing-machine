
let split_at_words (s:string)=
  (* Strip punctuation, and uncapitalize*)
  List.map String.uncapitalize (Str.split (Str.regexp "[^a-zA-Z0-9]+") s);;

let process_lines(s:string list)=
  (* Process multiple lines and place them into a list of lists of strings*)
  List.concat([List.map split_at_words s]);;

