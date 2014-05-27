
let rec split_at_words (s:string)=
  Str.split (Str.regexp "[^a-zA-Z0-9]+") s;;

let rec process_lines(s:string list)=
  List.concat([List.map split_at_words s]);;

