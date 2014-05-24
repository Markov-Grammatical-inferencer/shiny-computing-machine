
let split_at_words (s:string)=
  Str.split (Str.regexp " ") s;;

let process_lines(s:string list)=
  List.concat([List.map split_at_words s]);;
