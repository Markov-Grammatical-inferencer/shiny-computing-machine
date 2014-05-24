class filereader=
  object
    val mutable contents=[]
    val mutable number=0
    method addline (x:string)=
      contents<-contents@[x];
      number<-number+1;
      number
    method getlines =
      contents
    method reset_contents=
      contents<-[]
   end;;
let rec reader=new filereader;;
let read_file_t (file:string)=
  reader#reset_contents;
  let fi=open_in file in
  try
    while true;do
      let str=input_line fi in
      reader#addline str;(*add a line to the read file*)
    done;
  with End_of_file->
    print_endline "File finished reading";
    close_in fi;;

let read_file (file:string)=
  read_file_t file;
  reader#getlines;;
