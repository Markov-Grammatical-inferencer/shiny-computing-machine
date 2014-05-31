class filereader=
  object
    val mutable contents=[]
    method addline (x:string)=
      contents<-contents@[x];
    method getlines =
      contents
    method reset_contents=
      contents<-[]
    method getnumber=
      List.length contents
   end;;


let read_file (file:string)=
  let reader=new filereader in
  let read_file_t (file:string)=
    (reader#reset_contents;
    let fi=open_in file in
    try
      while true;do
	let str=input_line fi in
	reader#addline str;(*add a line to the read file*)
      done;
    with End_of_file->
      print_endline "File finished reading";
      close_in fi) in
  read_file_t file;
  reader#getlines;;
