class server addr port=
  let origin=Unix.socket Unix.PF_NET Unix.SOCK_STREAM 0 in
  object (self)
	   val p=port
	   method start_server=
	     Unix.bind origin (Unix.ADDR_INET(my_addr,p));
	     Unix.listen origin 2;
	     Unix.accept origin

	   initializer
	     print_endline "Starting process server.";
	     print_string "Accepting connections at port ";
	     print_int port;
	     print_endline ""
  end
