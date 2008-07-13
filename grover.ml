open Printf

let create_main_window () =
  let w = GWindow.window ~width:600 ~height:600 ()
(*  in let area = GMisc.drawing_area ~width:100 ~height:100 ~packing:evbox#add () *)
  in
    w

let server_msg_callback world socket =
  Run.world_step world socket

let main () =
  prerr_endline "Starting up GTK version of rover..";
  begin
    try
      ignore (GMain.init ());
    with
	x ->
	  fprintf stderr "are you sure you have an X server running?\n";
	  raise x
  end;
  let mainwin = create_main_window ()
  in
    mainwin#connect#destroy GMain.quit;
    mainwin#show ();
    let socket = Run.create_socket ()
    in let world = Run.world_init socket
    in let ch = GMain.Io.channel_of_descr socket
    in let input_callback c =
	let world = ref world
	in
	  if List.mem `IN c then begin (* input from server *)
	    world := server_msg_callback !world socket;
	    true;
	  end else begin
	    prerr_endline "got HUP or ERR from server";
	    false;
	  end
    in
      GMain.Io.add_watch ch ~prio:0 ~cond:[`IN; `HUP; `ERR]
	~callback:input_callback;
      GMain.Main.main ()

let _ =
  main ()
