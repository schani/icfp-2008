open Printf
open Telemetry

let drawing_xdim = ref 600
let drawing_ydim = ref 600

let drawcoords_of_gamecoords board (x,y) =
  let fxmul = !drawing_xdim / board.fxdim
  and fymul = !drawing_ydim / board.fydim
  and fxshift = board.fxdim / 2
  and fyshift = board.fydim / 2
  in
    (x + fxshift) * fxmul, !drawing_ydim - (y + fyshift) * fymul

let create_main_window () =
  let win = GWindow.window ~width:600 ~height:600 ()
  in let area = GMisc.drawing_area ~width:600 ~height:600 ~packing:win#add ()
  in let drawing = area#misc#realize (); new GDraw.drawable (area#misc#window)
  in let style = area#misc#style#copy
  in
    style#set_bg [`NORMAL,`BLACK];
    area#misc#set_style style;
    drawing#set_background `BLACK;
    win, area, drawing

let redraw_world world (drawing: GDraw.drawable) =
  let draw_bc bcr =
    let dx, dy = drawcoords_of_gamecoords world.world_board (bcr.bcx, bcr.bcy)
    and rx, ry = drawcoords_of_gamecoords world.world_board (bcr.bcr, bcr.bcr)
    in
      drawing#set_foreground (`NAME (match bcr.bctype with
					 Boulder -> "brown"
				       | Crater -> "gray"));
      drawing#arc ~filled:false ~x:(dx - rx / 2) ~y:(dy - ry / 2)
	~width:rx ~height:ry ();
  in
    drawing#arc ~filled:false ~x:0 ~y:0 ~width:600 ~height:600 ();
    drawing#set_foreground (`NAME "white");
    BCRecorder.iter draw_bc world.world_board.bcrecorder

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
  let mainwin, area, drawing = create_main_window ()
  in
    ignore (mainwin#connect#destroy GMain.quit);
    mainwin#show ();
    let socket = Run.create_socket ()
    in let world = Run.world_init socket
    in let ch = GMain.Io.channel_of_descr socket
    in let input_callback c =
	let world = ref world
	in
	  if List.mem `IN c then begin (* input from server *)
	    world := server_msg_callback !world socket;
(*	    area#set_size ~width:(xscaler * !world.world_board.xdim)
	      ~height:(xscaler * !world.world_board.ydim);
*)
	    redraw_world !world drawing; 
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
