open Printf
open Telemetry

let pi = 3.1415926535897932384626433

let drawing_xdim = ref 801
let drawing_ydim = ref 801
let f_drawing_xdim = ref 801.0
let f_drawing_ydim = ref 801.0

let rnd x = int_of_float (floor (x +. 0.5))
let foi = float_of_int

let drawlength_of_gamelength board (x,y) =
  (foi x) *. !f_drawing_xdim /. board.f_rxdim,
  (foi y) *. !f_drawing_ydim /. board.f_rydim

let drawcoords_of_gamecoords board (x,y) =
  let f_rxshift = board.f_rxdim /. 2.
  and f_ryshift = board.f_rydim /. 2.
  in
    (((foi x) +. f_rxshift) *. !f_drawing_xdim /. board.f_rxdim,
     ((!f_drawing_ydim -. 1.0) -. ((foi y) +. f_ryshift) *.
	!f_drawing_ydim /. board.f_rydim))

let create_main_window () =
  let win = GWindow.window ~width:801 ~height:801 ()
  in let area = GMisc.drawing_area ~width:801 ~height:801 ~packing:win#add ()
  in let drawing = area#misc#realize (); new GDraw.drawable (area#misc#window)
  in let style = area#misc#style#copy
  in
    style#set_bg [`NORMAL,`BLACK];
    area#misc#set_style style;
    drawing#set_background `BLACK;
    win, area, drawing

let draw_bc board (drawing: GDraw.drawable) bcr =
  let f_dx, f_dy = drawcoords_of_gamecoords board (bcr.bcx, bcr.bcy)
  and f_rx, f_ry = drawlength_of_gamelength board (bcr.bcr, bcr.bcr)
  in
    drawing#set_foreground (`NAME (match bcr.bctype with
				       Boulder -> "gray"
				     | Crater -> "brown"));
    drawing#arc ~filled:false ~x:(rnd (f_dx -. f_rx)) ~y:(rnd (f_dy -. f_ry))
      ~width:(rnd (f_rx *. 2.)) ~height:(rnd (f_ry *. 2.)) ()

let draw_bot board (drawing: GDraw.drawable) x y angle angle2 dst =
(*  printf "draw_bot at %i,%i    angle=%f angle2=%f\n" x y angle angle2; *)
  let f_dx, f_dy = drawcoords_of_gamecoords board (x, y)
  and dst_x, dst_y = drawcoords_of_gamecoords board (dst)
  and bot_r = 20.0
  and line_len = 150.0
  in let line1x = f_dx +. (cos (angle *. pi /. 180.)) *. line_len
     and line1y = f_dy -. (sin (angle *. pi /. 180.)) *. line_len
     and line2x = f_dx +. (cos (angle2 *. pi /. 180.)) *. line_len
     and line2y = f_dy -. (sin (angle2 *. pi /. 180.)) *. line_len
  in
    drawing#set_foreground (`NAME "yellow");
    drawing#arc ~filled:false ~x:(rnd (f_dx -. bot_r)) ~y:(rnd (f_dy -. bot_r))
      ~width:(rnd (bot_r *. 2.)) ~height:(rnd (bot_r *. 2.)) ();
    drawing#line (rnd f_dx) (rnd f_dy) (rnd line1x) (rnd line1y);
    drawing#set_foreground (`NAME "red");
    drawing#line (rnd f_dx) (rnd f_dy) (rnd dst_x) (rnd dst_y);
    drawing#set_foreground (`NAME "lightblue");
    drawing#line (rnd f_dx) (rnd f_dy) (rnd line2x) (rnd line2y)
      

let draw_background board (drawing: GDraw.drawable) =
  let fdrxdim = float_of_int !drawing_xdim
  and fdrydim = float_of_int !drawing_ydim
  and rxdim = float_of_int board.xdim
  and rydim = float_of_int board.ydim
  in let xbs = rnd (fdrxdim /. rxdim)
     and ybs = rnd (fdrydim /. rydim)
  in
    for yi = 0 to (board.ydim - 1)
    do
      for xi = 0 to (board.xdim - 1)
      do
	let f = board.fields.(yi).(xi)
	in
	  if f.state != Unknown then begin
	    drawing#set_foreground (`NAME (match f.state with
					       Free -> "darkgreen"
					     | Occupied -> "darkred"
					     | Partially_Free ->
						 "darkgray"
					     | _ -> "white" ));
	    drawing#rectangle ~filled:false
	      ~x:(rnd (fdrxdim *. (foi xi) /. rxdim))
	      ~y:(rnd (fdrydim -. fdrydim *. (foi yi) /. rydim -. (foi ybs)))
	      ~width:(xbs - 1) ~height:(ybs - 1) ();
	  end
      done
    done

let draw_homebase board (drawing: GDraw.drawable) =
  let f_dx, f_dy = drawcoords_of_gamecoords board (0, 0)
  and f_rx, f_ry = drawlength_of_gamelength board (2500, 2500)
  in
    (*
    printf "drawing homebase at %f,%f (%i,%i size: %i,%i)\n" f_dx f_dy
      (rnd (f_dx -. f_rx)) (rnd (f_dy -. f_ry))
      (rnd (f_rx *. 2.)) (rnd (f_ry *. 2.)); *)
    drawing#set_foreground (`NAME "green");
    drawing#arc ~filled:false ~x:(rnd (f_dx -. f_rx)) ~y:(rnd (f_dy -. f_ry))
      ~width:(rnd (f_rx *. 2.)) ~height:(rnd (f_ry *. 2.)) ()

let drawing_hacks board (drawing: GDraw.drawable) =
  for i = 0 to board.xdim - 1
  do
    board.fields.(i).(i).state <- Occupied;
  done;
  drawing#set_foreground (`NAME "white");
  drawing#line 0 !drawing_ydim !drawing_xdim 0

let redraw_world world (area: GMisc.drawing_area) (drawing: GDraw.drawable) _ =
  let x,y = drawing#size;
  and board = !world.world_board
  in
    drawing_xdim := x;
    f_drawing_xdim := float_of_int x;
    drawing_ydim := y;
    f_drawing_ydim := float_of_int y;
    drawing#set_foreground (`NAME "black");
    drawing#rectangle ~filled:true ~x:0 ~y:0 ~width:x ~height:y ();
    draw_background board drawing;
    draw_homebase board drawing;
    begin
      match !world.world_current_telemetry with
	  Some t -> draw_bot board drawing t.x t.y t.dir !world.world_aiming_at !world.world_dst
	| None -> ()
    end;
    flush stdout;

    BCRecorder.iter (draw_bc board drawing) board.bcrecorder;
(*    drawing_hacks board drawing; *)
    true

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
    in let world = ref (Run.world_init socket)
    in let ch = GMain.Io.channel_of_descr socket
    in let input_callback c =
	if List.mem `IN c then begin (* input from server *)
	  world := server_msg_callback !world socket;
	  (*	    area#set_size ~width:(xscaler * !world.world_board.xdim)
		    ~height:(xscaler * !world.world_board.ydim);
	  *)
(*	  ignore (redraw_world world area drawing ()); *)
	  true;
	end else begin
	  prerr_endline "got HUP or ERR from server";
	  false;
	end
    in
      ignore (area#event#connect#expose
		~callback:(redraw_world world area drawing));
      ignore (GMain.Io.add_watch ch ~prio:0 ~cond:[`IN; `HUP; `ERR]
		~callback:input_callback);
      (*
      ignore (GMain.Idle.add (redraw_world world area drawing));
      *)
      GMain.Timeout.add 100 (redraw_world world area drawing);
      GMain.Main.main ()

let _ =
  main ()

