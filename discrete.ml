(* FIXME: diid overflow must be catched *)

open Telemetry

let pi = 3.1415926535897932384626433
  
let param_base_cost = 10
let param_martian_core_penalty = 50
let param_martian_penalty_radius = 3
let param_martian_penalty_decrementer = 12

let neg_dir = function
    East -> West
  | North -> South
  | West -> East
  | South -> North
  | Start ->
      Printf.fprintf stderr "discrete.neg_dir: error: called with start";
      Start

let make_array_array x y f =
  Array.init y (fun _ -> Array.init x f)

(* API: use this to create a new board *)
let create_board x y fx fy minsens maxsens =
  {
    xdim = x;
    ydim = y;
    fxdim = fx;
    fydim = fy;
    minsens = minsens;
    maxsens = maxsens;
    fields = make_array_array x y (fun _ -> {
				     state = Unknown;
				     bouldercraters = [];
				     enemy_penalty = 0;
				     dijkstra_cost = 0;
				     dijkstra_round = 0;
				     dijkstra_prev = South; (* means nothing *)
				   });
    bcrecorder = BCRecorder.empty;
  }

let incr_coords (x,y) = function
    East -> (x + 1, y)
  | North -> (x, y + 1)
  | West -> (x - 1, y)
  | South -> (x, y - 1)
  | Start ->
      Printf.fprintf stderr "discrete.neg_dir: error: called with start";
      x,y

(* this returns 4 coords of the corners of the discrete field,
   first south west then clockwise *)
let undiscretize_coords board (x,y) =
  let multx = board.fxdim / board.xdim
  and shiftx = board.fxdim / 2
  and multy = board.fydim / board.ydim
  and shifty = board.fydim / 2
  in
    (((x * multx + shiftx), (y * multy + shifty)),
     ((x * multx + shiftx), (y * multy + shifty + multy)),
     ((x * multx + shiftx + multx), (y * multy + shifty + multy)),
     ((x * multx + shiftx + multx), (y * multy + shifty)))

let discretize_coords board (fx,fy) =
  (board.xdim * fx / board.fxdim + board.xdim / 2,
   board.ydim * fy / board.fydim + board.ydim /
     
(* tests:
  discretize_coords { xdim = 9; ydim = 9; fxdim = 90; fydim = 90 } (0,0)

  discretize_coords { xdim = 9; ydim = 9; fxdim = 90; fydim = 90 } (-50,-50)
  
  discretize_coords { xdim = 9; ydim = 9; fxdim = 90; fydim = 90 } (39,40)
*)

2)

let discrete_inner_range board (fx1, fy1) (fx2, fy2) =
  let x1,y1 = discretize_coords board (fx1, fy1)
  and x2,y2 = discretize_coords board (fx2, fy2)
  in let x1,y1 = x1 + 1, y1 + 1
     and x2,y2 = x2 - 1, y2 - 1
  in
    if x1 > x2 || y1 > y2 then (* range might be non existant *)
      None
    else
      Some ((x1,y1), (x2, y2))

let discrete_outer_range board (fx1, fy1) (fx2, fy2) =
  let x1,y1 = discretize_coords board (fx1, fy1)
  and x2,y2 = discretize_coords board (fx2, fy2)
  in
    (x1,y1), (x2, y2)

(* API: this can be used to register a geometric circle into the
   discrete map.
   It works by: first flagging all fields that are in are fully in an inner
   rectangle.
   Then it checks all rectangles that are partly in an outer rectangle. Those
   in inner rectangles will be skipped anyway.
*)
let register_boldercrater board bcr =
  let x = bcr.Telemetry.bcx
  and y = bcr.Telemetry.bcy
  and r = bcr.Telemetry.bcr
  in
    if BCRecorder.mem bcr board.bcrecorder then (* prohibits doulbes *)
      ()
    else (* skip circles that are already known *)
      board.bcrecorder <- BCRecorder.add bcr board.bcrecorder;
      let rsquare = r * r
      in let check_inside (cx, cy) =
	  if (x - cx) * (x - cx) + (y - cy) * (y - cy) < rsquare then
	    1 (* inside *)
	  else
	    (-1) (* outside *)
      in let flag_as_occupied board (x1, y1) (x2, y2) bcr =
	  for xi = x1 to x2 do
	    for yi = y1 to y2 do
	      let f = board.fields.(yi).(xi)
	      in
		f.state <- Occupied;
		f.bouldercraters <- bcr :: f.bouldercraters;
	    done
	  done
      in let check_for_occupied (x1, y1) (x2, y2) =
	  for xi = x1 to x2 do
	    for yi = y1 to y2 do
	      let f = board.fields.(yi).(xi)
	      in
		if List.mem bcr f.bouldercraters then
		  () (* no need to double check *)
		else
		  let (fx1,fy1), (fx2,fy2), (fx3,fy3), (fx4,fy4) =
		    undiscretize_coords board (xi,yi)
		  and count_inside = ref 0
		  in
		    count_inside := !count_inside + check_inside (fx1,fy1);
		    count_inside := !count_inside + check_inside (fx2,fy2);
		    count_inside := !count_inside + check_inside (fx3,fy3);
		    count_inside := !count_inside + check_inside (fx4,fy4);
		    match !count_inside with
			4 -> (* fully inside *)
			  f.state <- Occupied; 
			  f.bouldercraters <- bcr :: f.bouldercraters
		      | (-4) -> () (* outside *)
		      | _ -> (* partly inside *)
			  if (f.state != Occupied) then
			    f.state <- Partially_Free;
			  f.bouldercraters <- bcr :: f.bouldercraters
	     done
	  done
      in let fr = float_of_int r; (* calculate inner square *)
      in let fh = fr /. sqrt(2.0);
      in let h = int_of_float fh
      in
	begin
	  match discrete_inner_range board (x - h, y - h) (x + h, y + h) with
	      Some (c1, c2) -> flag_as_occupied board c1 c2 bcr
	    | None -> () (* inner range might be empty *);
	end;
	let c1, c2 = discrete_outer_range board (x - r, y - r) (x + r, y + r)
	in
	  check_for_occupied c1 c2

(* API: this can be used to register the view ellipse, works like
   register_boldercrater but marks space as free not occupied.
   Note:  all circles MUST be registered before

   It first approximates a inner circle and then make hard work on remaining
   fields in outer rectangle.
*)
let register_ellipse board (f1x, f1y) angle =
  let p = board.minsens
  and q = board.maxsens
  in let a = (p + q) / 2
     and e = (q - p) / 2
  in let bsquare = a * a - e * e
  in let fb = sqrt (float_of_int bsquare)
     and cosangle = cos(angle *. pi /. 180.)
     and sinangle = sin(angle *. pi /. 180.)
  in let fmx = (float_of_int f1x) +. (float_of_int e) *. cosangle
     and fmy = (float_of_int f1y) +. (float_of_int e) *. sinangle
     and ff2x = (float_of_int f1x) +. 2. *. (float_of_int e) *. cosangle
     and ff2y = (float_of_int f1y) +. 2. *. (float_of_int e) *. sinangle
      (* now flag circle located at mx,my with radius b *)
  in let mx = int_of_float fmx
     and my = int_of_float fmy
     and f2x = int_of_float ff2x
     and f2y = int_of_float ff2y
     and f2a = float_of_int (2 * a)
  in let fh = fb *. sqrt(2.0)
  in let flag_as_free board (x1, y1) (x2, y2) =
      for xi = x1 to x2 do
	for yi = y1 to y2 do
	  if board.fields.(yi).(xi).state = Unknown then
	    board.fields.(yi).(xi).state <- Free;
	done
      done
  in let check_inside (cx, cy) =
      (* my theory: if sum of distance to F1 and F2 is more then 2a then
	 point lies outside, else inside *)
      let sqr1 = (f1x - cx) * (f1x - cx) + (f1y - cy) * (f1y - cy)
      and sqr2 = (f2x - cx) * (f2x - cx) + (f2y - cy) * (f2y - cy)
      in
	if (sqrt (float_of_int sqr1)) +. (sqrt (float_of_int sqr2)) > f2a then
	  (-1) (* outside *)
	else
	  1 (* inside *)
  in let check_for_free (x1, y1) (x2, y2) =
      for xi = x1 to x2 do
	for yi = y1 to y2 do
	  let f = board.fields.(yi).(xi)
	  in
	    if f.state = Occupied || f.state = Free then
	      () (* no need to double check *)
	    else
	      let (fx1,fy1), (fx2,fy2), (fx3,fy3), (fx4,fy4) =
		undiscretize_coords board (xi,yi)
	      and count_inside = ref 0
	      in
		count_inside := !count_inside + check_inside (fx1,fy1);
		count_inside := !count_inside + check_inside (fx2,fy2);
		count_inside := !count_inside + check_inside (fx3,fy3);
		count_inside := !count_inside + check_inside (fx4,fy4);
		match !count_inside with
		    4 -> f.state <- Occupied (* fully inside *)
		  | (-4) -> () (* outside but might not be free *)
		  | _ -> f.state <- Partially_Free (* partly inside *)
	done
      done
  in let h = int_of_float fh
  in
    begin
      match discrete_inner_range board (mx - h, my - h) (mx + h, my + h) with
	  Some (c1, c2) -> flag_as_free board c1 c2
	| None -> () (* inner range might be empty *);
    end;
    let c1, c2 = discrete_outer_range board (mx - a, my - a) (mx + a, my + a)
    in
      check_for_free c1 c2

let martian_add b x y p =
  if x >= 0 && x < b.xdim && y >= 0 && y < b.ydim then
    b.fields.(y).(x).enemy_penalty <- b.fields.(y).(x).enemy_penalty + p

let martian_squarecircle b x y r p =
  for xi = x - r to x + r
  do
    martian_add b xi (y - r) p;
    martian_add b xi (y + r) p;
  done;
  for yi = y - r + 1 to y + r - 1
  do
    martian_add b (x - r) yi p;
    martian_add b (x + r) yi p;
  done

let rec martian_modify b x y p r =
  if r > param_martian_penalty_radius then
    ()
  else begin
    martian_squarecircle b x y r p;
    martian_modify b x y (p - param_martian_penalty_decrementer) (r + 1)
  end

(* API: use this to register martians *)
let register_martian board x y m =
  martian_modify board x y param_martian_core_penalty 1

(* API: use this to unregister martians *)
let unregister_martian board x y =
  martian_modify board x y (-1 * param_martian_core_penalty) 1

(* API: use this to register a bouldercrater *)
let register_bouldercrater board x y bc =
  if not (List.mem bc board.fields.(y).(x).bouldercraters) then
    board.fields.(y).(x).bouldercraters <-
      bc :: board.fields.(y).(x).bouldercraters

(* API: use this to query bouldercraters *)
let query_bouldercraters board x y =
  board.fields.(y).(x).bouldercraters

module PriSetEntry =
  struct
    type t = int * (field * int * int)
    let compare ((x, _):t) ((y, _):t) =
      Pervasives.compare x y
  end

module PriSet = Set.Make(PriSetEntry)

let queue_insert q e =
  q := PriSet.add e !q

let queue_fetch_cheapest q =
  let e = PriSet.min_elt !q
  in
    q := PriSet.remove e !q;
    e

let queue_remove q e =
    q := PriSet.remove e !q


(* dijkstra round id: stores the current round *)
let diid = ref 0

(* API: use this to find a way to a destination *)
let dijkstra_find_path board origin dest =
  let queue = ref PriSet.empty
  in let ox,oy = origin
     and dx,dy = dest
     and check_neighbours f x y =
      let relax s d dx dy dir =
	if d.state = Unknown || d.state = Occupied then
	  (* skip unkown and occupied fields *)
	  ()
	else
	  let negdir = neg_dir dir
	    (* calculate cost bonuses and maluses in here *)
	  in let n = param_base_cost +
	      (match d.state with
		   Partially_Free -> 30
		 | _ -> 0) +
	      d.enemy_penalty +
	      (match s.dijkstra_prev with
		   x when x = negdir -> (-2) (* keeping diretion -> bonus *)
		 | x when x = dir -> 2 (* u-turn -> malus *)
		 | _ -> 0)
	  in
	    if d.dijkstra_round < !diid then begin
	      d.dijkstra_round <- !diid;
	      d.dijkstra_cost <- s.dijkstra_cost + n;
	      d.dijkstra_prev <- negdir;
	      queue_insert queue (d.dijkstra_cost, (d, dx, dy));
	    end else (* already visited, check if new way is better *)
	      if (s.dijkstra_cost + n) < d.dijkstra_cost then begin
		(* we found a better way! *)
		queue_remove queue (d.dijkstra_cost, (d, dx, dy));
		d.dijkstra_cost <- s.dijkstra_cost + n;
		d.dijkstra_prev <- negdir;
		queue_insert queue (d.dijkstra_cost, (d, dx, dy));
	      end
      in
	if x < (board.xdim - 1) then
	  relax f board.fields.(y).(x+1) (x+1) y East;
	if y < (board.ydim - 1) then
	  relax f board.fields.(y+1).(x) x (y+1) North;
	if x > 0 then
	  relax f board.fields.(y).(x-1) (x-1) y West;
	if y > 0 then
	  relax f board.fields.(y-1).(x) x (y-1) South;
  in let start_field = board.fields.(oy).(ox)
  in
    incr diid;
    start_field.dijkstra_cost <- 0;
    start_field.dijkstra_round <- !diid;
    start_field.dijkstra_prev <- Start;
    queue_insert queue (0, (start_field, ox, oy));
    let rec loop () =
      let _, (f,x,y) = queue_fetch_cheapest queue
      in
	if x = dx && y = dy then (* found goal *)
	  let rec tracegoal (x, y) result =
	    let prev = board.fields.(y).(x).dijkstra_prev
	    in
	      if prev == Start then
		result
	      else
		tracegoal (incr_coords (x,y) prev) ((x, y) :: result)
	  in
	    tracegoal (x, y) []
	else begin
	  check_neighbours f x y;
	  loop ();
	end
    in
      loop ()
