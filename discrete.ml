(* FIXME: diid overflow must be catched *)

open Telemetry
open Printf

let pi = 3.1415926535897932384626433
  
let param_base_cost = 10
let param_martian_core_penalty = 50
let param_martian_penalty_radius = 3
let param_martian_penalty_decrementer = 12

let rnd x = int_of_float (floor (x +. 0.5))
let foi = float_of_int
let iof = int_of_float
  
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
    f_xdim = float_of_int x;
    ydim = y;
    f_ydim = float_of_int y;
    rxdim = fx;
    f_rxdim = float_of_int fx;
    rydim = fy;
    f_rydim = float_of_int fy;
    minsens = minsens;
    f_minsens = float_of_int minsens;
    maxsens = maxsens;
    f_maxsens = float_of_int maxsens;
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
  let f_multx = board.f_rxdim /. board.f_xdim
  and f_shiftx = board.f_rxdim /. 2.
  and f_multy = board.f_rydim /. board.f_ydim
  and f_shifty = board.f_rydim /. 2.
  in
    ((((foi x) *. f_multx -. f_shiftx), (* x1 *)
      ((foi y) *. f_multy -. f_shifty)), (* y1 *)
     (((foi x) *. f_multx -. f_shiftx), (* x2 *)
      ((foi y) *. f_multy -. f_shifty +. f_multy)), (* y2 *)
     (((foi x) *. f_multx -. f_shiftx +. f_multx), (* x3 *)
      ((foi y) *. f_multy -. f_shifty +. f_multy)), (* y3 *)
     (((foi x) *. f_multx -. f_shiftx +. f_multx), (* x4 *)
      ((foi y) *. f_multy -. f_shifty))) (* y4 *)

let discretize_coords board (fx,fy) =
  (board.f_xdim *. (foi fx) /. board.f_rxdim +. board.f_xdim /. 2.,
   board.f_ydim *. (foi fy) /. board.f_rydim +. board.f_ydim /. 2.)

let f_discretize_coords board (fx,fy) = (* float version *)
  (board.f_xdim *. fx /. board.f_rxdim +. board.f_xdim /. 2.,
   board.f_ydim *. fy /. board.f_rydim +. board.f_ydim /. 2.)

let sinvollify_coord max = function
    v when v < 0 -> 0
  | v when v >= max -> max - 1
  | v -> v
  
let discrete_inner_range board (fx1, fy1) (fx2, fy2) =
  let x1,y1 = f_discretize_coords board (fx1, fy1)
  and x2,y2 = f_discretize_coords board (fx2, fy2)
  in let x1,y1 = (iof (floor x1)) + 1, (iof (floor y1)) + 1
     and x2,y2 = (iof (ceil x2)) - 1, (iof (ceil y2)) - 1
  in
    if x1 > x2 || y1 > y2 then (* range might be non existant *)
      None
    else
      Some ((sinvollify_coord board.xdim x1, sinvollify_coord board.ydim y1),
	    (sinvollify_coord board.xdim x2, sinvollify_coord board.ydim y2))

let discrete_outer_range board (fx1, fy1) (fx2, fy2) =
  let x1,y1 = f_discretize_coords board (fx1, fy1)
  and x2,y2 = f_discretize_coords board (fx2, fy2)
  in let (x1, y1), (x2, y2) =
      (iof (floor x1), iof (floor y1)), (iof (ceil x2), iof (ceil y2))
  in
    (((sinvollify_coord board.xdim x1), (sinvollify_coord board.ydim y1)),
     ((sinvollify_coord board.xdim x2), (sinvollify_coord board.ydim y2)))

(* API: this can be used to register a geometric circle into the
   discrete map.
   It works by: first flagging all fields that are in are fully in an inner
   rectangle.
   Then it checks all rectangles that are partly in an outer rectangle. Those
   in inner rectangles will be skipped anyway.
*)
let register_boldercrater board bcr =
  if BCRecorder.mem bcr board.bcrecorder then (* prohibits doulbes *)
    ()
  else begin (* skip circles that are already known *)
    let x = bcr.Telemetry.bcx
    and y = bcr.Telemetry.bcy
    and r = bcr.Telemetry.bcr
    in let f_x = foi x
       and f_y = foi y
       and f_r = foi r
    in
      fprintf stdout "registering bouldercrater at %i,%i\n" x y;
      board.bcrecorder <- BCRecorder.add bcr board.bcrecorder;
      let f_rsquare = f_r *. f_r
      in let check_inside (cx, cy) =
	  (* fprintf stdout "check_inside (%i, %i)   circ at (%i, %i, r=%i)\n"
	     cx cy x y r; *)
	  if (f_x -. cx) *. (f_x -. cx) +.
	    (f_y -. cy) *. (f_y -. cy) <= f_rsquare then
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
		    printf "cound_inside is %i\n" !count_inside;
		    match !count_inside with
			4 -> (* fully inside *)
			  printf "SAUBUA DEPPATA\n";
			  f.state <- Occupied; 
			  f.bouldercraters <- bcr :: f.bouldercraters
		      | (-4) ->
			  () (* outside *)
		      | _ -> (* partly inside *)
			  if (f.state != Occupied) then
			    f.state <- Partially_Free;
			  f.bouldercraters <- bcr :: f.bouldercraters
	     done
	  done
      in let f_h = f_r /. sqrt(2.0);
      in
	(*
	begin
	  match discrete_inner_range board
	    (f_x -. f_h, f_y -. f_h) (f_x +. f_h, f_y +. f_h) with
		Some (c1, c2) ->
		  printf "checking inner range: %f,%f - %f,%f [%i,%i - %i,%i]\n"
		    (f_x -. f_h) (f_y -. f_h) (f_x +. f_h) (f_y +. f_h)
		    (fst c1) (snd c1) (fst c2) (snd c2);
		flag_as_occupied board c1 c2 bcr
	    | None -> () (* inner range might be empty *);
	end;
	*)
	let c1, c2 = discrete_outer_range board
	  (f_x -. f_r, f_y -. f_r) (f_x +. f_r, f_y +. f_r)
	in
	  printf "checking outer range: %f,%f - %f,%f [%i,%i - %i,%i]\n"
	    (f_x -. f_r) (f_y -. f_r) (f_x +. f_r) (f_y +. f_r)
	    (fst c1) (snd c1) (fst c2) (snd c2);
	  check_for_occupied c1 c2
  end

(* API: this can be used to register the view ellipse, works like
   register_boldercrater but marks space as free not occupied.
   Note:  all circles MUST be registered before

   It first approximates a inner circle and then make hard work on remaining
   fields in outer rectangle.
*)
(* note: this fun is floatified without f_ prefixes *)
let register_ellipse board (r1x, r1y) angle =
  let p = board.f_minsens
  and q = board.f_maxsens
  and f1x = foi r1x
  and f1y = foi r1y
  in let a = (p +. q) /. 2.
     and e = (q -. p) /. 2.
  in let bsquare = a *. a -. e *. e
  in let fb = sqrt bsquare
     and cosangle = cos(angle *. pi /. 180.)
     and sinangle = sin(angle *. pi /. 180.)
  in let f_mx = f1x +. e *. cosangle
     and f_my = f1y +. e *. sinangle
     and f2x = f1x +. 2. *. e *. cosangle
     and f2y = f1y +. 2. *. e *. sinangle
      (* now flag circle located at mx,my with radius b *)
     and f_2a = 2. *. a
  in let f_h = fb *. sqrt(2.0)
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
      let f_sqr1 = (f1x -. cx) *. (f1x -. cx) +. (f1y -. cy) *. (f1y -. cy)
      and f_sqr2 = (f2x -. cx) *. (f2x -. cx) +. (f2y -. cy) *. (f2y -. cy)
      in
	if (sqrt f_sqr1) +. (sqrt f_sqr2) > f_2a then
	  (-1) (* outside *)
	else
	  1 (* inside *)
  in let check_for_free (x1, y1) (x2, y2) =
      for xi = x1 to x2 do
	for yi = y1 to y2 do
	  let f = board.fields.(yi).(xi)
	  in
	    if f.state = Unknown then
	      let (fx1,fy1), (fx2,fy2), (fx3,fy3), (fx4,fy4) =
		undiscretize_coords board (xi,yi)
	      and count_inside = ref 0
	      in
		count_inside := !count_inside + check_inside (fx1,fy1);
		count_inside := !count_inside + check_inside (fx2,fy2);
		count_inside := !count_inside + check_inside (fx3,fy3);
		count_inside := !count_inside + check_inside (fx4,fy4);
		match !count_inside with
		    4 ->
		      if f.state = Unknown then
			f.state <- Free; (* fully inside *)
		  | (-4) -> () (* outside but might not be free *)
		  | _ -> () (*f.state <- Partially_Free (* partly inside *) *)
	done
      done
  in
    (*
    begin
      match discrete_inner_range board
	(f_mx -. f_h, f_my -. f_h) (f_mx +. f_h, f_my +. f_h) with
	    Some (c1, c2) -> flag_as_free board c1 c2
	  | None -> () (* inner range might be empty *);
    end;
    *)
    let c1, c2 = discrete_outer_range board
      (f_mx -. a, f_my -. a) (f_mx +. a, f_my +. a)
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
