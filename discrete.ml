(* FIXME: diid overflow must be catched *)

type direction = Start | East | North | West | South
type field_state = Known | Partially | Unknown
    
type field = {
  mutable state: field_state;
  mutable enemy_penalty: int; (* pentalty of martians *)
  mutable dijkstra_cost: int; (* stores tmp values for calculation *)
  mutable dijkstra_round: int; (* stores in which round the field was
				  calculated last *)
  mutable dijkstra_prev: direction;
  (* stores previous field to accelerate result *)
}

type board = {
  xdim: int;
  ydim: int;
  fields: (field array) array;
}

let param_base_cost = 10
 
let neg_dir = function
    East -> West
  | North -> South
  | West -> East
  | South -> North
  | Start ->
      Printf.fprintf stderr "discrete.neg_dir: error: called with start";
      Start

let make_array_array x y z =
  Array.init y (fun _ -> Array.make x z)

let create_board x y =
  {
    xdim = x;
    ydim = y;
    fields = make_array_array x y {
      state = Unknown;
      enemy_penalty = 0;
      dijkstra_cost = 0;
      dijkstra_round = 0;
      dijkstra_prev = South; (* means nothing *)
    }
  }

let incr_coords (x,y) = function
    East -> (x + 1, y)
  | North -> (x, y + 1)
  | West -> (x - 1, y)
  | South -> (x, y - 1)
  | Start ->
      Printf.fprintf stderr "discrete.neg_dir: error: called with start";
      x,y

(* dijkstra round id: stores the current round *)
let diid = ref 0

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

let dijkstra_find_path board origin dest =
  let queue = ref PriSet.empty
  in let ox,oy = origin
     and dx,dy = dest
     and check_neighbours f x y =
      let relax s d dx dy dir =
	let negdir = neg_dir dir
	  (* calculate cost bonuses and maluses in here *)
	in let n = param_base_cost + d.enemy_penalty +
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
	if x < (board.xdim - 1) && board.fields.(y).(x+1).state != Unknown then
	  relax f board.fields.(y).(x+1) (x+1) y East;
	if y < (board.ydim - 1) && board.fields.(y+1).(x).state != Unknown then
	  relax f board.fields.(y+1).(x) x (y+1) North;
	if x > 0 && board.fields.(y).(x-1).state != Unknown then
	  relax f board.fields.(y).(x-1) (x-1) y West;
	if y > 0 && board.fields.(y-1).(x).state != Unknown then
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
