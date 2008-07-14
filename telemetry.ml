open Statemachines

type vehicle_state_speed = Statemachines.speedingmachinestates
type vehicle_state_turning = Statemachines.turningmachinestates

type vehicle_state = speedingmachinestates * turningmachinestates

type bctypes = Boulder | Crater
type bouldercrater = {bctype:bctypes;bcx:int;bcy:int;bcr:int}

type martian = {ex:int;ey:int;edir:float;espeed:int}

type event = CraterFall | BoulderHit | Killed | Success | Scored of int

type telemetry = {
  timestamp:int;
  speeding:vehicle_state_speed;
  turning:vehicle_state_turning;
  x:int;
  y:int;
  dir:float;
  speed:int;
  boulders:bouldercrater list;
  craters:bouldercrater list;
  martians:martian list;
}

type initialization = {
  idx:int;
  idy:int;
  itime_limit:int;
  imin_sensor:int;
  imax_sensor:int;
  imax_speed:int;
  imax_turn:float;
  imax_hard_turn:float;
}

type direction = Start | East | North | West | South
type field_state = Free | Partially_Free | Occupied | Unknown
    
type field = {
  mutable state: field_state;
  mutable bouldercraters: bouldercrater list;
  mutable enemy_penalty: int; (* pentalty of martians *)
  mutable dijkstra_cost: int; (* stores tmp values for calculation *)
  mutable dijkstra_round: int; (* stores in which round the field was
				  calculated last *)
  mutable dijkstra_prev: direction;
  (* stores previous field to accelerate result *)
}

module BCRecorderEntry =
  struct
    type t = bouldercrater
    let compare = Pervasives.compare
  end

module BCRecorder = Set.Make(BCRecorderEntry)

type board = {
  xdim: int;
  f_xdim: float;
  ydim: int;
  f_ydim: float;
  rxdim: int;
  f_rxdim: float;
  rydim: int;
  f_rydim: float;
  minsens: int;
  f_minsens: float;
  maxsens: int;
  f_maxsens: float;
  fields: (field array) array;
  mutable bcrecorder: Set.Make(BCRecorderEntry).t;
}

type step = Telemetry | Event | Init

type acceleration_tracker = {
  at_last_dir:float;
  at_last_change:float;
  at_max_angular_accel:float;
}

type world = {
  world_init:initialization; 
  world_vehicle_state:vehicle_state;
  world_straight_max:float;
  world_min_speed:int;
  world_max_speed:int;
  world_dst:int*int;
  world_really_close:int;
  world_board:board;
  world_acceleration_tracker:acceleration_tracker;
  world_last_step:step;
}
     

let spaceregex = Str.regexp " " 

let ctl2speeding str = 
  match str.[0] with
    | '-' -> Rolling
    | 'a' -> Accelerating
    | 'b' -> Breaking
    | _ -> failwith "invalid char in ctl2speeding"

let ctl2turning str = 
  match str.[1] with
    | 'L' -> HardLeft
    | 'l' -> Left
    | '-' -> Straight
    | 'r' -> Right
    | 'R' -> HardRight
    | _ -> failwith "invalid char in ctl2turning"

let dotregex = Str.regexp "\\."

let parsefixpoint factor string = 
  let list = Str.split dotregex string in
  match list with 
    | [] -> 0
    | [x] -> (int_of_string x)*factor
    | [x;y] -> (int_of_string x)*factor+(int_of_string y)
    | _ -> failwith ("error in parsefixpoint with string "^string)

  
let telemetry_of_string str = 
  let rec parse_rest list t = 
    match list with 
      | [] -> t
      | "b"::x::y::r::tl -> let b = {
	  bctype = Boulder;
	  bcx=(parsefixpoint 1000 x);
	  bcy=(parsefixpoint 1000 y);
	  bcr=(parsefixpoint 1000 r)}::t.boulders in
	parse_rest tl {t with boulders=b}
      | "c"::x::y::r::tl -> let c = {
	  bctype = Crater;
	  bcx=(parsefixpoint 1000 x);
	  bcy=(parsefixpoint 1000 y);
	  bcr=(parsefixpoint 1000 r)}::t.craters in
	parse_rest tl {t with craters=c}
      | "m"::x::y::dir::speed::tl -> let m = {
	  ex=(parsefixpoint 1000 x);
	  ey=(parsefixpoint 1000 y);
	  edir=(float_of_string dir);
	  espeed=(parsefixpoint 1000 speed)
	}::t.martians in
	parse_rest tl {t with martians=m}
      | "h"::x::y::r::tl -> parse_rest tl t
      | _ -> 
(Printf.fprintf stderr "parse_rest <<<<<%s>>>> hd=%s\n " str (List.hd list);
	  failwith ("parse_rest <<<<<"^str^">>>> hd="^(List.hd list)))
  in

  let list = Str.split spaceregex str in 
  let tele = 
    match list with 
      | "T"::time::ctl::x::y::dir::speed::rest -> 
	  let t = {
	    timestamp=(int_of_string time);
	    speeding=(ctl2speeding ctl);
	    turning=(ctl2turning ctl);
	    x=(parsefixpoint 1000 x);
	    y=(parsefixpoint 1000 y);
	    dir=(float_of_string dir);
	    speed=(parsefixpoint 1000 speed);
	    boulders=[];
	    craters=[];
	    martians=[];
	  } 
	  in 
	  let t = parse_rest rest t in 
	  t
      | _ -> failwith ""
  in
  tele

let event_of_string str = 
  let list = Str.split spaceregex str in
  match list with 
    | ["C"; _] -> CraterFall
    | ["B"; _] -> BoulderHit
    | ["K"; _] -> Killed
    | ["S"; _] -> Success
    | ["E"; _; s] -> Scored (int_of_string s)
    | _ -> failwith ("invalid char in event_of_string "^str)
	
let is_telemetry str = 
  (str.[0] == 'T')

let init_accel_tracker = 
  {
    at_last_dir = 0.;
    at_last_change = 0.;
    at_max_angular_accel = 0.;
  }
    
let acceltracker_skip at t = 
  {at with 
    at_last_dir = t.dir;
  }

let acceltracker_step at t = 
  let dir_change = 10. *. abs_float (at.at_last_dir -. t.dir) in
  let change_change = abs_float (at.at_last_change -. dir_change) in
(* Printf.fprintf stderr "change= %f was %f newmax: %f last_change %f %B\n" dir_change at.at_last_change change_change at.at_last_change (at.at_last_change > 0.0001); *)
  if (change_change > at.at_max_angular_accel) && (at.at_last_change > 0.0001) then
    {
      at_last_dir = t.dir;
      at_last_change = dir_change;
      at_max_angular_accel = change_change;
    }
  else
    {at with at_last_dir = t.dir; at_last_change = dir_change}

let merge_nontelemetry_into_world w e = 
  let w = {w with world_last_step = Event} in
  w

let merge_telemetry_into_world w t = 
  let w = 
    if t.timestamp = 0 then
      {w with 
	world_vehicle_state = (t.speeding,t.turning);
      }
    else 
      w
  in
  let w = 
    if w.world_last_step = Event then 
      {w with world_acceleration_tracker = acceltracker_skip w.world_acceleration_tracker t}
    else
      {w with world_acceleration_tracker = acceltracker_step w.world_acceleration_tracker t}
  in
  {w with world_last_step = Telemetry}
  
let initialization_of_string str =
  match (Str.split spaceregex str) with
    | "I"::dx::dy::t_lim::mins::maxs::maxv::maxt::maxht::[] -> begin
	try
	  {
	    idx = parsefixpoint 1000 dx;
	    idy = parsefixpoint 1000 dy;
	    itime_limit = int_of_string t_lim;
	    imin_sensor = parsefixpoint 1000 mins;
	    imax_sensor = parsefixpoint 1000 maxs;
	    imax_speed = parsefixpoint 1000 maxv;
	    imax_turn = float_of_string maxt;
	    imax_hard_turn = float_of_string maxht;
	  }
	with
	    _ -> failwith "initialization_of_string: illegal number in msg"
      end
    | _ -> failwith "initialization_of_string: got venusian message"

