open Statemachines

type vehicle_state_speed = Statemachines.speedingmachinestates
type vehicle_state_turning = Statemachines.turningmachinestates

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
      | _ -> failwith ("parse_rest "^str^" hd="^(List.hd list))
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

let merge_telemetry_into_world x t = x

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

