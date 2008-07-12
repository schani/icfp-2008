open Statemachines

type vehicle_state_speed = Statemachines.speedingmachinestates
type vehicle_state_turning = Statemachines.turningmachinestates

type bctypes = Boulder | Crater
type bouldercrater = {bctype:bctypes;bcx:int;bcy:int;bcr:int}

type martian = {ex:int;ey:int;edir:int;espeed:int}

type event = CraterFall | BoulderHit | Killed | Success | Scored of int

type telemetry = {
  timestamp:int;
  speeding:vehicle_state_speed;
  turning:vehicle_state_turning;
  x:int;
  y:int;
  dir:int;
  speed:int;
  boulders:bouldercrater list;
  craters:bouldercrater list;
  martians:martian list;
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
	  edir=(parsefixpoint 10 dir);
	  espeed=(parsefixpoint 1000 speed)
	}::t.martians in
	parse_rest tl {t with martians=m}
      | _ -> failwith "parse_rest"
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
	    dir=(parsefixpoint 10 dir);
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

