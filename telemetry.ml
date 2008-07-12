open Statemachines

type vehicle_state_speed = Statemachines.speedingmachinestates
type vehicle_state_turning = Statemachines.turningmachinestates

type bctypes = Bolder | Crater
type bouldercrater = {bctype:bctypes;bcx:int;bcy:int;bcr:int}

type martian = {ex:int;ey:int;edir:int;espeed:int}

type event = Crater | BoulderHit | Killed | Success | Scored of int

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
  let rec parse_rest list t = t
    (*
    match list with 
      | [] -> t
      | "b"::x::y::r:tl -> let b = {bcx=x;bcy=y;bcr=r}::t.boulders in
	parse_rest tl {t with boulders=b}
      | "c"::x::y::r:tl -> let b = {bcx=x;bcy=y;bcr=r}::t.craters in
	parse_rest tl {t with craters=c}
    *)
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
    | ["C"; _] -> Crater
    | ["B"; _] -> BoulderHit
    | ["K"; _] -> Killed
    | ["S"; _] -> Success
    | ["E"; _; s] -> Scored (int_of_string s)
    | _ -> failwith ("invalid char in event_of_string "^str)
	
let is_telemetry str = 
  (str.[0] == 'T')


let wanted = ref (Accelerating,Straight) 

let stupid_loop_one_game socket = 
  
  let _ = Communication.waitfordata socket in
  let init = Communication.sock_recv_next socket in 
  (match init with 
    | None -> failwith " IOFOOF ";
    | _ -> ignore(init););
  Printf.fprintf stderr "ignoring init for now\n";
  
  let rec loop () = 
    let _ = Communication.waitfordata socket in
    let next = 
      match Communication.sock_recv_next socket with 
	| Some(x) -> x
	| None -> (Printf.fprintf stderr "hoscherei\n"; loop ()) 
    in 
    if not (is_telemetry next) then 
      (Printf.fprintf stderr "Event: %s\n" next;flush stderr;
      match event_of_string next with
	| Scored x -> Printf.fprintf stdout "We have scored %d points!" x; loop ()
	| _ -> loop ())
    else
      let t = telemetry_of_string next in
      
      if t.timestamp >= 13*1000 then
	wanted := (Accelerating,Left)
      else
	();
      if t.timestamp >= 16*1000 then
	wanted := (Accelerating,Right)
      else
	();
      
      (* this might result in awful slingering if requested is left or right and communication becomes an issue*)
      let command = Statemachines.both_change_to (t.speeding,t.turning) !wanted in
      Communication.sock_send socket (command2string command);
      loop ()
  in
  loop ()
  
    
let main = 
  try
    (* Communication.open_connection (Sys.argv.(0)) (int_of_string
       Sys.argv.(1)) *)
    let socket = Communication.connect "localhost" 17676 in
    stupid_loop_one_game socket 
  with Unix.Unix_error(code,_,_) as e -> Printf.fprintf stderr "%s\n" (Unix.error_message code);
    raise e
  
