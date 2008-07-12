open Statemachines
open Telemetry

let dir_of_turn w turn = 
  if abs_float (turn) < w.world_straight_max then 
    Straight
  else if turn > 0. then
    if turn /. w.world_init.imax_turn > 5. then 
      HardLeft
    else
      Left
  else     
    if (-. turn) /. w.world_init.imax_turn > 5. then 
      HardRight
    else
      Right

let rel_angle_to_obj t o = 
  let angle = Geometry.angle_to_point (t.x,t.y) (o.bcx,o.bcy) in
  let rel_angle = Geometry.turn_towards_dstangle angle t.dir in
  rel_angle

let is_in_anglar_interval needle hayangle haywidth = 
  let diff = abs_float (needle -. hayangle) in
  let diff = if diff > 180. then 
    360. -. diff
  else
    diff
  in
  diff < haywidth


let find_all_blocking_objs t dst_angle objs = 
  let mypos = t.x,t.y in
  List.find_all (fun o -> 
    let ocoord = (o.bcx,o.bcy) in
    let width = Geometry.passing_angle mypos ocoord (o.bcr) in
    let angle = Geometry.rel_angle_to_point t.dir mypos ocoord in
    (* Printf.fprintf stderr "angle=%f width=%f radius=%d \n" angle width o.bcr; *)
    is_in_anglar_interval dst_angle angle (5. +. width)
  ) objs

let print_vec os name vec = 
  let x,y = vec in
  Printf.fprintf os "%s=(%d,%d)  " name x y
  
let obj_ahead_p t dst_angle objs= 
  let ahead = find_all_blocking_objs t dst_angle objs in
  (List.length ahead) > 0

let evade_objs w t turn objs = 
  let mypos = (t.x,t.y) in
  let ahead = find_all_blocking_objs t turn objs in 
  let x = List.length ahead in
  
  Printf.fprintf stderr "Found %d bad craters!\n" x;

  let distfromme o = Geometry.distanceSq mypos (o.bcx,o.bcy) in
  let ahead = List.sort (fun a b -> (distfromme a)-(distfromme b) ) ahead in
  
  let badguy = List.hd ahead in
  
  let badpos = (badguy.bcx,badguy.bcy) in

  let badvec = Geometry.rel_pos mypos badpos in
  
  let normvec = 
    if Geometry.angle_left_of t.dir (Geometry.angle_to_point mypos badpos) then
      Geometry.norm_vec_left 
    else
      Geometry.norm_vec_right
  in
  let norm = Geometry.scale_vec (normvec badvec) (badguy.bcr+2000) in
  let evade_dst = Geometry.abs_pos mypos (Geometry.abs_pos badvec norm) in
  

  print_vec stderr "mypos" mypos;
  print_vec stderr "badvec" badvec;
  print_vec stderr "norm" norm;
  print_vec stderr "evadedst" evade_dst;
  print_vec stderr "badpos" badpos;

  Printf.fprintf stderr "rad=%d\n" badguy.bcr;
  
  let evade_angle = Geometry.rel_angle_to_point t.dir mypos evade_dst in
  Printf.fprintf stderr "evading to %f (%d craters ahead) \n" evade_angle x;
  
  let speeding = 
    if t.speed*t.speed > 3*(Geometry.distanceSq mypos badpos) then 
      Breaking
    else if t.speed*t.speed > (Geometry.distanceSq mypos badpos) then 
      Rolling
    else
      Accelerating
  in
  speeding,(dir_of_turn w evade_angle)
   
 
let great_decision_procedure w t = 
  let dst = w.world_dst in
  let objs =  (List.append t.craters t.boulders) in
  let turn = Geometry.rel_angle_to_point t.dir (t.x,t.y) dst in
  flush stderr;
  let want_dir = dir_of_turn w turn in
  
  if obj_ahead_p t turn objs then 
    evade_objs w t turn objs
  else
    Accelerating,want_dir

      
let precalculation_hook x = x

let check_speed world t (wantspeed,wantturn) = 
  if t.speed < world.world_min_speed then
    Accelerating,wantturn
  else if (Geometry.distanceSq (t.x,t.y) world.world_dst) < t.speed*t.speed then
    Breaking,wantturn
  else
    wantspeed,wantturn
    

let stupid_loop_one_game socket = 
  let _ = Communication.waitfordata socket in
  let init = match Communication.sock_recv_next socket with
    | Some(x) -> x
    | None -> failwith "zeugs"
  in
  let init = initialization_of_string init in
  let world = {
    world_init = init;
    world_vehicle_state = Breaking,Straight;
    world_straight_max = 0.1;
    world_min_speed = (init.imax_sensor / 10); 
    world_dst = 0,0;
  }
  in
  
  let rec loop world = 
    if not (Communication.is_dataavailable socket) then
      let world = precalculation_hook world in loop world
    else
      let _ = Communication.waitfordata socket in
      let next = 
	match Communication.sock_recv_next socket with 
	  | Some(x) -> x
	  | None -> (Printf.fprintf stderr "hoscherei\n"; loop world) 
      in 
      if not (is_telemetry next) then 
	(Printf.fprintf stderr "Event: %s\n" next;flush stderr;
	match event_of_string next with
	  | Scored x -> Printf.fprintf stderr "We have scored %d points!\n" x;flush stderr; loop world
	  | BoulderHit | CraterFall | Killed | Success -> loop world
	)
      else
	begin
	  let t = telemetry_of_string next in
	  
	  let world = merge_telemetry_into_world world t in
	  let wantedstate = great_decision_procedure world t in
	  
	  let wantedstate = check_speed world t wantedstate in
	  
	  (* 
	     Printf.fprintf stderr "\nWant: %s\n Have: %s\nReported: %s\n" 
	     (string_of_state wantedstate) 
	     (string_of_state world.world_vehicle_state) 
	     (string_of_state (t.speeding,t.turning));
	  *)
	  
	  let command = Statemachines.both_change_to world.world_vehicle_state wantedstate in
	  let world = {world with world_vehicle_state = (apply_command command world.world_vehicle_state)} in
	  Communication.sock_send socket (command2string command);
	  loop world
	end
  in
  loop world
    
    
let main = 
  try
    (* Communication.open_connection (Sys.argv.(0)) (int_of_string
       Sys.argv.(1)) *)
    let socket = Communication.connect "localhost" 17676 in
    stupid_loop_one_game socket 
  with Unix.Unix_error(code,_,_) as e -> Printf.fprintf stderr "%s\n" (Unix.error_message code);
    raise e
  


