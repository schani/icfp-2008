open Statemachines
open Telemetry

let roversize = 500 (* you want to supersize this? *)

let dir_of_turn w turn = 
  if abs_float (turn) < w.world_straight_max then 
    Straight
  else if turn > 0. then
    if turn > w.world_init.imax_turn then 
      HardLeft
    else
      Left
  else     
    if (-. turn) > w.world_init.imax_turn then 
      HardRight
    else
      Right

let get_max_rot w turning = 
  match turning with
    | HardRight | HardLeft -> w.world_init.imax_hard_turn 
    | Right | Left -> w.world_init.imax_turn
    | Straight -> w.world_straight_max 

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
  let mydistSq = Geometry.distanceSq mypos (0,0) in
  let visible_p o = 
    let ocoord = (o.bcx,o.bcy) in
    let width = Geometry.passing_angle mypos ocoord (o.bcr + roversize) in
    let angle = Geometry.rel_angle_to_point t.dir mypos ocoord in
    let disttobad = Geometry.distanceSq mypos ocoord in
    if mydistSq > disttobad then
      is_in_anglar_interval dst_angle angle width
    else
      false
  in 
  List.find_all visible_p objs

let print_vec os name vec = 
  let x,y = vec in
  Printf.fprintf os "%s=(%d,%d)  " name x y
  
let obj_ahead_p t dst_angle objs= 
  let ahead = find_all_blocking_objs t dst_angle objs in
  (List.length ahead) > 0

let time_to speed mypos therepos = 
  let dist = Geometry.distanceSq mypos therepos in
  dist / (speed * speed)

let decide_evasion_dir w t turn badguy evade_rel_angle = 
  let mypos = (t.x,t.y) in
  let badpos = (badguy.bcx,badguy.bcy) in
  let dist_to_badguy_surface =
    (sqrt (float_of_int (Geometry.distanceSq mypos badpos))) -. (float_of_int badguy.bcr)
  in
  let time_to_impact = dist_to_badguy_surface /. (1. +. (float_of_int t.speed)) in
  
  let max_rot_accel = w.world_acceleration_tracker.at_max_angular_accel in

  let have_max_rot = get_max_rot w t.turning in

  let angle_to_bad = (Geometry.angle_to_point mypos badpos) in

  let need_to_turn_l = (abs_float (t.dir -. (angle_to_bad +. evade_rel_angle))) in
  let need_to_turn_r = (abs_float (t.dir -. (angle_to_bad -. evade_rel_angle))) in
  
  (* let need_change_l =  *)

  let degrees_per_sec =  w.world_init.imax_hard_turn *. 0.5 in (* discount for possible turning change needs improvement *)
  
  let min_timetoturn_l =  need_to_turn_l /. degrees_per_sec in
  let min_timetoturn_r =  need_to_turn_r /. degrees_per_sec in

  let can_turn_l_before_impact = (min_timetoturn_l < time_to_impact) in
  let can_turn_r_before_impact = (min_timetoturn_r < time_to_impact) in
  
  let curr_dir = 
    if Geometry.angle_left_of t.dir angle_to_bad then
      `PreferEvadeLeft
    else
      `PreferEvadeRight
  in
  let home_dir = 
    if Geometry.angle_left_of (Geometry.angle_to_point mypos (0,0)) (Geometry.angle_to_point mypos badpos) then
      `PreferEvadeLeft
    else
      `PreferEvadeRight
  in
  
  Printf.fprintf stderr "Home is to the %s of badboy\n" (match home_dir with `PreferEvadeRight -> "Right" | _ -> "Left"); 
  
  if home_dir = curr_dir then
    match home_dir with `PreferEvadeRight -> `EvadeRight | `PreferEvadeLeft -> `EvadeLeft 
  else
    if (home_dir = `PreferEvadeLeft) then
      if can_turn_l_before_impact then
	`EvadeLeft
      else
	`EvadeRight
    else (* homedir = `PreferEvadeRight *)
      if can_turn_r_before_impact then
	`EvadeRight
      else
	`EvadeLeft


let evade_obj w t turn badguy evadeto = 
  let mypos = (t.x,t.y) in
  let badpos = (badguy.bcx,badguy.bcy) in
  let evade_rel_angle = Geometry.passing_angle mypos badpos (badguy.bcr + roversize) in
  
  let evadeto = match evadeto with 
    | (`EvadeLeft | `EvadeRight) as x -> x
    | `EvadeDunno ->
	decide_evasion_dir w t turn badguy evade_rel_angle
  in
  Printf.fprintf stderr "Evading to %s\n" (match evadeto with `EvadeLeft-> "Left" | `EvadeRight -> "Right");  
  let evade_angle = 
    (match evadeto with `EvadeLeft -> (+.) | _ -> (-.)) 
      turn evade_rel_angle
  in
  evade_angle,evadeto


let evade_objs w t turn ahead evadeto = 
  let mypos = (t.x,t.y) in
  let distfromme o = Geometry.distanceSq mypos (o.bcx,o.bcy) in
  let ahead = List.sort (fun a b -> (distfromme a)-(distfromme b) ) ahead in
  let rec loop turn evadeto = function
    | [] -> turn,evadeto 
    | badguy::rest -> 
      let turn,evadeto = evade_obj w t turn badguy evadeto in
      loop turn evadeto rest
  in
  loop turn evadeto ahead


let evade_if_necessary w t dir_to_dst = 
  let rec loop depth direction evadeto = 
    let depth = depth + 1 in
    let visible_objs =  (List.append t.craters t.boulders) in
    let ahead = find_all_blocking_objs t direction visible_objs in 
    let l = List.length ahead in
    Printf.fprintf stderr "Found %d bad craters!\n" l; 
    if l > 0 && depth < 3 then 
      let new_dir,evadeto = evade_objs w t direction ahead evadeto in
      loop depth new_dir evadeto
    else
      direction
  in
  loop 0 dir_to_dst `EvadeDunno
    

let turncategory world turn = 
  if turn < world.world_init.imax_turn /. 3. then
    `NoTurnReally
  else if turn < (world.world_init.imax_turn +. world.world_init.imax_hard_turn) /. 2. then
    `MediumTurn
  else
    `HardTurn
    

let calc_speed world t turn = 
  let dist_left = 2 * (Geometry.distanceSq (t.x,t.y) (0,0)) in
  let timeleft = world.world_init.itime_limit - t.timestamp in
  let secs_left = timeleft / 1000 in
  let neededspeedSq = dist_left / (1 + secs_left * secs_left) in
  let howto_time = 
    if neededspeedSq > (t.speed * t.speed) then
      `Faster
    else if dist_left < world.world_really_close then
      `Slower
    else
      `Dontcare
  in
  let howto_turn = match turncategory world turn with
    | `HardTurn -> `Slower
    | `MediumTurn -> `Dontcare
    | `NoTurnReally -> `Faster
  in
  let whattodo = match howto_time,howto_turn with
    | `Faster,`Slower -> `Dontcare
    | `Faster,x -> `Faster
    |  _,x -> x
  in
  let whattodo = 
    if t.speed < world.world_min_speed then
      `Faster
    else if t.speed > world.world_max_speed then 
      (Printf.fprintf stderr "ABOVE MAX\n\n\n";
      `Slower)
    else
      whattodo 
  in
  match whattodo with
    | `Faster -> Accelerating
    | `Slower -> Breaking
    | `Dontcare -> Accelerating
	
 
let small_decision_procedure w t = 
  let dst = w.world_dst in
  let dir_to_dst = Geometry.rel_angle_to_point t.dir (t.x,t.y) dst in
  let turn = evade_if_necessary w t dir_to_dst in
  let speed = calc_speed w t turn in
  speed,(dir_of_turn w turn)

      
let world_init socket =
  let _ = Communication.waitfordata socket in
  let init = match Communication.sock_recv_next socket with
    | Some(x) -> x
    | None -> failwith "zeugs"
  in
  let init = initialization_of_string init in
    {
      world_init = init;
      world_vehicle_state = Breaking,Straight;
      world_straight_max = 0.01;
      world_min_speed = (init.imax_sensor / 10); 
      world_max_speed = (3 * init.imax_sensor + init.imin_sensor) / 5;
      world_dst = 0,0;
      world_really_close = 50*1000;
      world_board = Discrete.create_board 51 51 init.idx init.idy
	init.imin_sensor init.imax_sensor;
      world_last_step = Event;
      world_acceleration_tracker = init_accel_tracker;
    }

let world_step world socket =
  flush stderr;
  match Communication.sock_recv_next socket with 
    | Some(next) ->
	if not (is_telemetry next) then 
	  let e = event_of_string next in
	  let world = merge_nontelemetry_into_world world e in
	  (Printf.fprintf stderr "Event: %s\n" next;flush stderr;
	   match e with
	     | Scored x ->
		 Printf.fprintf stderr "We have scored %d points! %f\n" x world.world_acceleration_tracker.at_max_angular_accel;
		 flush stderr;
		 world
	     | BoulderHit | CraterFall | Killed | Success ->
		 world
	  )
	else
	  begin
	    let t = telemetry_of_string next in
	      List.iter (Discrete.register_boldercrater world.world_board)
		t.boulders;
	      List.iter (Discrete.register_boldercrater world.world_board)
		t.craters;
	    let world = merge_telemetry_into_world world t in
	    let wantedstate = small_decision_procedure world t in
	      
	    
             (* 
		if world.world_vehicle_state != wantedstate then
		Printf.fprintf stderr "\nWant: %s\n Have: %s\nReported: %s\n" 
		(string_of_state wantedstate) 
		(string_of_state world.world_vehicle_state) 
		(string_of_state (t.speeding,t.turning));
	     *)
	    
	    let rec sending_loop world = 
	      let command = Statemachines.both_change_to world.world_vehicle_state wantedstate in
	      let world = {world with world_vehicle_state = (apply_command command world.world_vehicle_state)} in
	      let command_string = (command2string command) in
	      Communication.sock_send socket command_string;
	      if world.world_vehicle_state = wantedstate then
		world
	      else
		sending_loop world
	    in
	    sending_loop world
	  end
    | None ->
	Printf.fprintf stderr "hoscherei\n";
	world 

let stupid_loop_one_game socket =
  let world = world_init socket
  in
  let rec loop world maxtime = 
    let _ = Communication.waitfordata socket in
    (* let tstart = Sys.time () in *)
    let world =  (world_step world socket) in
    (* let tend = Sys.time () in 
       let tdiff =  tend -. tstart in 
       let maxtime = if tdiff > maxtime then (Printf.fprintf stderr "new slowness record %f\n" tdiff; tdiff) else maxtime in *)
    loop world maxtime
	
  in
    loop world 0.

let create_socket () =
  Communication.connect (* (Sys.argv.(1)) (int_of_string Sys.argv.(2)) *) "localhost" 17676

let main () = 
  try
    (* Communication.open_connection *)
    let socket = create_socket ()
    in
      stupid_loop_one_game socket 
  with Unix.Unix_error(code,_,_) as e -> Printf.fprintf stderr "%s\n" (Unix.error_message code);
    raise e
  


