open Statemachines
open Telemetry

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
      ((*Printf.fprintf stderr "Event: %s\n" next;flush stderr;*)
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
  


