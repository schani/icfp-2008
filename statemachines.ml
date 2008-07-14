
type turncmd = 
  | TurnLeft
  | TurnRight
  | Noturn
      
type speedcmd = 
  | Accelerate
  | Break
  | Nochange

type command = turncmd*speedcmd


type speedingmachinestates = Breaking | Rolling | Accelerating 

type turningmachinestates = HardLeft | Left | Straight | Right |
    HardRight


let speeding_change_to current wanted = 
  match current,wanted with
    | x,y when x=y -> Nochange
    | _,Accelerating -> Accelerate
    | _,Breaking -> Break
    | Accelerating,_ -> Break
    | Breaking,_ -> Accelerate

let turning_change_to current wanted = 
  match current,wanted with 
    | x,y when x=y -> Noturn
    | _,HardLeft -> TurnLeft
    | _,HardRight -> TurnRight
    | HardRight,_ -> TurnLeft
    | HardLeft,_ -> TurnRight
    | Left,_ -> TurnRight
    | Right,_ -> TurnLeft
    | _,Left -> TurnLeft
    | _,Right -> TurnRight
	

let both_change_to (cur_speed,cur_turn) (want_speed,want_turn) = 
  (speeding_change_to cur_speed want_speed),
  (turning_change_to cur_turn want_turn)
  
let turn2string = function 
  | TurnLeft -> "l"
  | TurnRight -> "r"
  | Noturn -> ""

let speed2string = function
  | Accelerate -> "a"
  | Break -> "b"
  | Nochange -> ""

let command2string = function speed,turn -> 
  let x = (speed2string speed)^(turn2string turn)^";" in
  (* if (String.compare ";" x) = 0 then
    x 
     else *)
    ((Printf.fprintf stderr "%s\n" x); x)

let string_of_state (s,t) = 
  (
    match s with 
      | Breaking -> "B"
      | Rolling -> "-"
      | Accelerating -> "A"
  )^(
    match t with 
      | HardRight -> "R"
      | Right -> "r"
      | Straight -> "-"
      | Left -> "l"
      | HardLeft -> "L"
  )

let apply_command (speedcmd,turncmd) (speeding,turning) = 
  let newspeeding = 
    match speeding,speedcmd with
      | Accelerating, Break -> Rolling
      | Rolling, Break -> Breaking
      | Rolling, Accelerate -> Accelerating
      | Breaking, Accelerate -> Rolling
      | _ -> speeding
  in
  let newturning = 
    match turning,turncmd with
      | HardLeft, TurnRight -> Left
      | Left, TurnRight -> Straight
      | Left, TurnLeft -> HardLeft
      | Straight, TurnRight -> Right
      | Straight, TurnLeft -> Left
      | Right, TurnRight -> HardRight
      | Right, TurnLeft -> Straight
      | HardRight, TurnLeft -> Right
      | _ -> turning
  in
  newspeeding,newturning
