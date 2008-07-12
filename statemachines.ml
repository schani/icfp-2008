
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
  (speed2string speed)^(turn2string turn)^";"

