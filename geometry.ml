
let pi = 3.1415926535897932384626433



let angle_to_point (xpos,ypos) (xdst,ydst) = 
  let y = float_of_int (ydst-ypos) in
  let x = float_of_int (xdst-xpos) in
  let phi = (atan2 y x)/.pi*.180. in
  if phi < 0. then
    phi +. 360. 
  else
    phi

let turn_towards_dstangle soll haben = 
  let rel_angle = soll -. haben in
  if rel_angle > 180. then
    -. (rel_angle -. 180.)
  else
    rel_angle

let rel_angle_to_point myangle mypos dst = 
  let angle = angle_to_point mypos dst in
  let turn = turn_towards_dstangle angle myangle  in
  turn


let distanceSq (x1,y1) (x2,y2) = 
  let x = x1-x2 in
  let y = y1-y2 in 
  x*x + y*y

(* not exact value! *)
let distanceSqFromCircle xy1 xy2 r2 = 
  let d = distanceSq xy1 xy2 in
  d - r2*r2

let rel_pos (x1,y1) (x2,y2) = 
  let x = x2-x1 in
  let y = y2-y1 in 
  x,y

(* some normal vector ... *)
let norm_vec (x1,y1) = 
  (-y1,x1)

let scale_vec (x,y) len = 
  let wantlen = float_of_int len in
  let reallen = sqrt (float_of_int (x*x + y*y)) in
  let fact = wantlen /. reallen in
  let scale z = (int_of_float (fact *. (float_of_int z))) in
  (scale x),(scale y)
  
let abs_pos (x,y) (xrel,yrel) = 
  (x+xrel),(y+yrel)


let midpoint (x1,y1) (x2,y2) = 
  (x1+x2)/2,(y1+y2)/2



(* calculate relative (to direction towards center) angle to pass by circle *)
let passing_angle mypos center radius = 
  let dist = sqrt (float_of_int (distanceSq mypos center)) in
  let sin = (float_of_int radius)/.dist in
  let sin = if sin > 1. then 1. else if sin < (-1.) then (-1.) else sin in
  let asin = asin sin in
  (* Printf.fprintf stderr "asin(%f/%f) = %f" (float_of_int radius) dist asin; *)
  asin

