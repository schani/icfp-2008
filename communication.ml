
let connect name port = 
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let hentry = Unix.gethostbyname name in
  Unix.connect sock (Unix.ADDR_INET (hentry.Unix.h_addr_list.(0), port));
  Unix.set_nonblock sock;
  sock

(*  let hentry = Unix.gethostbyname name in
  Unix.open_connection (Unix.ADDR_INET (hentry.Unix.h_addr_list.(0),
    port))
*)

	

let select_read socket timeout = 
  Unix.select [socket] [] [] timeout 

let waitfordata socket = 
  let rec loop () = 
    match select_read socket (-1.) with 
      | [],_,_ -> Printf.fprintf stderr "strange\n"; loop ()
      | [x],_,_ -> (* Printf.fprintf stderr "available\n"; *) ignore(x)
      | _ -> failwith "mindbogling select bug"
  in
  loop ()
      

let sock_data_avail socket = 
    match select_read socket 0.0 with
      | [],_,_ -> false
      | _ -> true

let sock_send sock str =
  let len = String.length str in
  ignore(Unix.send sock str 0 len [])

let sock_recv_all sock =
  let buffer = String.create 512 in
  let rec loop acc = 
    if sock_data_avail sock then
      let count = Unix.recv sock buffer 0 512 [] in
      (* Printf.fprintf stderr "read %d\n" count; *)
      if count = 0 then 
	acc 
      else
	loop (acc^(String.sub buffer 0 count))
    else
      acc
  in
  loop ""

let buffer = ref ""
let regex = Str.regexp ";"

let sock_recv_next socket = 
  buffer:=(!buffer^(sock_recv_all socket));
  let x = Str.bounded_split regex !buffer 2 in
(*Printf.fprintf stderr "XXXX %d %d >%s<\n" (List.length x)
    (String.length !buffer) !buffer; 
  flush stderr;*)
  match x with
    | [cmd;rest] -> (buffer:=rest; Some cmd) 
    | [cmd] -> buffer:=""; Some cmd
    | _ -> None

let is_dataavailable socket = 
  ((String.length !buffer)>0) or (sock_data_avail socket)


