(* Do not delete this line. This includes the nat type from common.ml
   Do not redefine the nat type. *)
open Common;;


let remove_suc = function
|Zero	-> Zero
|Suc (x)-> x;;

let rec sub x y = match x, y with
|Zero, Zero -> Zero
|Zero, y 	-> failwith "sub"
|x, Zero	-> x
|x, y 		-> sub (remove_suc x) (remove_suc y);;
