let enq a (xs, sx) = (a :: xs, sx);;

let deq (xs, sx) = match sx with
  | x :: sx -> x, (xs, sx)
  | [] -> if xs = []
    then failwith "deq"
    else let sx = List.rev xs in
      (List.hd sx, ([], List.tl sx));;


let rec has_seg' xs v q sum = if sum = v then true else match q, xs with
|([], []), []		-> false
|([], []), x::xs	-> has_seg' xs v (enq x q) x 
|(_, _), x::xs when sum < v	-> has_seg' xs v (enq x q) (sum + x)
|(_, _), xs when sum > v	-> let h2, q2 = deq q in has_seg' xs v q2 (sum - h2)
|(_, _), _ 	-> false;;

let has_seg xs v = has_seg' xs v ([], []) 0;;

