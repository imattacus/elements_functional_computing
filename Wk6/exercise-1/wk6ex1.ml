(* let rec search n = function
	|[]			-> [n]
	|x :: xs	-> if n = x then xs else x :: (search n xs);;

let rec dif xs ys = match (xs, ys) with
	|([], []) -> []
	|(xs, []) -> xs
	|([], ys) -> ys
	|(xs, y::ys) -> dif xs (search y xs);; *)

let rec replace n ns result = match ns with 
	|[]	-> result @ [n]
	|x::xs -> if n = (x:int) then result @ xs else replace n xs (result @ [x]);;

let rec dif xs ys = match (xs, ys) with
	|([],[]) -> []
	|(xs,[]) -> xs
	|([],ys) -> ys
	|(xs, y::ys) -> dif (replace y xs []) ys;;