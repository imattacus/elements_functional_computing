open Num;;

let midpoint a b =
	let n = (a +/ b) // num_of_int 2 in 
	n;;

let rec root f (a:Num.num) (b:Num.num) (err:Num.num) =
	let c = midpoint a b in
	if (f c) = num_of_int 0 or (b -/ a) // (num_of_int 2) </ err then c else
	if sign_num (f c) = sign_num (f a) then root f c b err else root f a c err;;
	 



