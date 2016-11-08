let rec greater a b = match a, b with
|(_, a), (_, b) -> if a > b then true else false;;

let rec enq x q = match q with
|[]	-> [x]
|q::qs when greater x q -> x::q::qs
|q::qs	-> q::(enq x qs);;

let deq (q:('a * int) list) = match q with
|[] -> failwith "deq"
|(a, _)::qs	-> (a, qs);;
