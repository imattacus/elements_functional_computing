let rec remove_chunk y xs = match xs with
|[] -> []
|x::xs -> if x = y then remove_chunk y xs else x::xs;;

let rec cut xs n acc = match xs with
|[] -> acc
|x::xs -> if x = n then cut xs n (acc@[n]) else acc;;

let rec listcut xs acc = match xs with
|[] -> acc
|z::zs -> listcut (remove_chunk z xs) ((cut xs z [])::acc);;

let split xs = match xs with
|[] -> []
|xs -> List.rev (listcut xs []);;
