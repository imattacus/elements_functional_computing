let rec replace x y xs acc = match xs with
|[]	-> acc;
|z::zs -> if z = x then replace x y zs (acc@[y]) else replace x y zs (acc@[z]);;

let replace x y xs = replace x y xs [];;
