let rec nextEdge edge ns frontier = match (ns, edge) with
	|([], _)	-> frontier
	|((nA, nB)::ns, (a, b))	-> if b = nA then nextEdge edge ns (frontier @ [(nA, nB)]) else nextEdge edge ns frontier;;

let rec path origin ns = function
	|[] 	-> false
	|f::frontier -> 

let searchFrontier origin edge ns frontier = match frontier with
	|x :: f 	-> 

let rec loop = function
	|[]						-> false
	|(a, b)::xs when a=c 	-> true
	|(a, b)::xs 			-> for each path test if there is a loop return true or false
