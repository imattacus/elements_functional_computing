let rec slumpcount t ts c (i:int) = match ts with 
	|[] -> (c,i)
	|nt::ts -> if nt < t then slumpcount t ts (c + 1) i else (c, i);;

let rec eachSlump ts i = match ts with
	|[] -> [(0,0)];
	|t::ts -> [slumpcount t ts 0 i] @ eachSlump ts (i+1);;

let rec largestSlump largest ts = match (ts, largest) with
	|([], (a, b))	-> (a, b);
	|((a,b) :: ts, (nA, nB)) -> if a > nA then largestSlump (a, b) ts else largestSlump largest ts;;  

let rec slump ts = match eachSlump ts 0 with
	|[]	-> (0,0)
	|xs -> largestSlump (0,0) (eachSlump ts 0);;