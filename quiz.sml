(*Sum of multiples of 4*)
fun sumMult(a)= List.foldl (fn (x,y)=>x+y) 0 (List.filter (fn x=> x mod 4 =0) a);(*How I should have done it*)

(*	case a of
		[]=>0
		| h::t=> (if h mod 4=0 then h else 0) + sumMult(t);*)(*How I actually did it*)

print (Int.toString(sumMult([4,5,8,7]))^"\n");
