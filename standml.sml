(*List to String*)
fun toString(xs):string = 
  case xs of
       [] => ""
     | h::t => Int.toString(h)^", "^toString(t);


(*Hypotenuse*)
fun hyp(base:real,height:real) = Math.sqrt(Math.pow(base,2.0) + Math.pow(height,2.0));

print "Hypotenuse\n";
print (Real.toString(hyp(3.0,4.0))^"\n\n");


(*Volume of a Sphere*)
fun sphereVol(radius) = (4.0/3.0)*Math.pi*Math.pow(radius,3.0);

print "Volume of a Sphere:\n";
print (Real.toString(sphereVol(10.0))^"\n\n");


(*State of Water*)
fun watState(temp) = if temp <= 32 then "ice" else if temp < 212 then "water"
                                                  else "gas";
print "State of Water:\n";
print ("at 32 degrees: "^watState(32)^"\n");
print ("at 100 degrees: "^watState(100)^"\n");
print ("at 300 degrees: "^watState(300)^"\n\n");


(*Dot Product*)
val dotRealLists = ListPair.foldlEq Real.*+0.0;

print "Dot Product\n";
print (Real.toString(dotRealLists([1.0,2.0,3.0],[4.0,5.0,6.0]))^"\n\n");


(*Double Sequence*)
fun doubSeq(a) = List.map (fn x=>x*2) a;

print "Double Sequence:\n";
print (toString(doubSeq([1,2,3])));
print"\n\n";


(*Duplicate Sequence*)
fun dupSeq(a) = List.concat[a,a];

print "Duplicate Sequence:\n";
print (toString(dupSeq([1,2,3]))^"\n\n");


(*Remove Duplicates*)
fun removeDup(xs) = 
  case xs of
       [] => []
     | h::t => h::(removeDup(List.filter (fn y => y<> h) xs));

print "Remove Duplicates:\n";
print (toString(removeDup([1,2,1,3,4,3]))^"\n\n");


(*Translate*)
fun translate(a) = List.map ( fn x =>
  if x = "one" then "uno"
  else
    if x = "two" then "dos"
    else
      if x = "three" then "tres"
      else "no se") a;

print "Translate:\n";
fun printList(xs) =
  case xs of
       [] => ""
     | h::t => h^", "^printList(t);
print (printList(translate(["one", "two", "three"]))^"\n\n");


(*Sum and Product*)
fun procedure(a, f, b) = List.foldl f b a;

print "Sum and Product:\n";
print (Real.toString(procedure([1.0,2.0,3.0],(fn(x,a)=>a+x),0.0))^"\n");
print (Real.toString(procedure([2.0,3.0,4.0], (fn(x,a)=>a*x), 1.0))^"\n\n");


(*Count Articles*)
fun countArt(a) = List.length (List.filter (fn word=> "a"= word orelse "an"=word orelse
  "the"=word) a);

print "Count Articles:\n";
print (Int.toString(countArt(["you", "are","a","the"]))^"\n\n");


(*List of Colors*)
val colors=["black","brown","blue","red","yellow","orange","purple","green","gray", "pink"];

fun listCol(a) = List.filter (fn x=> List.exists (fn y=> y=x) colors) a;

print "List of Colors:\n";
print (printList(listCol(["a","blue","green", "person", "is", "red"]))^"\n\n");


(*List of Positives*)
fun listPos(a) = List.filter (fn x=>x>0) a;

print "List of Positives:\n";
print (toString(listPos([1,2,0,~4,5,~6]))^"\n\n");


(*Max*)
fun max(a) =
  case a of
       []=>0
     | h::t => Int.max(h, max(t));

print "Maximum number in List:\n";
print (Int.toString(max([5,3,6,7,2]))^"\n\n");


(*Nest Level*)(*
fun nest(x,a) =
  case a of
       ([]::[]) => 0
     |((h::t)::L) =>
      let val level = nest(x, h::t)
       in
         if level <> 0 then level+1 else nest(x,t)
       end
     | (h::(t::L)) => if h=x then 1 else nest(x,t);
     *)


(*First N Square Numbers*)
fun nSquare(a) = List.tabulate(a, (fn x=>(x+1)*(x+1)));

print "N Square Numbers:\n";
print ("4->"^toString(nSquare(4))^"\n");
print ("10->"^toString(nSquare(10))^"\n\n");



(*Person Structure*)
type Person = { name:string, age:int };


(*Youngest Person*)
fun youngest(a:Person list) = 
  case a of
       [] => { name="No one", age=1000}
     | h::t => let val young = youngest(t)
               in
                 if #age h < #age young then h else young
               end;


val p1 = { name="Katrina", age=21 };
val p2 = { name="Jacob", age=24 };
val p3 = { name="Oliver", age=2 };
val p4 = { name="JoAnn", age=50 };

print "Youngest Person:\n";
print (#name (youngest([p1,p2,p3,p4]))^"\n\n");
