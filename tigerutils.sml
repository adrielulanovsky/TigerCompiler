(* Encuentra el primer duplicado de una lista, utilizando
   como funcion de comparacion la que recibe como argumento *)
fun findDupCmp _ [] = NONE
  | findDupCmp f (x::xs) = case List.find ( fn y => f (x, y) ) xs 
					                    of SOME z => SOME z
					                     | NONE => findDupCmp f xs 
					     
(* Encuentra el primer duplicado de una lista, comparando
	 los elementos con (op =) *)					                     
fun findDup xs = findDupCmp (op =) xs

(* Recorre dos listas y genera un tercera formada por
	 la aplicacion sucesiva de la funcion pasada a los
	 elementos de estas. *)	
fun zipWith f xs ys = List.map f ( ListPair.zip (xs, ys) )

(* Funcion identidad *)
fun id x = x

(* True si todos los elementos de la lista son true *)
val andList = List.all id

(*
    (* Genera el intervalo [a..b] *)
    fun interval a b = 
        if a < b then a :: interval (a+1) b 
        else if a = b then [a]
        else []
        
    (* Genera el intervalo [1..n] *)
    fun upTo n = interval 0 (n - 1)
*)
(* Genera el intervalo [0..n] *)
fun upTo n = List.tabulate (n, id)

(* Devuelve true si el elemento x esta en l *)
fun isinList x l = List.exists (fn y => x = y) l

(* Devuelve true si algun elemento de l1 esta en l2 *)
fun isAnyInList l1 l2 = List.exists (fn x => isinList x l2) l1

(* Union de las listas *)
fun unionList l1 [] = l1 
  | unionList l1 (x::xs) = if isinList x l1 then unionList l1 xs
                                            else x::(unionList l1 xs)

(* Union generalizada de las listas *)
fun concatUnionList [] = []
  | concatUnionList (xs::xxs) = unionList xs (concatUnionList xxs)

(*
fun concatUnionList xxs = List.foldr [] unionList
*)

(* Resta de las listas *)
fun diffList l1 l2 = List.filter (fn x => not (isinList x l2)) l1


fun removeDups' xs [] = xs
  | removeDups' xs (y::ys) = if isinList y xs then removeDups' xs ys else removeDups' (y::xs) ys

fun removeDups xs = removeDups' [] xs

type 'a stack = 'a list

val emptyStack = []

fun pop (ref []) = raise Fail "Pop a stack vacio"
  | pop (s as ref (x :: xs)) = (s := xs; x)
  
fun push x (s as ref xs) = s := (x::xs)

fun curry f x y = f (x, y)

fun showList xs = "[" ^ (String.concatWith ", " xs) ^ "]"

and showOptionList (SOME []) = "SOME []"
  | showOptionList (SOME (x::xs)) = "SOME " ^ showList (x::xs)
  | showOptionList NONE = "NONE"
  
fun forall (f: 'a -> bool) (s: 'a Splayset.set) =
    case Splayset.find (not o f) s
      of SOME x => false
       | NONE => true

fun minBy (cmp: 'a * 'a -> order) (xs: 'a list) =
    case xs
      of [] => raise Fail "minBy en lista vacia"
       | [x] => x
       | (x::y::zs) => let val m = minBy cmp (y::zs)
                       in case cmp (x, m)
                            of GREATER => m
                             | _ => x
                       end

fun remove x = List.filter (fn y => x<>y)

fun intersect xs ys = List.filter (fn y => isinList y xs) ys

(* nuestra intToString *)
fun intToString i = if i >= 0 then Int.toString i else "-" ^ (Int.toString ((~1)*i))

 
