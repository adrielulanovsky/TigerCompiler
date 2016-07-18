(*
	Frames para el 80386 (sin displays ni registers).

		|    argn    |	fp+4*(n+1)
		|    ...     |
		|    arg2    |	fp+16
		|    arg1    |	fp+12
		|	fp level |  fp+8
		|  retorno   |	fp+4
		|   fp ant   |	fp
		--------------	fp
		|   local1   |	fp-4
		|   local2   |	fp-8
		|    ...     |
		|   localn   |	fp-4*n
*)

structure tigerframe :> tigerframe = struct

open tigertree

type level = int
type register = string

val fp = "%ebp"				(* frame pointer *)
val sp = "%esp"				(* stack pointer *)
val rv = "%eax"				(* return value  *)
val ov = "%edx"				(* overflow value (edx en el 386) *)
val wSz = 4					(* word size in bytes *)
val log2WSz = 2				(* base two logarithm of word size in bytes *)
val fpPrev = 0				(* offset (bytes) *)
val fpPrevLev = 8			(* offset (bytes) *)
val argsInicial = 0			(* words *)
val argsOffInicial = 3		(* words *)
val argsGap = wSz			(* bytes *)
val regInicial = 1			(* reg *)
val localsInicial = 0		(* words *)
val localsGap = ~4 			(* bytes *)
val calldefs = [rv]
val specialregs = [rv, fp, sp]
val argregs = []
val callersaves = ["%eax", "%edx", "%ecx"]
val calleesaves = ["%ebx", "%edi", "%esi"]
(*COMPLETAR*)
val registers = callersaves @ calleesaves

val tempMap = tigertab.tabInserList( tigertab.tabNueva(), ListPair.zip (registers, registers))
(*COMPLETAR*)

datatype access = InFrame of int | InReg of tigertemp.label
fun showAcc (InFrame i) = "InFrame " ^ Int.toString(i)
  | showAcc (InReg l) = "InReg " ^ l
  
type frame = {
	name: string,
	formals: bool list,
	formalsAcc: access list ref,
	locals: bool list,
	actualArg: int ref,
	actualLocal: int ref,
	actualReg: int ref
}
type register = string  

datatype frag = PROC of {body: tigertree.stm, frame: frame}
	| STRING of tigertemp.label * string
	
fun newFrame{name, formals} = {
	name=name,
	formals=formals,
	formalsAcc=ref [InFrame (fpPrevLev)],
	locals=[],
	actualArg=ref argsInicial,
	actualLocal=ref localsInicial,
	actualReg=ref regInicial
}
fun name(f: frame) = #name f
fun string(l, s) = l^tigertemp.makeString(s)^"\n"


	
fun maxRegFrame(f: frame) = !(#actualReg f)

fun allocArg (f: frame) b = 
(*	case b of *)
    case true of
	true =>
		let	val ret = (!(#actualArg f)+argsOffInicial)*wSz
			val _ = #actualArg f := !(#actualArg f)+1
		in	((*print("\t\tAlocando argumento de frame " ^ #name f ^ " en posicion " ^ Int.toString(ret) ^ "\n");*)
		    (* #formalsAcc(f) := (!(#formalsAcc(f)))@[InFrame ret];*)
		     #formalsAcc(f) := hd (!(#formalsAcc(f))) :: (InFrame ret) :: tl((!(#formalsAcc(f))));
		     InFrame ret) end
	| false => let val t = tigertemp.newtemp()
	               val _ = #formalsAcc(f) := (InReg t) :: (!(#formalsAcc(f)))
	           in
	               InReg(t)
	           end
	           
(*fun formals({formals=f, ...}: frame) = *)
    (* aux: int * frame-> access list  
	let	fun aux(n, []) = []
		| aux(n, h::t) = InFrame(n)::aux(n+argsGap, t)
	in aux(argsInicial, f) end*)
fun formals (f: frame) = 
        ((*tigerpp.printIf(10, "formals: " ^ String.concatWith ", " (List.map Bool.toString (#formals(f))) ^ "\n");
        tigerpp.printIf(10, "formalsAcc: " ^ String.concatWith ", " (List.map showAcc ((!(#formalsAcc(f))))) ^ "\n");*)
        (!(#formalsAcc(f)))
	    )
	
fun allocLocal (f: frame) b = 
	case true of
	true =>
	  let	val ret = InFrame((!(#actualLocal f)) * wSz +localsGap) 
		(*let	val ret = InFrame(!(#actualLocal f)+localsGap)*)
		in	(*(print("Alocando local de frame " ^ #name f ^ " en posicion " ^ Int.toString(!(#actualLocal f)+localsGap) ^ "\n");*)
		     #actualLocal f:=(!(#actualLocal f)-1); ret end
	| false => InReg(tigertemp.newtemp())
	
fun exp(InFrame k) = MEM(BINOP(PLUS, TEMP(fp), CONST k))
| exp(InReg l) = TEMP l

fun externalCall(s, l) = CALL(NAME s, l)

(* COMPLETAR? *)
fun seq [] = EXP (CONST 0)
	| seq [s] = s
	| seq (x::xs) = SEQ (x, seq xs)
	
fun procEntryExit1 (frame,body) = 
    let val temps = List.map (fn _ => tigertemp.newtemp()) calleesaves
        val moves = tigerutils.zipWith (fn (t, r) => MOVE(TEMP t, TEMP r)) temps calleesaves
        val unMoves = tigerutils.zipWith (fn (t, r) => MOVE(TEMP t, TEMP r)) calleesaves temps 
    in
        seq(moves @ [body] @ unMoves)
    end

(* COMPLETAR *)
fun procEntryExit3 (frame, instrs) = 
    let 
(*        val (globl, finalName) = 
            if (name frame) = "_tigermain"
            then (".globl main", "main")
            else ("", name frame)*)
        val (globl, finalName) = 
            if (name frame) = "_tigermain"
            then (".globl _tigermain", "_tigermain")
            else ("", name frame)            
        
        val frameSize = (!(#actualLocal(frame))) * localsGap
        val pr = "" ^ globl ^ "\n" ^
                 ".type " ^ finalName ^ ", @function\n" ^
                 "" ^ finalName ^ ":\n" ^
	             "\tpushl %ebp \n" ^
	             "\tmovl %esp, %ebp \n" ^             
                 "\tsubl $" ^ (Int.toString(frameSize)) ^ ", %esp\n"
        val epi = "\taddl $" ^ (Int.toString(frameSize)) ^ ", %esp\n" ^
                  "\tleave\n" ^
                  "\tret\n\n"
    in
        {prolog=pr, body=instrs, epilog=epi}
    end
end
