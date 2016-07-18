structure tigertrans :> tigertrans = struct

open tigerframe
open tigertree
open tigertemp
open tigerabs

(* Guardar caller saves *)

fun saveCallers e = 
    let
        val temps = List.map (fn _ => tigertemp.newtemp()) callersaves	
        val saveCallerSaves = tigerutils.zipWith (fn (t, r) => MOVE(TEMP t, TEMP r)) temps callersaves
        val restoreCallerSaves = tigerutils.zipWith (fn (t, r) => MOVE(TEMP t, TEMP r)) callersaves temps 	    
    in
        saveCallerSaves @ e @ restoreCallerSaves   
    end
    
(*fun externalCall2(s, l) = seq(saveCallers [EXP(externalCall(s, l))])*)

(* BORRAR *)
fun showLvl {parent, frame, level} = 
    case parent
    of SOME f => "{parent=" ^ name(f) ^ ", frame=" ^ name(frame) ^ ", level=" ^ Int.toString level ^ "}"
     | NONE => "{parent=NONE, frame=" ^ name(frame) ^ ", level=" ^ Int.toString level ^ "}"
     
(* BORRAR *)

exception breakexc
exception divCero
	
type level = {parent:frame option , frame: frame, level: int}
type access = tigerframe.access

type frag = tigerframe.frag
val fraglist = ref ([]: frag list)

val actualLevel = ref ~1 (* _tigermain debe tener level = 0. *)
fun getActualLev() = !actualLevel

val outermost: level = {parent=NONE,
	frame=newFrame{name="_tigermain", formals=[]}, level=getActualLev()}
fun newLevel{parent={parent, frame, level}, name, formals} =
	{
	parent=SOME frame,
	frame=newFrame{name=name, formals = true :: formals},
	level=level+1}
fun allocArg{parent, frame, level} b = tigerframe.allocArg frame b
fun allocLocal{parent, frame, level} b = tigerframe.allocLocal frame b
fun formals{parent, frame, level} = tigerframe.formals frame

datatype exp =
	Ex of tigertree.exp
	| Nx of tigertree.stm
	| Cx of label * label -> tigertree.stm

fun seq [] = EXP (CONST 0)
	| seq [s] = s
	| seq (x::xs) = SEQ (x, seq xs)

fun unEx (Ex e) = e
	| unEx (Nx s) = ESEQ(s, CONST 0)
	| unEx (Cx cf) =
	let
		val r = newtemp()
		val t = newlabel()
		val f = newlabel()
	in
		ESEQ(seq [MOVE(TEMP r, CONST 1),
			cf (t, f),
			LABEL f,
			MOVE(TEMP r, CONST 0),
			LABEL t],
			TEMP r)
	end

fun unNx (Ex e) = EXP e
	| unNx (Nx s) = s
	| unNx (Cx cf) =
	let
		val t = newlabel()
		val f = newlabel()
	in
		seq [cf(t,f),
			LABEL t,
			LABEL f]
	end

fun unCx (Nx s) = raise Fail ("Error (UnCx(Nx..))")
	| unCx (Cx cf) = cf
	| unCx (Ex (CONST 0)) =
	(fn (t,f) => JUMP(NAME f, [f]))
	| unCx (Ex (CONST _)) =
	(fn (t,f) => JUMP(NAME t, [t]))
	| unCx (Ex e) =
	(fn (t,f) => CJUMP(NE, e, CONST 0, t, f))

fun Ir(e) =
	let	fun aux(Ex e) = tigerit.tree(EXP e)
		| aux(Nx s) = tigerit.tree(s)
		| aux _ = raise Fail "bueno, a completar!"
		fun aux2(PROC{body, frame}) = aux(Nx body)
		| aux2(STRING(l, "")) = l^":\n"
		| aux2(STRING("", s)) = "\t"^s^"\n"
		| aux2(STRING(l, s)) = l^":\t"^s^"\n"
		fun aux3 [] = ""
		| aux3(h::t) = (aux2 h)^(aux3 t)
	in	aux3 e end
fun nombreFrame frame = print(".globl " ^ tigerframe.name frame ^ "\n")

(* While y for necesitan la u'ltima etiqueta para un break *)
local
	val salidas: label option tigerpila.Pila = tigerpila.nuevaPila1 NONE
in
	val pushSalida = tigerpila.pushPila salidas
	fun popSalida() = tigerpila.popPila salidas
	fun topSalida() =
		case tigerpila.topPila salidas of
		SOME l => l
		| NONE => raise Fail "break incorrecto!"			
end

val datosGlobs = ref ([]: frag list)
fun procEntryExit{level: level, body} =
	let	val label = STRING(name(#frame level), "")
		val body' = PROC{frame= #frame level, body=unNx body}
		val final = STRING(";;-------", "")
	in	datosGlobs:=(!datosGlobs@[label, body', final]) end
fun getResult() = !datosGlobs

fun stringLen s =
	let	fun aux[] = 0
		| aux(#"\\":: #"x"::_::_::t) = 1+aux(t)
		| aux(_::t) = 1+aux(t)
	in	aux(explode s) end

fun stringExp(s: string) =
	let	val l = newlabel()
		val len = ".long "^makestring(stringLen s)
		val str = ".string \""^s^"\""
		val _ = datosGlobs:=(!datosGlobs @ [STRING(l, len), STRING("", str)])
        (*val _ = datosGlobs:=(!datosGlobs @ [STRING(l, len), STRING(l, str)])*)
	in	Ex(NAME l) end
fun preFunctionDec() =
	(pushSalida(NONE);
	actualLevel := !actualLevel+1)
fun functionDec(e, l, proc) =
	let	val body =
				if proc then unNx e
				else MOVE(TEMP rv, unEx e)
		val body' = procEntryExit1(#frame l, body)
		val () = procEntryExit{body=Nx body', level=l}
	in	Ex(CONST 0) end
fun postFunctionDec() =
	(popSalida(); actualLevel := !actualLevel-1)

fun unitExp() = Ex (CONST 0)

fun nilExp() = Ex (CONST 0)

fun intExp i = Ex (CONST i)

fun simpleVar(acc, nivel) =
	case acc of
		InReg r => Ex (TEMP r)
		| InFrame k =>
			let 
				fun aux 0 = TEMP fp
				| aux n = MEM(BINOP(PLUS,CONST fpPrevLev, aux (n-1)))
			in
				Ex (MEM (BINOP(PLUS, CONST k, aux (getActualLev() - nivel))))
			end

fun varDec(acc, exp) = Nx (MOVE(unEx (simpleVar(acc, getActualLev())), unEx exp))

fun fieldVar(var, field) = 
let
  val v = unEx var
  val rv = newtemp()
in
  Ex(
    ESEQ(
      seq[
        MOVE(TEMP rv, v),
        EXP(externalCall("_checkNil", [TEMP rv]))
      ],
      MEM(BINOP(PLUS, TEMP rv,
			BINOP(MUL, CONST field, CONST tigerframe.wSz)))
    )
  )
end (*COMPLETAR*)

fun subscriptVar(arr, ind) =
let
	val a = unEx arr
	val i = unEx ind
	val ra = newtemp()
	val ri = newtemp()
in
	Ex( ESEQ(seq[
	      MOVE(TEMP ra, a),
		    MOVE(TEMP ri, i),
		    EXP(externalCall("_checkIndexArray", [TEMP ra, TEMP ri]))
		  ],
		MEM(BINOP(PLUS, TEMP ra,
			BINOP(MUL, TEMP ri, CONST tigerframe.wSz))))
	)
end

fun recordExp l =
let 
	val ret = newtemp()
	
	fun aux (e,_) =
	let 
	    val t = newtemp()
	in 
	    ESEQ (MOVE (TEMP t, unEx e), TEMP t)
	end
in 
    Ex (ESEQ (seq [EXP (externalCall ("_allocRecord", CONST (List.length l)::(map aux l))), MOVE (TEMP ret, TEMP rv)], TEMP ret))
end (*COMPLETAR*)

fun arrayExp{size, init} =
let
	val s = unEx size
	val i = unEx init
in
	Ex (externalCall("_initArray", [s, i]))
end

fun callExp (name,external,isproc,lev:level,ls) = 
let
	fun memArray 0 = TEMP fp
	|   memArray n = MEM (BINOP (PLUS, CONST fpPrevLev, memArray (n-1)))
	
	val fpLev =  
		if #level lev = getActualLev() 
		then MEM (BINOP (PLUS, CONST fpPrevLev, TEMP fp))
		else
			if (#level lev < getActualLev())
			then memArray(getActualLev() - #level lev + 1)
			else (TEMP fp)
        
	fun preparaArgs [] (rt, re) = (rt, re)
	|   preparaArgs (h::t) (rt,re) =
		case h of
			Ex (CONST n) => preparaArgs t ((CONST n)::rt, re)
			| Ex (NAME n) => preparaArgs t ((NAME n)::rt, re)
			| Ex (TEMP t') => preparaArgs t ((TEMP t')::rt, re)
			| _ =>
				let val temp = newtemp()
				in preparaArgs t ((TEMP temp)::rt,(MOVE (TEMP temp, unEx h))::re)
				end 
	val (ta,la') = preparaArgs (ls) ([],[]) 
	val ta' = if external then ta else fpLev :: ta
	

in
	if isproc 
	(*then Nx (seq(saveCallers ((la'@[EXP (CALL (NAME name, ta'))]))))*)
	then Nx (seq((la'@[EXP (CALL (NAME name, ta'))])))
	else
		let val tmp = newtemp()
		(*in  Ex (ESEQ (seq (saveCallers (la' @[EXP(CALL(NAME name,ta')),MOVE(TEMP tmp,TEMP rv)])),TEMP tmp))*)
		in  Ex (ESEQ (seq (la' @[EXP(CALL(NAME name,ta')),MOVE(TEMP tmp,TEMP rv)]),TEMP tmp))
		end
end



fun letExp ([], body) = Ex (unEx body)
 |  letExp (inits, body) = Ex (ESEQ(seq inits,unEx body))

fun breakExp() = 
let 
    val l = topSalida()
in 
    Nx (JUMP(NAME l,[l])) 
end (*COMPLETAR*)

fun seqExp ([]:exp list) = Nx (EXP(CONST 0))
	| seqExp (exps:exp list) =
		let
			fun unx [e] = []
				| unx (s::ss) = (unNx s)::(unx ss)
				| unx[] = []
		in
			case List.last exps of
				Nx s =>
					let val unexps = map unNx exps
					in Nx (seq unexps) end
				| Ex e => Ex (ESEQ(seq(unx exps), e))
				| cond => Ex (ESEQ(seq(unx exps), unEx cond))
		end

fun preWhileForExp() = pushSalida(SOME(newlabel()))

fun postWhileForExp() = (popSalida(); ())

fun whileExp {test: exp, body: exp, lev:level} =
let
	val cf = unCx test
	val expb = unNx body
	val (l1, l2, l3) = (newlabel(), newlabel(), topSalida())
in
	Nx (seq[LABEL l1,
		cf(l2,l3),
		LABEL l2,
		expb,
		JUMP(NAME l1, [l1]),
		LABEL l3])
end

fun forExp {lo, hi, var, body} = 
let 
    val var' = unEx var
    val (l1,l2,lsal) = (newlabel(), newlabel(), topSalida())
in
   Nx (seq (case hi of
               Ex (CONST n) =>
                   if n < valOf Int.maxInt then
                      [MOVE (var', unEx lo),
                       JUMP (NAME l2, [l2]),
                       LABEL l1,
                       unNx body,
                       MOVE (var', BINOP (PLUS, var', CONST 1)),
                       LABEL l2,
                       CJUMP (GT, var', CONST n, lsal, l1),
                       LABEL lsal]
                   else
                      [MOVE (var', unEx lo),
                       LABEL l1,
                       unNx body,
                       CJUMP (EQ, var', CONST n, lsal, l2),
                       LABEL l2,
                       MOVE (var', BINOP (PLUS, var', CONST 1)),
                       JUMP (NAME l1, [l1]),
                       LABEL lsal]
             | _ => 
                   let 
                       val t = newtemp()
                   in 
                       [MOVE (var', unEx lo),
                       MOVE (TEMP t, unEx hi),
                       CJUMP (LE, var', TEMP t, l2, lsal),
                       LABEL l2,
                       unNx body,
                       CJUMP (EQ, var', TEMP t, lsal, l1),
                       LABEL l1,
                       MOVE (var', BINOP (PLUS, var', CONST 1)),
                     JUMP (NAME l2, [l2]),
                       LABEL lsal]
                   end))
end
	 (*COMPLETAR*)

fun ifThenExp{test, then'} =
let
    val tf = unCx test
    val st = unNx then'
    val (lthen, lsal) = ( newlabel(), newlabel() )
in
    Nx(
        seq[
            tf(lthen, lsal),
            LABEL lthen,
            st,
            LABEL lsal
        ]
    )
end
	(*COMPLETAR*)

(* Preguntar a Guido *)
(* Si esta bien boludear a Lulo *)
(* HECHO *)
fun ifThenElseExp {test,then',else'} =
let
    val tf = unCx test
    val st = unNx then'
    val et = unEx then'
    val se = unNx else'
    val ee = unEx else'
    val (lthen, lelse, lsal) = ( newlabel(), newlabel(), newlabel() )
    val r = newtemp()
in
    Ex( ESEQ(
        seq[
            tf(lthen, lelse),
            LABEL lthen,
            MOVE(TEMP r, et),
            JUMP(NAME lsal, [lsal]),
            LABEL lelse,
            MOVE(TEMP r, ee),
            LABEL lsal
        ],
        TEMP r
        )
    )
end
	(*COMPLETAR*)

fun ifThenElseExpUnit {test,then',else'} =
let
    val tf = unCx test
    val st = unNx then'
    val se = unNx else'
    val (lthen, lelse, lsal) = ( newlabel(), newlabel(), newlabel() )
in
    Nx(
        seq[
            tf(lthen, lelse),
            LABEL lthen,
            st,
            JUMP(NAME lsal, [lsal]),
            LABEL lelse,
            se,
            LABEL lsal
        ]
    )
end
(*COMPLETAR*)

fun assignExp{var, exp} =
let
	val v = unEx var
	val vl = unEx exp
in
	Nx (MOVE(v,vl))
end

fun binOpIntExp {left, oper, right} = 
let
    val l = unEx left
    val r = unEx right
in
   case oper 
   of PlusOp => Ex( BINOP(PLUS, l, r) )
    | MinusOp => Ex( BINOP(MINUS, l, r) )
    | TimesOp => Ex( BINOP(MUL, l, r) )
    | DivideOp => Ex( BINOP(DIV, l, r) )
	| _ => raise Fail("Error interno(binOpIntExp): operador no aritmetico")
end
	 (*COMPLETAR*)

fun binOpIntRelExp {left,oper,right} =
let
    val l = unEx left
    val r = unEx right
in
    case oper
    of EqOp => Cx( fn (t,f) => CJUMP(EQ, l, r, t, f) )
	 | NeqOp => Cx( fn (t,f) => CJUMP(NE, l, r, t, f) )
	 | LtOp => Cx( fn (t,f) => CJUMP(LT, l, r, t, f) )
	 | LeOp => Cx( fn (t,f) => CJUMP(LE, l, r, t, f) )
	 | GtOp => Cx( fn (t,f) => CJUMP(GT, l, r, t, f) )
	 | GeOp => Cx( fn (t,f) => CJUMP(GE, l, r, t, f) )
	 | _ => raise Fail("Error interno(binOpIntRelExp): operador no relacional")
end
	(*COMPLETAR*)

(* Preguntar a Guido: Parametros a externalCall en registros? *)
(* Se pasan los argumentos por registros cuando se quiere forzar la evaluacion.
   Es mas que nada necesario en el pasaje de argumentos a funcion.
   En otras instrucciones se puede dejar para mas adelante. *)
fun binOpStrExp {left,oper,right} =
let
    val l = unEx left
    val r = unEx right
    val rl = newtemp()
    val rr = newtemp()
    val operadorRelacionalDeStrings = case oper
            of EqOp => EQ
             | NeqOp => NE
             | LtOp => LT
             | LeOp => LE
             | GtOp => GT
             | GeOp => GE
             | _ => raise Fail("Error interno(binOpStrExp): operador de strings invalido")    
in
    Cx( fn (t,f) => seq[
        MOVE(TEMP rl, l),
        MOVE(TEMP rr, r),
        CJUMP(operadorRelacionalDeStrings, externalCall("_stringCompare", [TEMP rl, TEMP rr]), CONST 0, t, f)]
    )
end (*COMPLETAR*)


end
