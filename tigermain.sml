open tigerlex
open tigergrm
open tigerescap
open tigerseman
open BasicIO Nonstdio

(* BORRAR *)
fun println s = (print s; print("\n"))
(* BORRAR *)

fun lexstream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n);
fun errParsing(lbuf) = (print("Error en parsing!("
	^(makestring(!num_linea))^
	")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")
fun main(args) =
	let	fun arg(l, s) =
			(List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
		val (arbol, l1)		= arg(args, "-arbol")
		val (escapes, l2)	= arg(l1, "-escapes") 
		val (ir, l3)		= arg(l2, "-ir") 
		val (canon, l4)		= arg(l3, "-canon") 
		val (code, l5)		= arg(l4, "-code") 
		val (flow, l6)		= arg(l5, "-flow") 
		val (inter, l7)		= arg(l6, "-inter") 
		val filename =
			case l7 of
			[n] => substring (n, 0, (size n) - 4)
			| [] => ""
			| _ => raise Fail "opcio'n dsconocida!"
		val entrada =
			case l7 of
			[n] => ((open_in n)
					handle _ => raise Fail (n^" no existe!"))
			| [] => std_in
			| _ => raise Fail "opcio'n dsconocida!"
		val lexbuf = lexstream entrada
		val expr = prog Tok lexbuf handle _ => errParsing lexbuf
		val _ = findEscape(expr)
		val _ = if arbol then tigerpp.exprAst expr else ()
		val _ = transProg(expr)
		val filenameS = filename ^ ".s"
		(* BORRAR *)
		fun name (tigerframe.PROC{body, frame}) = tigerframe.name(frame)
		  | name _ = raise Fail "error interno (name): no es PROC"
		(* BORRAR *)
		val frags = tigertrans.getResult() (* lista de fragmentos *)
		val func_frags = let fun isFunc (tigerframe.PROC _) = true 
		                       | isFunc _ = false
		                 in
		                   List.filter isFunc frags
		                 end
		val str_frags = let fun isStr (tigerframe.STRING _) = true
		                      | isStr _ = false
		                    fun strip (tigerframe.STRING f) = f
		                      | strip _ = raise Fail "error interno (strip): no es STRING"
		                in
		                   List.map strip (List.filter isStr frags)
		                end
		fun canon_frag (tigerframe.PROC {body, frame}) = (tigercanon.canonizar body, frame)
		  | canon_frag _ = raise Fail "error interno (canon_frag): no es proc"
		val canon_frags = List.map canon_frag func_frags
		fun codegenTuple (stms, f) = List.concat (List.map (tigercodegen.codegen f) stms )
				
		(* Generemos las alocaciones muchacho *)
		
		val instrsAllBlocks = List.map codegenTuple canon_frags 
		val framesAllBlocks = List.map (#2) canon_frags
		val instrsAndAllocsAllBlocks = 
                            List.map (tigerregalloc.alloc) (ListPair.zip (instrsAllBlocks, framesAllBlocks))

                val instrsWithFrames = ListPair.zip (framesAllBlocks, List.map (#1) instrsAndAllocsAllBlocks) 
		val instrsProcEntryExit3 = List.map tigerframe.procEntryExit3 instrsWithFrames 
                val pee3prologsAndEpilogs = List.map (fn x => ((#prolog) x, (#epilog) x)) instrsProcEntryExit3
                val pee3body = List.map (#body) instrsProcEntryExit3

                val instrsAndAllocsAllBlocks2 = List.map (fn ((instrsOld, alloc), instrsNew) => (instrsNew, alloc)) (ListPair.zip (instrsAndAllocsAllBlocks, pee3body))

		
		fun florDeLaV f x = if tigerutils.isinList x tigerframe.specialregs then x else f x
		val oggiAlloc = florDeLaV o tigertab.tab2Func (* transforma un coloreo en funcion *)

        val lizzyTagliani : (string list) list = List.map (fn (is, a) => List.map (tigerassem.format (oggiAlloc a)) is) instrsAndAllocsAllBlocks2 

        val zulmaLobato = List.map (fn (instrLst, (epil, prol)) => epil ^ (String.concat instrLst) ^ prol ) (ListPair.zip (lizzyTagliani, pee3prologsAndEpilogs))
 
        (* Seccion .text final *)
		val finalTextInstrs : string = String.concat zulmaLobato
		
		(* Seccion .data final *)
		fun makeStrFrag (l, s) = if s = "" 
		                         then "" 
		                         else if l = "" 
		                              then "\t" ^ s ^ "\n"
		                              else l ^ ": \n\t" ^ s ^ "\n"
		                              
		val finalDataInstrs : string = String.concatWith "\n" (List.map makeStrFrag str_frags)
		
		(* Programa final *)
		val finalProgram : string = 
		         ".data\n" ^
		         finalDataInstrs ^
		         ".text\n" ^
		         finalTextInstrs
		         		
		(* Exportamos las instrucciones a un .s *)
		val outs = TextIO.openOut(filenameS)
		val _ = TextIO.output(outs, finalProgram)
		val _ = TextIO.closeOut(outs)

        (* Generamos el ejecutable *)
		val _ = Process.system("gcc -g -m32 " ^ filenameS ^ " runtime.o -o " ^ filename)
				
	in
		print "yes!!\n"
	end	handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())

