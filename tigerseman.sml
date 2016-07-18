structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres
open tigertemp
open tigerpp
open tigertrans

(* tipo de retorno para transexp *)
type expty = {exp: unit, ty: Tipo}

(* tipo para el entorno de variables *)
type venv = (string, EnvEntry) tigertab.Tabla

(* tipo para el entorno de tipos *)
type tenv = (string, Tipo) tigertab.Tabla

(* entorno de tipos inicial *)
val tab_tipos : (string, Tipo) Tabla = tabInserList(
	tabNueva(),
	[("int", TInt), ("string", TString)])

val levelPila: tigertrans.level tigerpila.Pila = tigerpila.nuevaPila1(tigertrans.outermost) 
fun pushLevel l = tigerpila.pushPila levelPila l 
fun topLevel() = tigerpila.topPila levelPila
fun popLevel() = tigerpila.popPila levelPila 

fun printPila() = (print("PILA: "); tigerpila.appPila (print o showLvl) levelPila; print("\n"))

(* entorno de variables inicial *)
val tab_vars : (string, EnvEntry) Tabla = tabInserList(
	tabNueva(),
	[("print", Func{level=topLevel(), label="print",
		formals=[TString], result=TUnit, extern=true}),
	("flush", Func{level=topLevel(), label="flush",
		formals=[], result=TUnit, extern=true}),
	("getchar", Func{level=topLevel(), label="getstr",
		formals=[], result=TString, extern=true}),
	("ord", Func{level=topLevel(), label="ord",
		formals=[TString], result=TInt, extern=true}),
	("chr", Func{level=topLevel(), label="chr",
		formals=[TInt], result=TString, extern=true}),
	("size", Func{level=topLevel(), label="size",
		formals=[TString], result=TInt, extern=true}),
	("substring", Func{level=topLevel(), label="substring",
		formals=[TString, TInt, TInt], result=TString, extern=true}),
	("concat", Func{level=topLevel(), label="concat",
		formals=[TString, TString], result=TString, extern=true}),
	("not", Func{level=topLevel(), label="not",
		formals=[TInt], result=TInt, extern=true}),
	("exit", Func{level=topLevel(), label="exit",
		formals=[TInt], result=TUnit, extern=true})
	])

(* Obtiene el tipo final al que apunta un TTipo *)
fun tipoReal (TTipo (s, ref (SOME (t)))) = tipoReal t
  | tipoReal t = t

(* Igualdad de tipo que tiene en cuenta casos especiales como 
   nil para records, y unique para records y array *)
fun tiposIguales (TRecord _) TNil = true
  | tiposIguales TNil (TRecord _) = true 
  | tiposIguales (TRecord (_, u1)) (TRecord (_, u2 )) = (u1=u2)
  | tiposIguales (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
  | tiposIguales (TTipo (_, r)) b =
		let
			val a = case !r of
				SOME t => t
				| NONE => raise Fail "No debería pasar! (1)"
		in
			tiposIguales a b
		end
  | tiposIguales a (TTipo (s, r)) =
		let
		    val _ = printIf(5, "Llamando a tiposIguales con a = " ^ tipoToString a ^
		                       " y b = " ^ tipoToString (TTipo(s, r)) ^ "\n")
			val b = case !r of
				SOME t => t
				| NONE => raise Fail ("No debería pasar! (2) " ^ s)
		in
			tiposIguales a b
		end
  | tiposIguales a b = (a=b)

(* Compara dos tipos obtienendo primer sus tipos reales *)
fun tiposIgualesReales a b = tiposIguales (tipoReal a) (tipoReal b)

(* Dados un entorno de variables y uno de tipos, tipa una variable y genera
   su codigo intermedio asociado (no aun!) *)
fun transExp(venv, tenv)  =
	let (* Lanza una excepcion imprimiendo el error dado junto al nº de linea del programa dado *)
	    fun error(s, p) = raise Fail ("Error -- línea "^Int.toString(p)^": "^s^"\n")
	    
	    
	   (* (* Determina si una expresion es de un tipo dado, usando los entornos venv y tenv *)
	  	fun hasType (t, e) = tiposIgualesReales ( #ty( trexp e ) ) t 
	  	
	  	(* Determina si una expresion es de un tipo dado, usando los entornos pasados como argumentos *)
		  and hasTypeEnv (t, e, ve, te) = tiposIgualesReales ( #ty( transExp(ve, te) e ) ) t *)
		  
		  (* Traduce una expresion utilizando los entornos venv y tenv *)
	    fun trexp(VarExp v) = trvar(v)
		| trexp(UnitExp _) = {exp=unitExp(), ty=TUnit}
		| trexp(NilExp _)= {exp=nilExp(), ty=TNil}
		| trexp(IntExp(i, _)) = {exp=intExp i, ty=TInt}
		| trexp(StringExp(s, _)) = {exp=stringExp(s), ty=TString}
		| trexp(CallExp({func, args}, nl)) =
      		let val(targs, ext, tret, lab, lev) =
      		    
    			case tabBusca(func, venv) of 
    					SOME(Func{formals, extern, result, level, label}) =>
    									(formals, extern, result, label, level)
    					| SOME _ => error (func ^ "no es funcion", nl)
    					| NONE => error(func ^ " no existe", nl)
      			fun aux [] [] r = r
      			  | aux [] _ _ = error ("muchos argumentos", nl)
      			  | aux _ [] _ = error ("pocos argumentos", nl)
      			  | aux (t :: tt) (a :: aa) r =
      			    let val {exp = ae', ty = at'} = trexp a
      			        val _ = if(not (tiposIgualesReales t at'))
      			                then error("argumento incorrecto", nl)
								else ()
      			    in 
      			      aux tt aa r@[{exp=ae', ty=at'}]
      			    end
      			val leargs = aux targs args []
      			val leargs' = map #exp leargs
      			(* val pf = tret = TUnit  procedure no vuelve valor *)
      			
      			val isproc = tiposIgualesReales tret TUnit
            in
              {exp=callExp(lab, ext, isproc, lev,leargs'), ty=tret}
            end (* HECHO? *)
            
			(*{exp=(), ty= let val (argsTys, retTy) = case tabBusca(func, venv) 
														of SOME(Func ee) => (#formals(ee), #result(ee))
                       									 | _ => error("El nombre " ^ func ^ " no esta definido.", nl)
						 in if( andList ( zipWith hasType argsTys args ) )
						 	then retTytabBusca
						 	else error("El tipo de la funcion " ^ func ^ " no coincide con los argumentos pasados", nl)
						 end } *) (*COMPLETADO*)
		| trexp(OpExp({left, oper=EqOp, right}, nl)) =
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
			in
				if tiposIgualesReales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=if tiposIguales tyl TString then binOpStrExp {left=expl,oper=EqOp,right=expr} else binOpIntRelExp {left=expl,oper=EqOp,right=expr}, ty=TInt}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper=NeqOp, right}, nl)) = 
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
			in
				if tiposIgualesReales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=if tiposIguales tyl TString then binOpStrExp {left=expl,oper=NeqOp,right=expr} else binOpIntRelExp {left=expl,oper=NeqOp,right=expr}, ty=TInt}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper, right}, nl)) = 
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
			in
				if tiposIgualesReales tyl tyr then
					case oper of
						PlusOp => if tipoReal tyl=TInt then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt} else error("Error de tipos", nl)
						| MinusOp => if tipoReal tyl=TInt then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt} else error("Error de tipos", nl)
						| TimesOp => if tipoReal tyl=TInt then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt} else error("Error de tipos", nl)
						| DivideOp => if tipoReal tyl=TInt then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt} else error("Error de tipos", nl)
						| LtOp => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=if tipoReal tyl=TInt then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt} else error("Error de tipos", nl)
						| LeOp => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=if tipoReal tyl=TInt then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt} else error("Error de tipos", nl)
						| GtOp => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=if tipoReal tyl=TInt then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt} else error("Error de tipos", nl)
						| GeOp => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=if tipoReal tyl=TInt then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt} else error("Error de tipos", nl)
						| _ => raise Fail "No debería pasar! (3)"
				else error("Error de tipos", nl)
			end
		| trexp(RecordExp({fields, typ}, nl)) =
			let
				(* Traducir cada expresión de fields *)
				val tfields = map (fn (sy,ex) => (sy, trexp ex)) fields

				(* Buscar el tipo *)
				val (tyr, cs) = case tabBusca(typ, tenv) of
					SOME t => (case tipoReal t of
						TRecord (cs, u) => (TRecord (cs, u), cs)
						| _ => error(typ^" no es de tipo record", nl))
					| NONE => error("Tipo inexistente ("^typ^")", nl)
				
				(* Verificar que cada campo esté en orden y tenga una expresión del tipo que corresponde *)
				fun verificar _ [] [] = []
				  | verificar _ (c::cs) [] = error("Faltan campos", nl)
				  | verificar _ [] (c::cs) = error("Sobran campos", nl)
				  | verificar n ((s,t,_)::cs) ((sy,{exp,ty})::ds) =
						if s<>sy then error("Error de campo", nl)
						else if tiposIgualesReales ty t then (exp, n)::(verificar (n+1) cs ds)
							 else error("Error de tipo del campo "^s, nl)
				val lf = verificar 0 cs tfields
			in
				{exp=recordExp lf, ty=tyr}
			end
		| trexp(SeqExp(s, nl)) =
			let	val lexti = map trexp s
				val exprs = map (fn{exp, ty} => exp) lexti
				val {exp, ty=tipo} = hd(rev lexti)
			in	{ exp=seqExp (exprs), ty=tipo } end
        | trexp (AssignExp({var = SimpleVar s, exp}, nl)) =
		      let val _ = case tabBusca(s, venv) of
		                    SOME(VIntro _) => error ("No se puede asignar un valor a la variable de solo lectura " ^ s, nl)
		                  | _ => ()
		          val {exp = expi, ty = tyi} = trvar(SimpleVar s, nl)
		          val {exp = expd, ty = tyd} = trexp exp
		      in
		        if tiposIgualesReales tyi tyd then () else error ("error", nl);
		        tigerpp.printIf(1, "Entrando a AssignExp(SimpleVar..) con VENV: " ^ venvToString venv ^ "\n");
		        {exp = assignExp{var=expi, exp=expd}, ty = TUnit}
		      end (* HECHO? *)
	    | trexp (AssignExp({var, exp}, nl)) =
	        let val {exp = ei, ty = ti} = trvar(var, nl)
	            val {exp = ed, ty = td} = trexp exp
	        in
	          if tiposIgualesReales ti td 
			  then () 
			  else error ("El tipo de la variable de la izquierda no coincide con el tipo de la expresion a la derecha", nl);
	          	   {exp=assignExp{var=ei, exp=ed}, ty = TUnit}
	        end	(* HECHO? *)				     				 
										 (*COMPLETANDO*)
						
		| trexp(IfExp({test, then', else'=SOME else'}, nl)) =
			let val {exp=testexp, ty=tytest} = trexp test
			    val {exp=thenexp, ty=tythen} = trexp then'
			    val {exp=elseexp, ty=tyelse} = trexp else'
			in
				if tipoReal tytest=TInt andalso tiposIgualesReales tythen tyelse then {exp=if tipoReal tythen=TUnit then ifThenElseExpUnit {test=testexp,then'=thenexp,else'=elseexp} else ifThenElseExp {test=testexp,then'=thenexp,else'=elseexp}, ty=tythen}
				else error("Error de tipos en if" ,nl)
			end
		| trexp(IfExp({test, then', else'=NONE}, nl)) =
			let val {exp=exptest,ty=tytest} = trexp test
			    val {exp=expthen,ty=tythen} = trexp then'
			in
				if tipoReal tytest=TInt andalso tythen=TUnit then {exp=ifThenExp{test=exptest, then'=expthen}, ty=TUnit}
				else error("Error de tipos en if", nl)
			end
		| trexp(WhileExp({test, body}, nl)) =
			let val ttest = trexp test
                val _ = preWhileForExp()
				val tbody = trexp body
				val ret = if tipoReal (#ty ttest) = TInt andalso #ty tbody = TUnit 
				              then {exp=whileExp {test=(#exp ttest), body=(#exp tbody), lev=topLevel()}, ty=TUnit}
				          else if tipoReal (#ty ttest) <> TInt 
				              then error("Error de tipo en la condición", nl)
				          else 
				              error("El cuerpo de un while no puede devolver un valor", nl)
				val _ = postWhileForExp()
			in
                ret
			end


		| trexp(ForExp({var, escape, lo, hi, body}, nl)) =
            let
                val {ty=tylo,exp=explo} = trexp lo
                val {ty=tyhi,exp=exphi} = trexp hi
                val _ = if (tiposIguales tylo TInt) andalso (tiposIguales tyhi TInt) then ()
                        else error("Los li­mites del for deben ser enteros",nl)
                val acc' = allocLocal (topLevel()) (!escape)
                val lev = getActualLev()
                val venv' = tabRInserta(var, VIntro {access=acc',level=lev}, venv)
                val _ = preWhileForExp()
                val {ty=tybody,exp=expbody} = transExp (venv', tenv) body
                val _ = if tiposIguales tybody TUnit then () else error("El cuerpo de un for no puede devolver un valor", nl)
                val ev' = simpleVar(acc',lev)
                val ef' = forExp {lo=explo, hi=exphi, var=ev',body=expbody}
                val _ = postWhileForExp()
            in
                   {exp=ef',ty=TUnit}
            end
		| trexp(LetExp({decs, body}, _)) =
			let
				fun aux (d, (v, t, exps1)) =
				let
					val (v', t', exps2) = trdec (v, t) d
				in
					(v', t', exps1@(List.map #exp exps2))
				end
				val (venv', tenv', expdecs) = List.foldl aux (venv, tenv, []) decs
				val {exp=expbody,ty=tybody}=transExp (venv', tenv') body
			in 
				{exp=seqExp(expdecs@[expbody]), ty=tybody}
			end		
		(*
			let
				val (venv', tenv', _) = List.foldl (fn (d, (v, t, _)) => trdec(v, t) d) (venv, tenv, []) decs
				val {exp=expbody,ty=tybody}=transExp (venv', tenv') body
			in 
				{exp=seqExp(expdecs@[expbody]), ty=tybody} (* exp = seqExp(expdecs@[expbody]) LEO PUTO *)
			end (* HACER 5 *)*)
		| trexp(BreakExp nl) =
			{exp=breakExp(), ty=TUnit} 
			(*handle Empty => error("break fuera de un while/for", nl)*)
			
			(*COMPLETANDO*)
		| trexp(ArrayExp({typ, size, init}, nl)) =
		    let
		        val {exp=esize, ty=tsize} = trexp size
		        val {exp=einit, ty=tinit} = trexp init
		        val tyRet = let val (arrTy, elemTy) = case tabBusca(typ, tenv)
									     	     	  of SOME t => (case tipoReal t 
									     	     					of TArray(t, u) => (TArray(t, u), t)
									     	     					 | _ => error("El tipo " ^ typ ^ " no es de arreglo", nl))
									     		  	   | _ => error("El tipo " ^ typ ^ " no es de arreglo", nl)
						    in	
						        if not( tiposIgualesReales TInt tsize )
							        then error("La expresion del tamaño del arreglo no es de tipo int", nl)
								else if not ( tiposIgualesReales elemTy tinit )
                                    then error("El tipo del valor inicial no coincide con el tipo de los elementos del arreglo", nl)
							    else
							      	arrTy
						    end
		    in
			    (tigerpp.printIf(3, "Entrando a trexp(ArrayExp..) con entorno: " ^ tenvToString tenv ^ "\n");
			    tigerpp.printIf(3, "Entrando a trexp(ArrayExp..) con typ: " ^ typ ^ "\n");
			    tigerpp.printIf(3, "Entrando a trexp(ArrayExp..) con init: ");
			    {exp=arrayExp{size=esize, init=einit}, ty= tyRet}) (* HECHO? *)
			end			 			 
		and trvar(SimpleVar s, nl) =
		    let
		        val (t, acc, lev) = case tabBusca(s, venv)
				            	    of SOME(Var {ty, access, level}) =>  (ty, access, level) 
				                 	 | SOME(VIntro {access, level}) => (TInt, access, level)
					                 | _ => error("La variable " ^ s ^ " no esta definida", nl)
		    in
			    (tigerpp.printIf(1, "VENV en trvar(SimpleVar..) " ^ venvToString venv ^ "\n");
			     tigerpp.printIf(1, "TENV en trvar(SimpleVar..) " ^ tenvToString tenv ^ "\n");
			    {exp=simpleVar(acc, lev), ty= t})
			end				 (* HECHO? *)
		| trvar(FieldVar(v, s), nl) =
		    let
		        val {exp=e, ty=typ} = trvar(v, nl)
		        val (t', i') = case tipoReal ( typ ) 
				     		  of TRecord(xs, _) => (case List.find (fn (t,_,_) => s=t) xs 
						                               of SOME (_, ty, i) => (ty, i)
							 	     			        | NONE => error("No se encontro el campo " ^ s ^ " en el record", nl) )
						       | t => error("La variable no es un record, su tipo es " ^tipoToString t, nl)
		    in
			    {exp=fieldVar(e, i'), ty= t'} (*COMPLETANDO*)
			    (* case tipoReal (#ty( trvar(v, nl) ) )
					     		of TRecord(xs, _) => (case List.find (fn (t,_,_) => s=t) xs 
							                             of SOME (_, ty, _) => ty
													      | NONE => error("No se encontro el campo " ^ s ^ " en el record", nl) )
							     | t => error("La variable no es un record, su tipo es " ^tipoToString t, nl) *)
			end				 (* HECHO? *)
		| trvar(SubscriptVar(v, e), nl) =
		    let
		        val {exp=evar, ty=tvar} = trvar(v, nl)
		        val {exp=eexp, ty=texp} = trexp(e)
		    in
			    {exp=subscriptVar(evar, eexp), ty= if not( tiposIgualesReales TInt texp )
					                               then error("La expresion de la posicion del arreglo no es de tipo int", nl)
					                               else case tvar
						                                of TArray(ty, _) => ty
							                             | _ => error("La variable no es un arreglo", nl)
						     } (*COMPLETANDO*)
			end			 (* HECHO? *)
		and trdec (venv, tenv) (VarDec ({name,escape,typ=NONE,init},pos)) = 
			  let val _ = tigerpp.printIf(3, "Entrando a trDec(VarDec..) con entorno: " ^ tenvToString tenv ^ "\n")
			      val _ = tigerpp.printIf(3, "Entrando a trDec(VarDec..) con name: " ^ name ^ "\n")
			      val _ = tigerpp.printIf(3, "Entrando a trDec(VarDec..) con typ: NONE \n")
			      val _ = (tigerpp.printIf(3, "Entrando a trDec(VarDec..) con init: " ))
			      val {exp=einit, ty=tret'} = transExp(venv, tenv) init
			      val _ = case tret' 
			              of TNil => error("La expresion de inicializacion no puede ser nil porque no se indico un tipo para " ^ name, pos)
			               | _ => ()
			               
			      (*val tret' = case #ty (transExp(venv, tenv) init) 
								 of (TRecord _) => error("La expresion de inicializacion no puede ser un record porque no se indico un tipo para " ^ name, pos)
                                  | t => t*)
                  
                  
                  val acc = tigertrans.allocLocal (topLevel()) (!escape)
                  
                  val venv' = tabRInserta(name, Var {access=acc, level=getActualLev(), ty = tret'}, venv)
			  in
			  tigerpp.printIf(4, "Declarando variable " ^ name ^ "...\n");
				tigerpp.printIf(4, "VENV: " ^ venvToString venv' ^ "\n") ; 
				tigerpp.printIf(4, "TENV: " ^ tenvToString tenv ^ "\n") ; 					  
				(
					venv', 
     				tenv, 
					[{exp=varDec(acc, einit), ty = tret'}]
				)
			  end			(* HECHO? *)
		(*COMPLETAR*)
		| trdec (venv,tenv) (VarDec({name, escape, typ=SOME t, init}, nl)) =

			  (*let val {exp = e', ty = t'} = transExp(venv, tenv, init)*)
			  let val _ = tigerpp.printIf(3, "Entrando a trDec(VarDec..) con entorno: " ^ tenvToString tenv ^ "\n")
			      val _ = tigerpp.printIf(3, "Entrando a trDec(VarDec..) con name: " ^ name ^ "\n")
			      val _ = tigerpp.printIf(3, "Entrando a trDec(VarDec..) con typ: " ^ t ^ "\n")
			      val _ = (tigerpp.printIf(3, "Entrando a trDec(VarDec..) con init: " ))
			      val tret' = case tabBusca(t, tenv)
								 of SOME t => tipoReal t
				                  | NONE => error("El tipo " ^ t ^ " no esta definido (3)", nl)
				                  
				  val _ = tigerpp.printIf(3, "tret': " ^ tipoToString tret' ^ "\n")
				  val {exp=einit, ty=tyinit} = transExp(venv, tenv) init
				  
				  val _ = if not (tiposIgualesReales tret' tyinit)
                          then error("El tipo de la variable no coincide con la expresion de inicializacion", nl)
						  else ()
				  val acc = tigertrans.allocLocal (topLevel()) (!escape)
				  val venv' = tabRInserta(name, Var {access=acc, level=getActualLev(), ty = tret'}, venv) (*val venv' = tenv LEO PUTO *)
				  
			  in
			  tigerpp.printIf(4, "Declarando variable " ^ name ^ "...\n");
				tigerpp.printIf(4, "VENV: " ^ venvToString venv' ^ "\n") ; 
				tigerpp.printIf(4, "TENV: " ^ tenvToString tenv ^ "\n") ; 			  
				(
					venv', 
     				tenv, 
					[{exp=varDec(acc, einit), ty = tret'}]
				)
			  end (* HECHO? *)
			 (*COMPLETAR*)
			 
			 
			 
			 
			 
			 
			 
			 
			 
			 
			 
			 
		| trdec (venv,tenv) (FunctionDec fs) =
			let val _ = printIf(10, "Entrando a FunctionDec con fs: " ^ String.concatWith ", " (List.map (#name o #1) fs) ^ "\n") 
			
			    fun fldToTipo nl {name, escape, typ} = 
					case typ 
					   of NameTy s => (case tabBusca(s, tenv) 
										  of SOME t => tipoReal t
										   | NONE => error("El tipo " ^ s ^ " no esta definido (5)", nl))
					    | RecordTy _ => error("No se puede definir un parametro de tipo record", nl)
						| ArrayTy _ => error("No se puede definir un parametro de tipo array", nl)
				val fldToTipoOk = fldToTipo 0  (* usada cuando sabemos que no hay errores *)
				fun resTy nl res = case res 
 							  	      of SOME t => (case tabBusca(t, tenv) 
   											           of SOME ty => tipoReal ty
													    | _ => error("El tipo " ^ t ^ " no esta definido (4)", nl)) 
									   | NONE => TUnit				
				fun makeEnvEntry ({name, params, result, body}, nl ) = 
					let val formals = map (fldToTipo nl) params
						val label = case name 
									   of "_tigermain" => "_tigermain"
                  						| name => "." ^ name ^ Int.toString(nl) ^ "_" ^ newlabel()
                  						
                        val lvl = case name
            				      of "_tigermain" => outermost
            				       | _ => newLevel{parent=topLevel(), 
					                     				  name=label, 
					                     				  formals=List.map (fn p => !(#escape(p))) params}
					                     				                    						
                        val entry = {level=lvl, 
									 label=label, 
									 formals=formals, 
									 result=resTy nl result, 
									 extern=false}    

					in
					    (printIf(10, "EnvEntry armada: ");
					    printIf(10, showLvl (#level(entry)));
					    printIf(10, "\n");
					    Func entry)
					end
										
				val _ = let fun findDup [] = NONE
				              | findDup (f::fs) = if ( List.exists (fn y => #name(#1(f)) = #name(#1(y))) fs )
				              										then SOME (#name(#1(f)), #2(f))
				              										else findDup fs
				        in
				          case findDup fs
				             of NONE => ()
				              | SOME (x, pos) => error("La funcion " ^ x ^ " ya fue definida en este batch", pos)
				        end
				        
				fun updEnv env [] = env
				  | updEnv env (f::fs) = let val env' = tabRInserta(#name(#1(f)), makeEnvEntry f, env)
										 in updEnv env' fs
									     end
									     
				val venv' = updEnv venv fs
				
				(* debug *)
				val _ = tigerpp.printIf(1, tigerpp.venvToString venv' ^ "\n") 
				
				fun updEnvParam env [] = env
				  | updEnvParam env (p::ps) = let val acc = tigertrans.allocArg (topLevel()) (!(#escape p)) 
				                                  val env' = tabRInserta(#name(p), Var {access=acc, level=getActualLev(), ty = fldToTipoOk p}, env)
					    			 		  in updEnvParam env' ps
					    			 		  end
				
				fun getIsProc (result, nl)  = ((resTy nl result) = TUnit )
				fun getEscapes params = 
				    let fun esc ps = case ps 
				                     of [] => []
				                      | (f :: fs) => (!(#escape(f))) :: esc fs
				    in
				        esc params
				    end 
				    				
				
				fun transFunc ({name, params, result, body}, nl ) = 
					let val _ = printIf(10, "\tEntrando a transFunc de funcion " ^ name ^ " con nivel " ^ Int.toString(getActualLev()) ^ "\n")
					    val label = case tabBusca(name, venv')
					                of SOME(Func e) => #label(e)
					                 | _ => error("error interno (transFunc): no encuentra la funcion", nl)
					    
					    val esc = getEscapes params
					    val isProc = getIsProc (result, nl)
					    fun pushl() = case label
					                  of "_tigermain" => ()
					                   | _ => pushLevel(newLevel{parent=topLevel(), name=label, formals=esc})

					    
					    
					    val _ = (printIf(10, "\t\tEntrando a pushl para funcion " ^ name ^ " con nivel " ^ Int.toString(getActualLev()) ^ "\n");
					             pushl();	
					             printIf(10, "\t\tEntrando a preFunctionDec de funcion " ^ name ^ " con nivel " ^ Int.toString(getActualLev()) ^ "\n")
		    		             )
		    		             
		    		    val _ = case label
		    		            of "_tigermain" => ()
		    		             | _ => preFunctionDec()
		    		               
		    		    val env = updEnvParam venv' params	
		    		    
		    		    val _ = printIf(10, "\t\tEntrando a transExp del body de funcion " ^ name ^ " con nivel " ^ Int.toString(getActualLev()) ^ "\n")
					    val {exp=ebody, ty=tbody} = transExp(env, tenv) body
					    	    
    					val _ = if not( tiposIgualesReales (resTy nl result) tbody )
					            then error("El tipo del cuerpo de la funcion (" ^ 
					   						tigerpp.tetyToString (transExp(env, tenv) body) ^ ")" ^
					   						" no coincide con el tipo de retorno declarado (" ^ tigerpp.tipoToString (resTy nl result) ^ ")", nl)
					            else ()
					    val _ = (functionDec(ebody, topLevel(), isProc);
					             (case label
		    		              of "_tigermain" => ()
		    		               | _ => postFunctionDec());
					             popLevel())
					in 
                        ()
					end
					
			    val _ = List.app transFunc fs
				
				
			in
				(venv', tenv, [])
			end (* HECHO :D *)
















		(*COMPLETAR*)
		| trdec (venv,tenv) (TypeDec ts) =
			let (* Posicion inicial del batch *)
			    val posInitBatch = #2 (List.hd ts)
					(* Chequeamos que un tipo no se defina dos veces en el mismo batch *)
			    val _ = case tigerutils.findDup (map (#name o #1) ts)
		                 of NONE => ()
		                  | SOME x => error("El tipo " ^ x ^ " esta definido mas de una vez en este batch", posInitBatch)
				   
				  (* True si un tipo (ty) es record en el entorno y batch dados *)       
			    fun isRecord ty tenv ts =  
		    		case ty
		    		   of RecordTy _ => true
								| ArrayTy _ => false
								| NameTy s => case List.find (fn({name=n, ty=_}, _) => n = s) ts
										 						 of SOME({name=n, ty=ttttt}, _) => isRecord ttttt tenv ts
										  					  | NONE => (case tabBusca(s, tenv)
										                						of SOME t => true
										                						 | NONE => error("Error interno: isRecord", 0) )
				  
				  (* Actualiza el entorno con la lista de declaraciones de tipo *)
					fun updEnv env [] = env
				  | updEnv env (({name: symbol, ty: ty}, nl)::ts) = 
				  		let val env' = tabRInserta(name, TTipo (name, ref NONE), env)
				  		in updEnv env' ts
				  		end
				  		
				  (* Entorno luego de agregar los nombres de la izquierda *)
			    val tenv' = updEnv tenv ts
			    
			    val _ = tigerpp.printIf(1, "Entorno antes de la primera pasada: " ^ tenvToString tenv)
			    val _ = tigerpp.printIf(1, "Entorno despues de la primera pasada: " ^ tenvToString tenv')
			    		
					(* Chequeamos que los records no contengan campos duplicados *)			    			    		   
					val _ = let fun checkRec pos (RecordTy flds) = (case tigerutils.findDupCmp (fn (x, y) => #name(x) = #name(y) ) flds
					                                       						of SOME f => error("El campo " ^ #name(f) ^ " esta repetido\n", pos)
					                                        					 | NONE => () )
					              | checkRec p t = ()
					        in
					          List.app (fn ({name=n, ty=t}, nl) => checkRec nl t) ts
					        end
					          
			    (* Obtiene el simbolo de un ty, excepto si es RecordTy: ahi tira error :D *)	 
			    fun getSymbol (NameTy s) = s
			      | getSymbol (ArrayTy s) = s
			      | getSymbol _ = error("error interno: getSymbol", 0)
			      
			    (* Dada una declaracion simple de tipos (type A =...) genera la lista de 
			       dependencias asociadas *)
			    fun makeDeps ({name, ty}, nl) =
			    		case ty 
			    		   of NameTy s => (if List.exists (fn({name=n, ty=_}, _) => n = s) ts
			    		   				   then [(s, name)]
			    		   				   else case tabBusca(s, tenv')
			    		   				           of SOME t => []
			    		   				            | NONE => error("El tipo " ^ s ^ " no esta definido (1)", nl))
			    		    | ArrayTy s => (if List.exists (fn({name=n, ty=_}, _) => n = s) ts
			    		   				    			then [(s, name)]
			    		   				    			else case tabBusca(s, tenv')
			    		   				            			of SOME t => []
			    		   				             			 | NONE => error("El tipo " ^ s ^ " no esta definido (2)", nl))
			    		    | RecordTy flist => List.concat (List.map (fn {name=_, escape=_, typ=ty} => if isRecord ty tenv' ts then [] else [(getSymbol ty, name)]) flist)
			    
			    (* Dependencias de este batch *)		    
			    val deps = List.concat (List.map makeDeps ts)
			    
			    val _ = tigerpp.printIf(3, "Dependencias: " ^ tigerpp.depsToString deps ^ "\n")
			    
			    (* Dependencias ordenadas con topsort. Notar que no estan incluidos los nombres
			       que no generaron dependencias en un principio. *)
			    val sortedDeps = topsort.topsort deps
			    
			    (* Obtenemos la parte izquierda (nombre) de las declaraciones de este batch *)
			    val allLeftDecs = List.map (fn ({name=n, ty=_}, _) => n) ts
			    
			    (* Obtenemos todos los nombres de tipos, ordenados topologicamente, 
			       incluidos aquellos que no generaron dependencias *)
			    val allSortedDecs = List.filter (fn x => not (elem x sortedDeps)) allLeftDecs @ sortedDeps
			    
			    (* Traduce un tipo de ty (NameTy, RecordTy, ArrayTy..) a Tipo (TRecord, TArray...) 
			       en base al entorno dado *)
					fun transTy nl tenv ty = 
						let fun transFields flds = 
									let fun trFlds n [] = []
									      | trFlds n ({name, escape, typ}::fs) = 
									      	(name, transTy nl tenv typ, n) :: trFlds (n+1) fs
									in
									  trFlds 0 flds
									end
								val _ = tigerpp.printIf(3, "TENV en transTy: " ^ tenvToString tenv ^ "\n") ; 	
						in
							case ty 
								 of NameTy s => (case tabBusca(s, tenv) 
													of SOME t => tipoReal t
													 | NONE => error("El tipo " ^ s ^ " no esta definido (11)", nl))
								| RecordTy flds => TRecord (transFields flds, ref ())
								| ArrayTy s => (case tabBusca(s, tenv)
																   of SOME t => TArray (tipoReal t, ref ())
																    | NONE => error("El tipo " ^ s ^ "no esta definido (12)", nl))
						end
												  
				  (*fun addDecs [] ent = ent
				    | addDecs (dec::ordenDecs) ent = 
				    	case List.find (fn ({name=n, ty=_}, _) => n = dec) ts
				    	   of SOME ({name=_, ty=t}, nl) => addDecs ordenDecs (tabRInserta(dec, transTy nl ent t, ent))
				    	    | NONE => error("error interno: addDecs", 0) *)
				  
				  (* Agrega una lista de declaraciones (identificadas con su nombre) al entorno dado *)  	    
				  fun addDecs [] ent = ent
				    | addDecs (dec::ordenDecs) ent = 
				    	case List.find (fn ({name=n, ty=_}, _) => n = dec) ts
				    	   of SOME ({name=n, ty=t}, nl) => ( printIf(4, "\nAgregando declaracion " ^ dec ^ " con name=" ^ n ^ " y ty=" ^ tyToString t ^ "\n");
				    	   									 printIf(4, "La nueva entrada sera: " ^ dec ^ " -> " ^ tipoToString (transTy nl ent t) ^ "\n\n"  );
				    	   									 addDecs ordenDecs (tabRInserta(dec, transTy nl ent t, ent)) )
				    	    | NONE => error("error interno: addDecs", 0)				    	    
				  
				  val _ = tigerpp.printIf(3, "Dependencias ordenadas: [" ^ String.concatWith ", " sortedDeps ^ "]\n") 
			  	
			  	(* Entorno "casi" final *)
			  	val almostRetEnv = addDecs allSortedDecs tenv'

				fun fixNone (TTipo (a, r)) = (case r 
				                                of ref NONE => (case tabBusca(a, almostRetEnv)
															       of SOME t => r := SOME t
															        | NONE => error("error interno: fixField", 0))
									             | ref (SOME t) => ())
			  	  | fixNone (TRecord (fs, u)) = List.app (fixNone o #2) fs
			  	  | fixNone (TArray (t, u)) = fixNone t
			  	  | fixNone (TFunc (ts, t)) = (List.app fixNone ts; fixNone t)
       			  | fixNone _ = () 

          		val retEnv = tabApp fixNone almostRetEnv
			  	(* caso problematico: pueden todavia haber TTipo con NONE adentro *)
			  	val _ = printIf(4, "TENV luego de las declaraciones:\n" ^ tenvToString almostRetEnv ^ "\n")	                
			in
				tigerpp.printIf(3, "Declarando batch de tipos: [" ^ String.concatWith ", " (List.map (#name o #1) ts) ^ "]...\n");
				tigerpp.printIf(5, "VENV luego de declarar tipos: " ^ venvToString venv ^ "\n") ; 
				tigerpp.printIf(4, "TENV luego de declarar tipos: " ^ tenvToString retEnv ^ "\n") ; 
				(venv, retEnv, []) 
			end (* HECHO? *)
		
			(*COMPLETAR*)
	
	in trexp end
	
(* Dada una expresion e (ver tigerabs), crea la funcion principal _tigermain con e como
   cuerpo y le aplica transExp con los entornos iniciales	*)
fun transProg ex =
	let	val main =
				LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
								result=SOME "int", body=ex}, 0)]],
						body=UnitExp 0}, 0)
		val _ = transExp(tab_vars, tab_tipos) main
	in	print "bien!\n" end
end
