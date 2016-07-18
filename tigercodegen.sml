structure tigercodegen :> tigercodegen =
struct
open tigertree
open tigerassem
open tigerframe

fun relOp relop = case relop
                  of EQ => "je "
                   | NE => "jne "
                   | LT => "jl "
	               | GE => "jge "
	               | GT => "jg "
	               | LE => "jle "
	               | ULT => "jb "
	               | UGE => "jae "
	               | ULE => "jbe "
	               | UGT => "ja "
	               
fun codegen frame stm =
    
    let (* nuestra intToString *)
        val intToString = tigerutils.intToString
        
        (* lista de instrucciones que vamos llenando en munch *)
        val ilist = ref []: tigerassem.instr list ref
        
        (* emit agrega una instruccion al principio de la lista *)
        fun emit i = ilist := i :: (!ilist)
        
        (* result es usada por munchExp para facilitar la idea de aplicar
           efectos laterales y luego devolver un registro. 
       
           gen es una funcion que toma un tigertemp.temp y genera instrucciones
           (mediante emit), usandolo como registro destino *)
        fun result gen = let val t = tigertemp.newtemp() in gen t; t end
        
        
        fun munchExp e = 
            case e 
            of CONST i => (* int *)
                 result(fn r => emit(OPER{assem="movl $" ^ intToString i ^ ", `d0\n",
                                          src=[], 
                                          dst=[r], 
                                          jump=NONE}))
		     | NAME l => 
		         result(fn r => emit(OPER{assem="movl $" ^ l ^ ", `d0\n",
                                          src=[], 
                                          dst=[r], 
                                          jump=NONE})) (* tigertemp.label *)
                                          
		     | TEMP t => t (* tigertemp.temp *)
		     | BINOP(DIV, e1, e2) => 
		         let
		             val r1 = munchExp e2
		             val r2 = munchExp e1		             
		         in
	                 result(fn r => ((emit(MOVE{assem = "movl `s0, `d0\n",
                                    src=r2, 
                                    dst="%eax"}));
                                     
                                    (emit(OPER{assem = "movl `s0, `d0\n",
                                     src=["%eax"], 
                                     dst=["%edx"],
                                     jump=NONE}));                                     
                                     
                                    (emit(OPER{assem = "shrl $0x1F, `d0\n",
                                     src=["%edx"], 
                                     dst=["%edx"],
                                     jump = NONE}));
                                                     
                                    (emit(OPER{assem = "negl `d0\n",
                                     src=["%edx"], 
                                     dst=["%edx"],
                                     jump = NONE}));
                                                                                                              
                                    (emit(OPER{assem = "idiv `s0\n",
                                     src=[r1], 
                                     dst=["%eax", "%edx"],
                                     jump=NONE}));
                                    
                                    (emit(MOVE{assem = "movl `s0, `d0\n",
                                     src="%eax",
                                     dst=r}))))		         
		         end
		     | BINOP(bop, e1, e2) => (* binop*exp*exp *)
		         let
		             fun opcode PLUS = "addl"
		               | opcode MINUS = "subl"
		               | opcode MUL = "imul"
		               (*| opcode DIV = "idiv"*)
		               | opcode AND = "andl"
		               | opcode OR = "orl"
		               | opcode LSHIFT = "shll"
		               | opcode RSHIFT = "shrl"
		               | opcode ARSHIFT ="sarl"
		               | opcode XOR = "xorl"
		         in
		             let
		                 val r1 = munchExp e2
		                 val r2 = munchExp e1
		             in
		                 result(fn r => ((emit(MOVE{assem = "movl `s0, `d0\n",
                                        src=r2, 
                                        dst=r}));
                                         
                                        (emit(OPER{assem = opcode bop ^ " `s1, `d0\n",
                                        src=[r, r1], 
                                        dst=[r],
                                        jump=NONE}))))
                     end
		         end
		     
		     | MEM(BINOP(PLUS, CONST i, TEMP t)) =>
		         result(fn r => emit(OPER{assem="movl " ^ intToString i ^ "(`s0), `d0\n",
                                          src=[t], 
                                          dst=[r],
                                          jump=NONE}))		
		     | MEM e => (* exp *)
                 result(fn r => emit(MOVE{assem="movl (`s0), `d0\n",
                                          src=munchExp e, 
                                          dst=r}))		     
                 
		     | CALL(e, args) => raise Fail "Error interno (munchExp): call" (* exp*exp list *)
		     | ESEQ(s, e) => (* stm*exp *)
		         (munchStm s;
		         result(fn r => emit(MOVE{assem="movl `s0, `d0\n",
		                                  src=munchExp e,
		                                  dst=r})))
		     
        (* aplica el algoritmo de maximal munch al statement, produciendo
           instrucciones que se guardan en ilist (mediante emit). 
           
           VER IMPLEMENTACION DE EJEMPLO EN PAG 204. *)
        and munchStm (s: tigertree.stm) =
            case s 
            of SEQ(a, b) => (munchStm a; munchStm b)
             | tigertree.MOVE(TEMP t1 , MEM(BINOP(PLUS, CONST i, TEMP t2))) =>
                    emit(OPER{assem="movl " ^ intToString i ^ "(`s0), `d0\n",
                         src=[t2], 
                         dst=[t1],
                         jump=NONE})
                         
             | tigertree.MOVE(MEM(BINOP(PLUS, CONST i, TEMP t1)), TEMP t2) =>
                    emit(OPER{assem="movl `s0, " ^ intToString(i) ^ "(`s1)\n",
                         src=[t2, t1], 
                         dst=[],
                         jump=NONE})
                                 
             | tigertree.MOVE(MEM(BINOP(PLUS, CONST i1, TEMP t1)), MEM(BINOP(PLUS, CONST i2, TEMP t2))) =>
                    let val r = tigertemp.newtemp()
                        val _ = print("En el caso dudoso con temp: " ^ r ^ " \n")
                    in
                        (emit(OPER{assem="movl " ^ intToString i2 ^ "(`s0), `d0\n",
                             src=[t2], 
                             dst=[r],
                             jump=NONE});
                        emit(OPER{assem="movl `s0, " ^ intToString(i1) ^ "(`s1)\n",
                             src=[r, t1], 
                             dst=[],
                             jump=NONE})   )                      
                    end
                         
                                                       
             | tigertree.MOVE(MEM(BINOP(PLUS, CONST i1, TEMP t1)), CONST i2) =>
                    emit(OPER{assem="movl $" ^ intToString(i2) ^ ", " ^ intToString(i1) ^ "(`s0)\n", 
                         src=[t1], 
                         dst=[],
                         jump=NONE})
                                                               
             (* lugar para casos particulares *)
             | tigertree.MOVE(MEM e1, MEM e2) =>
                let val t = tigertemp.newtemp()
                in
                    emit(OPER{assem = "movl (`s0), `d0\n",
                              src = [munchExp e2],
                              dst = [t],
                              jump=NONE});
                    emit(OPER{assem = "movl `s0, (`s1)\n",
                              src = [t, munchExp e1],
                              dst = [],
                              jump=NONE})
                end
             | tigertree.MOVE(MEM(CONST i), e) =>
                    emit(OPER{assem = "movl `s0, " ^ intToString i ^ "\n",
                              src = [munchExp e],
                              dst = [],
                              jump = NONE})
             | tigertree.MOVE(MEM e1, e2) =>
                    emit(OPER{assem = "movl `s0, (`s1)\n",
                              src = [munchExp e2, munchExp e1],
                              dst = [],
                              jump=NONE})
             | tigertree.MOVE(e1, CALL e) =>
                  let
                      val r1 = munchExp e1
                      val r2 = (munchStm(EXP(CALL e)); "%eax")
                  in
                      emit(MOVE{assem = "movl `s0, `d0\n",
                                src = r2,
                                dst = r1})
                  end 
                
             | tigertree.MOVE(e1, e2) =>
                let val t = tigertemp.newtemp()
                in
                    emit(MOVE{assem = "movl `s0, `d0\n",
                              src = munchExp e2,
                              dst = t});
                    emit(MOVE{assem = "movl `s0, `d0\n",
                              src = t,
                              dst = munchExp e1})
                end
             | EXP(CALL(NAME f, args)) =>
                ((*saveCallerSaves();*)
                 emit(OPER{
                    assem = "#guardando caller\n",
                    src = [],
                    dst = tigerframe.callersaves,
                    jump = NONE
                 });
                 emit(OPER{assem = "call " ^ f ^ "\n",
                           src = munchArgs args,
                           dst = calldefs,
                           jump = NONE});
                 if length args > 0 then
                    emit(OPER{assem = "addl $" ^ intToString(length args * wSz) ^ ", "^ sp ^"\n", 
                              src = [],
                              dst = [],
                              jump = NONE})
                 else ();
                 emit(OPER{
                    assem = "#recuperando caller\n",
                    src = tigerframe.callersaves,
                    dst = [],
                    jump = NONE
                 })                 
                 (*restoreCallerSaves()*))
             | EXP e => emit(MOVE{assem = "movl `s0, `d0\n",
                                  src = munchExp e,
                                  dst = tigertemp.newtemp()})
             (* espacio para casos particulares *)
             | CJUMP(relop, CONST i, CONST j, l1, l2) =>
                let val que = case relop 
                              of EQ => i = j
                               | NE => i <> j
                               | LT => i < j
	                           | GE => i >= j
	                           | GT => i > j
	                           | LE => i <= j
	                           | ULT => i < j
	                           | UGE => i >= j
	                           | ULE => i <= j
	                           | UGT => i > j
	                val l' = if que then l1 else l2
	            in
	                emit(OPER{assem = "jmp " ^ l' ^ "\n",
	                          src = [],
	                          dst = [],
	                          jump = SOME [l1, l2]})
	            end
             | CJUMP(relop, e, CONST i, l1, l2) =>
                let val _ = emit(OPER{assem = "cmpl $" ^ intToString i ^ ", `s0\n",
                                      src = [munchExp e],
                                      dst = [],
                                      jump = NONE})
                in
                    emit(OPER{assem = relOp relop ^ l1 ^ "\n", 
                              src = [],
                              dst = [],
                              jump = SOME [l1, l2]})
                end
             | CJUMP(relop, CONST i, e, l1, l2) =>
                let val _ = emit(OPER{assem = "cmpl `s0, $" ^ intToString i ^ "\n",
                                      src = [munchExp e],
                                      dst = [],
                                      jump = NONE})
                in
                    emit(OPER{assem = relOp relop ^ l1, 
                              src = [],
                              dst = [],
                              jump = SOME [l1, l2]})
                end                
             | CJUMP(relop, e1, e2, l1, l2) =>
                let val _ = emit(OPER{assem = "cmpl `s0, `s1\n",
                                      src = [munchExp e2, munchExp e1],
                                      dst = [],
                                      jump = NONE})
                in
                    emit(OPER{assem = relOp relop ^ l1 ^ "\n", 
                              src = [],
                              dst = [],
                              jump = SOME [l1, l2]})
                end
             (* completar casos particulares de JUMP (si los hay) *)
             | JUMP(NAME e, labs) => emit(OPER{assem = "jmp " ^ e ^ "\n",
                                               src = [],
                                               dst = [],
                                               jump = SOME labs})
             | JUMP (e, labs) => emit(OPER{assem = "jmp `s0\n",
                                   src = [munchExp e],
                                   dst = [],
                                   jump = SOME labs})
             
             (* completar casos particulares de LABEL (si los hay) *)
             | tigertree.LABEL s => emit(LABEL{assem=s ^ ":\n", lab = s})
             
        and saveCallerSaves() =
            let fun emitedefs s =
                emit(OPER{assem = "pushl `s0\n",
                          src = [s],
                          dst = [],
                          jump = NONE})
            in
                List.map emitedefs tigerframe.callersaves
            end
        and restoreCallerSaves() =
            let fun emitedefs s =
                emit(OPER{assem = "popl `d0\n", 
                          src = [],
                          dst = [s],
                          jump = NONE})
            in
                List.app emitedefs (rev tigerframe.callersaves)
            end
        and munchArgs params =
            let fun munchArgsSt [] = ()
                  | munchArgsSt (h :: t) =
                    let
                        val _ =    
                            case h
                            of CONST i => emit(OPER{assem = "pushl $" ^ intToString i ^ "\n",
                                               src = [],
                                               dst = [],
                                               jump = NONE})
                             | NAME n => emit(OPER{assem = "pushl $" ^ n ^ "\n",
                                               src = [],
                                               dst = [],
                                               jump = NONE})
                             | TEMP t => emit(OPER{assem = "pushl `s0\n",
                                               src = [t],
                                               dst = [],
                                               jump = NONE})
                               (* HACER CASOS PARTICULARES *)
                             | e => let val t = tigertemp.newtemp()
                                        val m = MOVE{assem = "movl `s0, `d0\n",
                                                     src = munchExp e,
                                                     dst = t}
                                    in
                                        (emit m;
                                        emit(OPER{assem = "pushl `s0\n",
                                                  src = [t],
                                                  dst = [],
                                                  jump = NONE}))
                                        
                                    end
                        val _ = munchArgsSt t
                    in 
                        ()
                    end
            in
                (munchArgsSt(List.rev params); []) (* ver orden de argumentos *)
            end
    in
        (munchStm stm; rev (!ilist))
    end
end
