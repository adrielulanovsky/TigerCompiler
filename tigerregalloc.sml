structure tigerregalloc :> tigerregalloc =
struct
    type allocation = (tigerframe.register, tigertemp.temp) tigertab.Tabla
    fun alloc (instrs, frame) = 
        let
            val tigerflow.FGRAPH fg = #1(tigerflow.instrs2graph instrs)
            
            val spCost = fn _ => 1
            (*val spCost = fn _ => Random.range (1, 100) (Random.newgen ())*)
            (*fun spCost n = List.length (tigertab.tabSaca(n, #use(fg)))
                         + List.length (tigertab.tabSaca(n, #def(fg)))*)
                         
            val ig = #1(tigerliveness.interferenceGraph (tigerflow.FGRAPH fg))
            val init = tigerframe.tempMap
            
            val (colorAlloc, spilled) = tigercolor.color {
                                            interference = ig,
                                            initial = init,
                                            spillCost = spCost,
                                            registers = tigerframe.registers
                                        }
                                        
            fun reescribirLaWea spills f = 
                let
                    val memLocsTab = tigertab.fromList (List.map (fn s => (s, (fn (tigerframe.InFrame i) => i) (tigerframe.allocLocal f true))) spills)
                    fun memLocs x = tigertab.tabSaca (x, memLocsTab)
                    
                    
                    (* t es el temporario original, t' el nuevo asociado *)
                    fun makeFetch (t: tigertemp.temp) (t': tigertemp.temp) = 
                                        tigerassem.OPER{
                                            assem = "movl " ^ (tigerutils.intToString (memLocs t)) ^ "(%ebp), `d0\n",
                                            dst = [t'],
                                            src = [],
                                            jump = NONE
                                        }
                    fun makeStore t t'= tigerassem.OPER{
                                            assem = "movl `s0, " ^ (tigerutils.intToString (memLocs t)) ^ "(%ebp)\n",
                                            dst = [],
                                            src = [t'],
                                            jump = NONE
                                        }                                        
                    fun procInstr (l as tigerassem.LABEL _) = [l]
                      | procInstr i = 
                        let
                            val spillUses: tigertemp.temp list = tigerutils.removeDups (tigerutils.intersect spills (tigerassem.src2List i))
                            val spillDefs: tigertemp.temp list = tigerutils.removeDups (tigerutils.intersect spills (tigerassem.dst2List i))
                            val newtempsTab = tigertab.fromList (List.map (fn t => (t, tigertemp.newtemp())) (tigerutils.unionList spillUses spillDefs))
                            fun newtemps x = case tigertab.tabBusca (x, newtempsTab) 
                                             of SOME t => t
                                              | NONE => x
                            val fetches = List.map (fn t => makeFetch t (newtemps t)) spillUses
                            val stores = List.map (fn t => makeStore t (newtemps t)) spillDefs
                            
                            val newInstr = case i 
                                           of tigerassem.OPER{assem, dst, src, jump} => tigerassem.OPER{assem=assem, dst = List.map newtemps dst, src = List.map newtemps src, jump = jump}
                                            | tigerassem.MOVE{assem, dst, src} => tigerassem.MOVE{assem = assem, dst = newtemps dst, src = newtemps src}
                                            | x => x 
                        in 
                            fetches @ [newInstr] @ stores
                        end
                        
                    val finalInstrs = List.concat (List.map procInstr instrs)
                in
                    (finalInstrs, f)
                end
                 
            val (finalInstrs, finalAlloc) = if spilled = []
                                            then (instrs, colorAlloc)
                                            else alloc (reescribirLaWea spilled frame)
                                                 
        in
            (finalInstrs, finalAlloc) (* lista de instrs final y allocation final *)
        end
end

(*
addl T0, T1

movl -4(%ebp), T2 # T0
movl -8(%ebp, T3  # T1
addl T2, T3

addl T2, T4
movl T4, -8(%ebp)


addl T0, T0
movl -4(%ebp), T1
addl T1, T1
movl T1, -4(%ebp)*)
