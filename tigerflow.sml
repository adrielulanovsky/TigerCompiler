structure tigerflow :> tigerflow =
struct
    type BoolTable = (tigergraph.node, bool) tigertab.Tabla 
    type TempListTable = (tigergraph.node, tigertemp.temp list) tigertab.Tabla
    datatype flowgraph =
		FGRAPH of {control: tigergraph.graph, 
			   def: TempListTable,
			   use: TempListTable,
			   ismove: BoolTable}
			   
    fun instrs2graph instrs =
    let
        val showList = tigerutils.showList
        val g = tigergraph.newGraph ()
        val tabUse: TempListTable ref = ref (tigertab.tabNueva ()) 
        val tabDef: TempListTable ref = ref (tigertab.tabNueva () )
        val tabMoves: BoolTable ref = ref (tigertab.tabNueva ()) 
        val tabLabels: (tigertemp.label, tigergraph.node) tigertab.Tabla ref = ref (tigertab.tabNueva ())
        val nlist = ref []
        val badRegs = ["%esp", "%ebp"]
        
        fun procInstrs () =
            let
                fun procInstr (tigerassem.OPER {assem, dst, src, jump}) =
                    let
                        val n = tigergraph.newNode g;
                    in
                            tigertab.inserta(n, tigerutils.diffList src badRegs, tabUse);
                            tigertab.inserta(n, tigerutils.diffList dst badRegs, tabDef);
                            tigertab.inserta(n, false, tabMoves);
                            nlist := (!nlist) @ [n]
                        
                    end
                  | procInstr (tigerassem.LABEL {assem, lab}) =
                    let
                        val n = tigergraph.newNode g;
                    in
                        tigertab.inserta(n, [], tabUse);
                        tigertab.inserta(n, [], tabDef);            
                        tigertab.inserta(n, false, tabMoves);
                        tigertab.inserta(lab, n, tabLabels);
                        nlist := (!nlist) @ [n]
                    end            
                  | procInstr (tigerassem.MOVE {assem, dst, src}) =
                    if tigerutils.isAnyInList badRegs [src, dst] then
                        procInstr (tigerassem.OPER{assem=assem, dst=[dst], src=[src], jump=NONE})
                    else
                        let
                            val n = tigergraph.newNode g;
                        in
                            tigertab.inserta(n, [src], tabUse);
                            tigertab.inserta(n, [dst], tabDef);
                            tigertab.inserta(n, true, tabMoves);
                            nlist := (!nlist) @ [n]
                        end
            in
                List.app procInstr instrs
            end
        fun addEdges () = 
            let
                val edges = ListPair.zip ((!nlist), (List.tl (!nlist)))
            in
                List.app (fn (n1, n2) => tigergraph.mk_edge {origen=n1, destino=n2} g) edges
            end
        fun addJmpEdges() = 
            let
                fun getLabNode j = case tigertab.tabBusca (j, !tabLabels)
                                     of SOME x => x
                                      | NONE => raise Fail ("Error interno (getLabNode)" ^  j)
                                      
                fun addEdge ((tigerassem.OPER {assem, dst, src, jump}), i) =
                    (case jump
                       of NONE => ()
                        | SOME xs => List.app (fn j => tigergraph.mk_edge {origen=i, destino=getLabNode j} g) xs)
                  | addEdge _ = ()
                
            in
                tigerutils.zipWith addEdge instrs (tigerutils.upTo (List.length instrs))
            end
            
        val _ = (procInstrs();
                 addEdges(); 
                 addJmpEdges())
        
        val fgraph = FGRAPH {
            control = g,
            def = !tabDef,
            use = !tabUse,
            ismove = !tabMoves
        }
        
    in
        (fgraph, !nlist)
    end

    fun printGraph ((FGRAPH f): flowgraph) =
        let
            val g = #control(f)
            fun printTab t s = print(s ^ ": " ^ (String.concatWith "\n" (List.map (fn (n, ts) => tigergraph.nodename n ^ " -> " ^ (String.concatWith ", " ts) ^ " ") (tigertab.tabAList t))) ^ "\n")
        in
            print("FLOWGRAPH: ");
            tigergraph.printGraph g tigergraph.nodename;
            printTab (#use(f)) "Use";
            printTab (#def(f)) "Def"
        end
end
