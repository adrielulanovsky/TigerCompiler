structure tigerliveness :> tigerliveness =
struct
    (*type liveSet = unit Temp.Table.table * temp list
    type liveMap = liveSet Flow.Graph.Table.table*)

    datatype igraph =
        IGRAPH of {graph: tigergraph.graph,
                   tnode: tigertemp.temp -> tigergraph.node,
                   gtemp: tigergraph.node -> tigertemp.temp,
                   moves: (tigergraph.node * tigergraph.node) list}

    fun interferenceGraph (fg as (tigerflow.FGRAPH {control, def, use, ismove})) = 
        let
            (*Calculamos los conjuntos liveIn y liveOut*)
            val nodes = tigergraph.nodes control
            val n = List.length(nodes)
            val liveIn = List.tabulate (n, (fn _ => ref []))  : ((tigertemp.temp list) ref) list
            val liveOut = List.tabulate (n, (fn _ => ref [])) : ((tigertemp.temp list) ref) list
            val flagStop = ref false
            fun hacerN n =
                let
                    val liveInViejoN = !(List.nth (liveIn, n))
                    val liveOutViejoN = !(List.nth (liveOut, n))
                    val useN = ( case tigertab.tabBusca (n, use)
                                   of SOME t => t
                                    | NONE => raise Fail ("Error interno (useN)") )
                    val defN = ( case tigertab.tabBusca (n, def)
                                   of SOME t => t
                                    | NONE => raise Fail ("Error interno (defN)") )
                    val _ = List.nth (liveIn, n) := tigerutils.unionList useN (tigerutils.diffList liveOutViejoN defN)
                    val succN = tigergraph.succNode n control (* ::listaDeNodos *)
                    val _ = List.nth (liveOut, n) := tigerutils.concatUnionList (List.map (fn nodo => !(List.nth (liveIn, nodo))) succN)
                in
                    (liveInViejoN = !(List.nth (liveIn, n))) andalso (liveOutViejoN = !(List.nth (liveOut, n)))
                end
            fun hacer () = tigerutils.andList (List.tabulate (n, hacerN))
            val _ = while (not (!flagStop))
                    do (flagStop := hacer ())
            (*Creamos el grafo de interferencia*)    
            val tnodeTab: (tigertemp.temp, tigergraph.node) tigertab.Tabla ref = ref (tigertab.tabNueva())    
            val gtempTab: (tigergraph.node, tigertemp.temp) tigertab.Tabla ref = ref (tigertab.tabNueva())  
            val moves = ref []
            val g = tigergraph.newGraph ()

            val temps = tigerutils.concatUnionList (List.map (tigerutils.removeDups o (#2)) ((tigertab.tabAList use) @ (tigertab.tabAList def)))
            
            fun addNode t =
                case tigertab.tabBusca(t, !tnodeTab) 
                  of SOME n => ()
                   | NONE => (let val n = tigergraph.newNode g
                              in
                                  tigertab.inserta(t, n, tnodeTab);
                                  tigertab.inserta(n, t, gtempTab)
                    
                              end)
            
            val _ = List.app addNode temps
            
            fun toNode b = tigertab.busca(b, !tnodeTab, "error interno (toNode)")
            
            fun procNode n = 
                let
                    val nDefs = tigertab.busca(n, def, "error interno (procNode nDefs)")
                    val nUses = tigertab.busca(n, use, "error interno (procNode nUses)")
                    val nLiveOut = !(List.nth(liveIn, n))
                    
                    fun addEdges a xs = List.app (fn b => tigergraph.mk_edge {origen = toNode a, destino = toNode b} g) xs
                       
                    val _ = case tigertab.busca(n, ismove, "error interno (procNode)")
                              of true => (addEdges (List.hd nDefs) (List.filter (fn y => y <> (List.hd nUses)) nLiveOut);
                                         moves := (toNode (List.hd nDefs), toNode (List.hd nUses))::(!moves))
                               | false => List.app (fn x => addEdges x nLiveOut) nDefs
                in
                    ()
                end
            
            val _ = List.app procNode nodes
            
            (* AGREGANDO COMPLETO *)
            val regTemps: tigertemp.temp list = tigerframe.registers
            val _ = List.app addNode regTemps
            
            val edges = List.concat (List.map (fn t => List.map (fn w => {origen = toNode t,destino = toNode w}) (tigerutils.diffList regTemps [t])) regTemps)
            val _ = List.app (fn e => tigergraph.mk_edge e g) edges
            
            (* DEJANDO DE AGREGAR COMPLETO *)
            
            val iGraph = IGRAPH {
                graph = g,
                tnode = fn t => case tigertab.tabBusca(t, !tnodeTab)
                                  of SOME n => n
                                   | NONE => raise Fail ("Error interno (tnode): no se encuentra " ^ t),
                gtemp = fn t => case tigertab.tabBusca(t, !gtempTab)
                                  of SOME n => n
                                   | NONE => raise Fail "Error interno (gtemp)",
                moves = !moves
            }            
        in
            (iGraph, fn n => !(List.nth(liveOut, n)))
        end

    fun show (IGRAPH {graph,tnode,gtemp,moves}) =
        let
            val nodes = tigergraph.nodes graph
            fun printTNode n = print("  " ^ gtemp n ^ " -> " ^ tigergraph.nodename(tnode(gtemp n)) ^ "  ")
            fun printGTemp n = print("  " ^ (tigergraph.nodename n) ^ " -> " ^ gtemp n ^ "  ")
            val _ = (tigergraph.printGraph graph gtemp;
                    print("TNODE: \n");
                    List.app printTNode nodes;
                    print("\nGTEMP: \n");
                    List.app printGTemp nodes;
                    print("\nMOVES: \n");
                    List.app (fn (a, b) => print("(" ^ tigergraph.nodename a ^ ", " ^ tigergraph.nodename b ^ ")\t")) moves)
                    
        in
            ()
        end
end

