structure tigercolor :> tigercolor =
struct
    type allocation = (tigertemp.temp, tigerframe.register) tigertab.Tabla
    type node = tigergraph.node
    type register = tigerframe.register
    type temp = tigertemp.temp
    type 'a stack = 'a tigerutils.stack
    type 'a set = 'a Splayset.set
    type ('key, 'a) map = ('key, 'a) Splaymap.dict
    (*
        IGRAPH of {graph: tigergraph.graph,
                   tnode: tigertemp.temp -> tigergraph.node,
                   gtemp: tigergraph.node -> tigertemp.temp,
                   moves: (tigergraph.node * tigergraph.node) list}*)
(*                   
            #graph(igraph): tigergraph.graph,
            #tnode(igraph): tigertemp.temp -> tigergraph.node,
            #gtemp(igraph): tigergraph.node -> tigertemp.temp,
            #moves(igraph): (tigergraph.node * tigergraph.node) list*)
                   
                   
                       
    fun color {interference = tigerliveness.IGRAPH igraph,
                initial =  initAlloc,
                spillCost = cost,
                registers = regs} = 
        let
            
            (* Funciones de impresion *)
            val nodename = tigergraph.nodename
            val showList = tigerutils.showList
            fun showNodeSet s = showList (map tigergraph.nodename (Splayset.listItems s))
            fun showNodeSetRef (ref s) = showNodeSet s
            fun showMoveSet s = showList (map (fn (a,b) => "(" ^ tigergraph.nodename a ^ ", " ^ tigergraph.nodename b ^ ")") (Splayset.listItems s))
            fun showMoveSetRef (ref s) = showMoveSet s
            fun showMap m showKey showVal = 
                let 
                    val l = Splaymap.listItems m
                in
                    String.concatWith ", " (List.map (fn (a, b) => showKey a ^ " -> " ^ showVal b) l)
                end            
            fun showMapRef (ref m) showKey showVal = showMap m showKey showVal

            
            (* Funciones y valores auxiliares *)
            val (SOME infinity) = Int.maxInt
            val emptyNodeSet: node set = Splayset.empty Int.compare
            val list2NodeSet: node list -> node set = tigerutils.curry Splayset.addList emptyNodeSet
            val singletonNode: node -> node set = Splayset.singleton Int.compare
            
            
            (* Funcion de comparacion para moveSet *)
            fun cmpMoves ((a, b), (c, d)) = 
                case Int.compare (a, c) 
                  of EQUAL => Int.compare (b, d)
                   | x => x
                               
            val emptyEdgeSet: (node * node) set = Splayset.empty cmpMoves
            val list2EdgeSet: (node * node) list -> (node * node) set = tigerutils.curry Splayset.addList emptyEdgeSet
            
            
            val list2NodeMap = List.foldr (fn ((n, r), m) => Splaymap.insert (m, n, r)) (Splaymap.mkDict Int.compare)
            
            (*fun update (t as ref t') f = t := f t'*)
            fun deleteSet (ss, k, _) = Splayset.delete(ss, k) handle NotFound => ss
            fun findMapa (m, k, s) = Splaymap.find(m, k) handle NotFound => raise Fail s
            fun updMapa (m as ref m') key f = m := Splaymap.insert(m', key, f(findMapa(m', key, "findMapa: updMapa")))
            fun formatEdges (edges: (node, node set) map) = List.concat (List.map (fn (n, ns) => List.map (fn n' => (n, n')) (Splayset.listItems ns)) (Splaymap.listItems edges))
            val sortPairList = Listsort.sort cmpMoves 
                   
            (* Sinonimos *)
            val graph: tigergraph.graph = #graph(igraph)
            val tnode: temp -> node = #tnode(igraph)
            val gtemp: node -> temp = #gtemp(igraph)
            val moves: (node * node) list = #moves(igraph)
            val nodes: node set = list2NodeSet (tigergraph.nodes graph)
            val edges: (node * node) list = List.map (fn {a1, a2} => (a1, a2)) (tigergraph.edges graph)
            
            (* Node work-lists, sets, and stacks. The following lists and sets are always
               mutually disjoint and every node is always in exactly one of the sets or lists. *)
               
            (* machine registers, preassigned a color *)
            val precolored: node set = list2NodeSet (List.map tnode (tigertab.tabClaves initAlloc))
            
            (* temporary registers, not precolored and not yet processed *)
            val initial: node set ref = ref (Splayset.difference (nodes, precolored))
            
            (* list of low-degree non-move-related nodes *)
            val simplifyWorklist: node set ref = ref emptyNodeSet
            
            (* low-degree move-related nodes *) 
            val freezeWorklist: node set ref = ref emptyNodeSet
            
            (* high-degree nodes *)
            val spillWorklist: node set ref = ref emptyNodeSet
            
            (* nodes marked for spilling during this round; initially empty *)
            val spilledNodes: node set ref = ref emptyNodeSet
            
            (* registers that have been coalesced; when u <— i is coalesced, 
               i is added to this set and u put back on some work-list (or vice versa). *)
            val coalescedNodes: node set ref = ref emptyNodeSet
            
            (* nodes successfully colored *)
            val coloredNodes: node set ref = ref emptyNodeSet
            
            (* stack containing temporaries removed from the graph *) 
            val selectStack: node stack ref = ref tigerutils.emptyStack     
            
            (* leus puto *)
            (*val _ = Splayset.app (fn n => tigerutils.push n selectStack) precolored*)
                    
                      
  
                      
                      
            (* Move sets. There are five sets of move instructions, and every move is in
               exactly one of these sets (after Build through the end of Main). *)
               
            (* moves that have been coalesced *)
            val coalescedMoves: (node * node) set ref = ref emptyEdgeSet
            
            (* moves whose source and target interfere *)
            val constrainedMoves: (node * node) set ref = ref emptyEdgeSet
            
            (* moves that will no longer be considered for coalescing *)
            val frozenMoves: (node * node) set ref = ref emptyEdgeSet
            
            (* moves enabled for possible coalescing *)
            val worklistMoves: (node * node) set ref = ref emptyEdgeSet
           
            (* moves not yet ready for coalescing *)
            val activeMoves: (node * node) set ref = ref emptyEdgeSet
            
            (* Other data structures *)
            
            (* the set of interference edges (u, v) in the graph. If (u, v) € adjSet then
               (v, u) € adjSet *)
            val adjSet: (node * node) set ref = ref emptyEdgeSet
            
            (* adjacency list representation of the graph, for each non-precolored 
               temporary u, adjList[u] is the set of nodes that interfere with u *)
            val adjList: (node, node set) map ref = ref(Splaymap.mkDict Int.compare)
            val _ = Splayset.app (fn n => adjList := Splaymap.insert(!adjList, n, emptyNodeSet)) nodes
            
            (* an array containing the current degree of each node *)
            val degree: (node, int) map ref = ref(Splaymap.mkDict Int.compare)
            val _ = Splayset.app (fn n => degree := (if Splayset.member (precolored, n)
                                                     then Splaymap.insert(!degree, n, infinity)
                                                     else Splaymap.insert(!degree, n, 0)) ) nodes
            
            (* a mapping from a node to the list of moves it is associated with *)
            val moveList: (node, (node * node) set) map ref = ref (Splaymap.mkDict Int.compare)
            
            (* when a move (u, v) has been coalesced, and v put in coalescedNodes,
               then alias(v) = u *)
            val alias: (node, node) map ref = ref (Splaymap.mkDict Int.compare)
            
            (* the color chosen by the algorithm for a node; for precolored nodes this is
               initialized to the given color *)
            (*val color: (node, register) map ref = ref(list2NodeMap(List.map (fn (t, r) => (tnode t, r)) (tigertab.tabAList initAlloc)))*)
            val color: (node, register) map ref = ref(list2NodeMap(List.map (fn n => (n, gtemp n)) (Splayset.listItems precolored)))
               
            (* nº de registros fisicos *)
            val k = List.length regs
            
            (* addEdge modifica las siguientes estructuras: 
               - adjSet
               - adjList
               - degree
            *)
            fun addEdge (u: node, v: node) = 
                (
                if not(Splayset.member(!adjSet, (u, v))) andalso u <> v
                then (adjSet := Splayset.addList(!adjSet, [(u, v), (v, u)]);
                     if not (Splayset.member(precolored, u))
                     then (updMapa adjList u (fn s => Splayset.add(s, v));
                          updMapa degree u (fn i => i+1))
                     else ()
                     ;
                     if not (Splayset.member(precolored, v))
                     then (updMapa adjList v (fn s => Splayset.add(s, u));
                          updMapa degree v (fn i => i+1))
                     else () )
                else ())
            
            (* build modifica las siguientes estructuras: 
               - moveList
               - workListMoves
               - lo que modifica AddEdge
            *)
            fun build () = 
                let
                    fun getMoves n = List.filter (fn (a,b) => a = n orelse b = n) moves
                    fun addToML n = moveList := Splaymap.insert( !moveList, n, list2EdgeSet (getMoves n))
                in
                    Splayset.app addToML nodes;
                    worklistMoves := list2EdgeSet moves;
                    List.app addEdge edges
                end
                
            
            
            (* Devuelve los moves con los que esta relacionado el nodo *)            
            fun nodeMoves (n: node): (node * node) set = 
                Splayset.intersection(
                    findMapa (!moveList, n, "findMapa: nodeMoves"), 
                    Splayset.union(!activeMoves, !worklistMoves) 
                )
            
            (* True si el nodo esta relacionado a algun move *)
            fun moveRelated (n: node): bool = not(Splayset.isEmpty (nodeMoves n))
            
            (* Inicializa las siguientes estructuras:
               - spillWorklist
               - freezeWorklist
               - simplifyWorklist
               Al finalizar, la estructura initial queda vacia.
             *)
            fun makeWorklist() =
                let
                    fun procNode (n: node) = 
                        (initial := deleteSet(!initial, n, "deleteSet: procNode");
                        if findMapa(!degree, n, "findMapa: makeWorkList") >= k
                        then spillWorklist := Splayset.add(!spillWorklist, n)
                        else if moveRelated(n)
                             then freezeWorklist := Splayset.add(!freezeWorklist, n)
                             else simplifyWorklist := Splayset.add(!simplifyWorklist, n))
                in
                    Splayset.app procNode (!initial)
                end
                                           
                        
            (* Devuelve el set nodos adyacentes al nodo dado *)
            fun adjacent (n: node): node set = 
                Splayset.difference(
                    findMapa(!adjList, n, "findMapa: adjacent"),
                    Splayset.union(list2NodeSet (!selectStack), !coalescedNodes)
                )
                
            (* Pasa los moves asociados a los nodos dados al conjunto de worklistMoves
               y los saca del conjunto de activeMoves.
               Modifica las siguientes estructuras:
               - activeMoves
               - worklistMoves
            *)
            fun enableMoves(nodes: node set) = 
				let
					fun procMove (m: (node * node)) = 
						if Splayset.member (!activeMoves, m)
						then (activeMoves := deleteSet(!activeMoves, m, "deleteSet: procMove");
						      worklistMoves := Splayset.add(!worklistMoves, m))
						else ()
					fun procNode (n: node) = Splayset.app procMove (nodeMoves(n))
					
				in
					Splayset.app procNode nodes
				end
            
            (* Decrementa el grado de un nodo. 
               Si el nodo pasa a tener grado no significativo, lo saca de la lista de nodos a spillear. 
               Si el nodo esta en un move, se agrega a lista de freeze y si no, a la de simplify.
               Modifica las siguientes estructuras:
               - degree
               - spillWorklist
               - freezeWorklist
               - simplifyWorklist
               - las que modifica enableMoves
            *) 
            fun decrementDegree (n: node) = 
                
				let
					val d = findMapa(!degree, n, "findMapa: decrementDegree")
				in
					degree := Splaymap.insert(!degree, n, d - 1);
					if d = k 
					then (enableMoves(Splayset.add(adjacent(n), n));
					      spillWorklist := deleteSet(!spillWorklist, n, "deleteSet: decrementDegree");
					      if moveRelated(n)
					      then freezeWorklist := Splayset.add(!freezeWorklist, n)
					      else simplifyWorklist := Splayset.add(!simplifyWorklist, n)
					      )
					else ()
				end
				
            (* Saca un nodo de la lista de simplify, lo guarda en el
               stack y baja un grado a todos los nodos adyacentes.
               Modifica las siguientes estructuras:
               - simplifyWorklist
               - selectStack
               - las que modifica decrementDegree
            *)
            fun simplify() =
				let
					val n = List.hd (Splayset.listItems (!simplifyWorklist))
				in(
					simplifyWorklist := deleteSet(!simplifyWorklist, n, "deleteSet: simplify");
					tigerutils.push n (selectStack);
					Splayset.app decrementDegree (adjacent n))
				end
		    
		    (* Saca un nodo de la lista de freeze y lo agrega a la de simplify
		       Modifica las siguientes estructuras:
		       - freezeWorklist
		       - simplifyWorklist
		    *)
		    fun addWorklist(n: node) =
		        if not(Splayset.member(precolored, n)) andalso
		           not( moveRelated(n) ) andalso
		           findMapa(!degree, n, "findMapa: addWorklist") < k
		        then ( freezeWorklist := deleteSet(!freezeWorklist, n, "deleteSet: addWorklist");
		               simplifyWorklist := Splayset.add(!simplifyWorklist, n))
		        else ()
		        
		    (* True si el primer nodo tiene grado no-significativo o
		       es precoloreado, o si interfiere con el segundo.
		    *)
		    fun ok(n: node, m: node): bool =  
		        (findMapa(!degree, n, "findMapa: ok") < k) orelse
		        Splayset.member(precolored, n) orelse
		        Splayset.member(!adjSet, (n, m))
		        
		    (* True si se cumple la condicion para coalescer conservativamente,
		       es decir, chequea que menos de k nodos tengan grado significativo. *)
		    fun conservative(nodes: node set): bool = 
		        List.length(
                    List.filter (fn n => findMapa(!degree, n, "findMapa: conservative") >= k)
                                (Splayset.listItems nodes)
                ) < k
            
            (* Obtiene el representante el nodo *)
            fun getAlias(n: node): node = 
                if Splayset.member(!coalescedNodes, n)
                then getAlias(findMapa(!alias, n, "findMapa: getAlias"))
                else n
                
            (* Modifica las siguientes estructuras:
               - freezeWorklist
               - spillWorklist
               - coalescedNodes
               - alias
               - moveList
               - las que modifica enableMoves
               - las que modifica decrementDegree
               - las que modifica addEdge
            *)
            fun combine(u: node, v: node) = 
                (if Splayset.member(!freezeWorklist, v)
                then freezeWorklist := deleteSet(!freezeWorklist, v, "deleteSet: combine then")
                else spillWorklist := deleteSet(!spillWorklist, v, "deleteSet: combine else") ;
                
                coalescedNodes := Splayset.add(!coalescedNodes, v);
                alias := Splaymap.insert(!alias, v, u);
                moveList := Splaymap.insert(!moveList,
                    u,
                    Splayset.union(
                        findMapa(!moveList, u, "findMapa: combine u"),
                        findMapa(!moveList, v, "findMapa: combine v")
                    )
                );
                enableMoves(singletonNode(v));
                
                Splayset.app (fn t => (addEdge(t, u); decrementDegree(t))) (adjacent(v));
                if (findMapa(!degree, u, "findMapa: combine if") >= k) andalso 
                    Splayset.member(!freezeWorklist, u)
                then (freezeWorklist := deleteSet(!freezeWorklist, u, "deleteSet: combine then 2");
                     spillWorklist := Splayset.add(!spillWorklist, u))
                else () )
                
            (* Coalesce la vaina.
               Modifica las estructuras:
               - worklistMoves
               - coalescedMoves
               - constrainedMoves
               - activeMoves
               - lo que modifica addWorklist
               - lo que modifica combine
               - la wea
              
            *)
            fun coalesce() =
                let 
                    val (m as (x, y)) = List.hd (Splayset.listItems(!worklistMoves))
                    val (x', y') = (getAlias x, getAlias y)
                    val (u, v) = if Splayset.member(precolored, y')
                                 then (y', x')
                                 else (x', y')
                    
                in
                    (worklistMoves := deleteSet(!worklistMoves, m, "deleteSet: coalesce");
                    if u = v 
                    then (coalescedMoves := Splayset.add(!coalescedMoves, m);
                         addWorklist(u))
                    else if Splayset.member(precolored, v) orelse
                            Splayset.member(!adjSet, (u, v))
                         then (constrainedMoves := Splayset.add(!constrainedMoves, m);
                               addWorklist(u);
                               addWorklist(v))
                         else if (Splayset.member(precolored, u) andalso 
                                 (tigerutils.forall (fn t => ok(t, u)) (adjacent(v))))
                                     orelse
                                 (not (Splayset.member(precolored, u)) andalso
                                 conservative(Splayset.union(adjacent(u), adjacent(v))))
                              then (coalescedMoves := Splayset.add(!coalescedMoves, m);
                                    combine(u, v);
                                    addWorklist(u))
                              else activeMoves := Splayset.add(!activeMoves, m))
                                 
                end
                
            (* Freezea la chingada de un nodo.
               Modifica las siguientes estructuras:
               - activeMoves
               - frozenMoves
               - freezeWorklist
               - simplifyWorklist
            *)
            fun freezeMoves(u: node) = 
                let
                    fun procMove (m as (x, y)) = 
                        let
                            val v = if getAlias(y) = getAlias(u)
                                    then getAlias(x)
                                    else getAlias(y)
                        in
                            activeMoves := deleteSet(!activeMoves, m, "deleteSet: procMove");
                            frozenMoves := Splayset.add(!frozenMoves, m);
                            if (Splayset.isEmpty (nodeMoves(v))) andalso
                               (findMapa(!degree, v, "findMapa: procMove") < k)
                            then (freezeWorklist := deleteSet(!freezeWorklist, v, "deleteSet: procMove then " ^ nodename v);
                                 simplifyWorklist := Splayset.add(!simplifyWorklist, v))
                            else ()
                        end
                         
                in
                    Splayset.app procMove (nodeMoves(u))
                end
                   
            (* Freezea las chingadas.
               Modifica las siguientes estructuras:
               - freezeWorklist
               - simplifyWorklist
               - las que modifica freezeMoves
            *) 
            fun freeze() =
                let
                    val u = List.hd (Splayset.listItems (!freezeWorklist))
                in
                    freezeWorklist := deleteSet(!freezeWorklist, u, "deleteSet: freeze");
                    simplifyWorklist := Splayset.add(!simplifyWorklist, u);
                    freezeMoves(u)
                end    
                
            (* Selecciona el mejor nodo a spillear (segun la funcion de costo),
               lo saca de la lista de spill y lo agrega a la de simplify.
               Luego freezea su chingada.
               Modifica las siguientes estructuras:
               - spillWorklist
               - simplifyWorklist
               - las que modifica freezeMoves
            *)
            fun selectSpill() = 
                let
                    fun cmp (n1, n2) = Int.compare (cost n1, cost n2)
                    val m = tigerutils.minBy cmp (Splayset.listItems(!spillWorklist))
                in
                    (spillWorklist := deleteSet(!spillWorklist, m, "deleteSet: selectSpill");
                    simplifyWorklist := Splayset.add(!simplifyWorklist, m);
                    freezeMoves(m))
                end
                
            fun assignColors() =
                let
                    fun addColor (n:node) = 
                        color := Splaymap.insert(
                                    !color, 
                                    n,
                                    findMapa(!color, getAlias(n), "findMapa: addColor " ^ nodename n ^ " (alias " ^ nodename (getAlias(n)) ^ ")")
                                 )
                        
                                                                                        
                    fun procNode (n: node) = 
                        let
                            val okColors = ref regs
                            fun procNode' (w: node) = 
                                    if Splayset.member(
                                           Splayset.union(!coloredNodes, precolored),
                                           getAlias(w)
                                       ) 
                                    then okColors := tigerutils.remove (findMapa(!color, getAlias(w), "findMapa: procNode'")) (!okColors) 
                                                     
                                    else ()
                                    
                        in
                            (Splayset.app procNode'  (findMapa(!adjList, n, "findMapa: procNode"));
                            if (!okColors) = []
                            then spilledNodes := Splayset.add(!spilledNodes, n)
                            else (coloredNodes := Splayset.add(!coloredNodes, n);
                                 color := Splaymap.insert(
                                              !color,
                                              n,
                                              List.hd (!okColors)
                                          )))
                        end
                in
                    List.app procNode (!selectStack);
                    selectStack := [];
                    Splayset.app addColor (!coalescedNodes)
                end
                
                fun repeat() = if not (Splayset.isEmpty (!simplifyWorklist))
                               then simplify()
                               else if not (Splayset.isEmpty (!worklistMoves))
                                    then coalesce()
                                    else if not (Splayset.isEmpty (!freezeWorklist))
                                         then freeze()
                                         else if not (Splayset.isEmpty (!spillWorklist))
                                              then selectSpill()
                                              else ()
                fun printAll() = print(
                                       "\nadjSet: " ^ (showMoveSetRef adjSet) ^ 
                                       "\nmoveList: " ^ (showMapRef moveList nodename showMoveSet) ^
                                       "\nadjList: " ^ (showMapRef adjList nodename showNodeSet) ^
                                       "\ndegree: " ^ (showMapRef degree nodename Int.toString) ^
                                       "\nprecolored: " ^ (showNodeSet precolored) ^
                                       "\ninitial: " ^ (showNodeSetRef initial) ^ 
                                       "\nnodes: " ^ (showNodeSet nodes) ^ 
                                       "\nspillWorklist: " ^ (showNodeSetRef spillWorklist) ^ 
                                       "\nfreezeWorklist: " ^ (showNodeSetRef freezeWorklist) ^ 
                                       "\nsimplifyWorklist: " ^ (showNodeSetRef simplifyWorklist) ^ 
                                       "\nworklistMoves: " ^ (showMoveSetRef worklistMoves) ^ 
                                       "\ncolors: " ^ (showMapRef color nodename tigerutils.id) ^ 
                                       "\nspilledNodes: " ^ (showNodeSetRef spilledNodes) ^ 
                                       "\ncoalescedNodes: " ^ (showNodeSetRef coalescedNodes) ^ 
                                       "\nselectStack: " ^ (showList (List.map nodename (!selectStack))) ^ 
                                       "\ncoloredNodes: " ^ (showNodeSetRef coloredNodes) ^ 
                                       "\ncontrainedMoves: " ^ (showMoveSetRef constrainedMoves) ^
                                       "\naliasDeLosNodos: " ^ (String.concatWith ", " (List.map (fn n => "(" ^ (nodename n) ^ ", " ^ (nodename (getAlias(n))) ^ ")") (Splayset.listItems (!coalescedNodes)))) ^ "\n")
                                                                                     
                fun main() = (build(); 
                             makeWorklist(); 
                             repeat(); 
                             while (not ((Splayset.isEmpty (!simplifyWorklist)) andalso
                                   (Splayset.isEmpty (!worklistMoves)) andalso 
                                   (Splayset.isEmpty (!freezeWorklist)) andalso
                                   (Splayset.isEmpty (!spillWorklist))))
                             do repeat();
                          
                             assignColors())
                             

                                       
                
                fun convColor color = let
                                        val lista = Splaymap.listItems color
                                        val listaTR = List.map (fn (n, r) => (gtemp n, r)) lista
                                        in
                                        tigertab.fromList listaTR
                                      end
        in
            (main();
            (convColor (!color), List.map gtemp (Splayset.listItems (!spilledNodes))))
        end
end

