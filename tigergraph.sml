
structure tigergraph :> tigergraph =
struct
    type node = int
    type graph = {vertices: (node list) ref, aristas: ({a1: node, a2: node} list) ref }
    
    fun nodes {vertices = ref vs, aristas = _} = vs
    fun edges {vertices = _, aristas = ref ars} = ars
    fun succNode node1 {vertices = _, aristas = ref ars} = map (#a2) (List.filter (fn {a1= n1, a2 = n2} => n1 = node1) ars)
    fun predNode node2 {vertices = _, aristas = ref ars} = map (#a1) (List.filter (fn {a1= n1, a2 = n2} => n2 = node2) ars)
    fun adj n g = tigerutils.unionList (predNode n g) (succNode n g) 
    fun eq (n1,n2) = n1 = n2
    fun newGraph () = {vertices= ref [], aristas= ref [] }
    fun maxInt xs = foldl (Int.max) 0 xs
    fun newNode {vertices = vs, aristas = _} = let (*val n = (maxInt vs)+1*)
                                                       val n = case (!vs)
                                                               of [] => 0
                                                                | vs' => List.hd vs' + 1
                                                       val _ = vs := (n::(!vs))
                                                   in n
                                                   end
    fun mk_edge {origen= n1, destino= n2} {vertices = ref vs, aristas = ar as (ref ars)} = let val t1 = tigerutils.isinList n1 vs
                                                                                       val t2 = tigerutils.isinList n2 vs
                                                                                       val t3 = not (tigerutils.isinList {a1 = n1, a2 = n2} ars)
                                                                                       val _ = if t1 andalso t2 andalso t3 
                                                                                                    then ar := ({a1 = n1, a2 = n2}::ars) 
                                                                                                    else ()(*raise GraphEdge*)
                                                                                   in ()
                                                                                   end
    fun rm_edge {origen= n1, destino= n2} {vertices = ref vs, aristas = ar as (ref ars)} = let val t1 = tigerutils.isinList {a1 = n1, a2 = n2} ars
                                                                                       val _ = if t1 then ar := List.filter (fn x => x <> {a1= n1, a2 = n2}) (ars) 
                                                                                                     else ()(*raise GraphEdge*)
                                                                                    in ()
                                                                                    end
    fun nodename n = Int.toString(n)
    
    fun printGraph (g: graph) (show: node -> string) = 
        let
            (*fun showEdge {a1, a2} = "(" ^ nodename a1 ^ ", " ^ nodename a2 ^ ")"   *)
            fun showEdge {a1, a2} = show a1 ^ "->" ^ show a2
            val nodeList = List.map show (!(#vertices(g)))
            val edgeList = List.map showEdge (!(#aristas(g)))
            val nodesString = String.concatWith ", " nodeList
            val edgesString = String.concatWith ", " edgeList
        in
            print("Vertices: ");
            print(nodesString ^ "\n");
            print("Aristas: ");
            print(edgesString ^ "\n")
        end
end



