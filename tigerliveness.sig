signature tigerliveness =
sig
    (*type liveSet = unit Temp.Table.table * temp list
    type liveMap = liveSet Flow.Graph.Table.table*)

    datatype igraph =
        IGRAPH of {graph: tigergraph.graph,
                   tnode: tigertemp.temp -> tigergraph.node,
                   gtemp: tigergraph.node -> tigertemp.temp,
                   moves: (tigergraph.node * tigergraph.node) list}

    val interferenceGraph: 
            (*tigerflow.flowgraph -> unit*)
            tigerflow.flowgraph -> igraph * (tigergraph.node -> tigertemp.temp list)

    val show: igraph -> unit
end

