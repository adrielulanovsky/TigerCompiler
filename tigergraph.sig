signature tigergraph = 
sig
	type graph
	type node = int

	val nodes: graph -> node list
	val edges: graph -> {a1: node, a2: node} list 
	val succNode: node -> graph -> node list
	val predNode: node -> graph -> node list
	val adj: node -> graph -> node list
	val eq: node*node -> bool

	val newGraph: unit -> graph
	val newNode: graph -> node
(*	exception GraphEdge*)
	val mk_edge: {origen: node, destino:node} -> graph -> unit
	val rm_edge: {origen: node, destino:node} -> graph -> unit
	
	val nodename: node -> string (* for debugging *) 
	val printGraph: graph -> (node -> string) -> unit
end
