signature tigerflow =
sig
    type BoolTable = (tigergraph.node, bool) tigertab.Tabla 
    type TempListTable = (tigergraph.node, tigertemp.temp list) tigertab.Tabla
    
	datatype flowgraph =
		FGRAPH of {control: tigergraph.graph, 
			   def: TempListTable,
			   use: TempListTable,
			   ismove: BoolTable}
    val instrs2graph: tigerassem.instr list -> flowgraph * tigergraph.node list
    val printGraph: flowgraph -> unit
end
