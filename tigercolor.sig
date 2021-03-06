signature tigercolor =
sig
    type allocation = (tigertemp.temp, tigerframe.register) tigertab.Tabla
    val color: {interference: tigerliveness.igraph,
                initial: allocation,
                spillCost: tigergraph.node -> int,
                registers: tigerframe.register list}
                -> allocation * tigertemp.temp list
end
