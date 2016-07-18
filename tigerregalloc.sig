signature tigerregalloc =
sig
    type allocation = (tigerframe.register, tigertemp.temp) tigertab.Tabla
    val alloc: tigerassem.instr list * tigerframe.frame -> tigerassem.instr list * allocation
end
