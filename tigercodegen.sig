signature tigercodegen =
sig
    (* codegen genera la lista de instrucciones de assembler para un stm.
       Debe recibir los arboles ya canonizados. 
       
       Como la asignacion de registros aun no se hizo, pasar una funcion que
       haga Temp.makestring para registros no especiales y que haga Frame.tempMap
       para registros especiales, a format como primer argumento *)
    val codegen : tigerframe.frame -> tigertree.stm -> tigerassem.instr list
end
