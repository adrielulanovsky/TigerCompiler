signature tigerassem =
sig
    type reg = string
    type temp = tigertemp.temp
    type label = tigertemp.label
    
    (* instruccion assembly sin asignacion de registros *)
    datatype instr = OPER of {assem: string, (* addl `s0, `d0
                                                subl `s1, `d1
                                                jmp `j0 *)
                                                
                                dst: temp list, (* registros destino (los `di) *)
                                src: temp list, (* registros fuente (los `si) *)
                                jump: label list option} (* posibles etiquetas de salto (las `ji) *)
                                
                    | LABEL of {assem: string, (* label vista en assembler *)
                                lab: label} (* label usada internamente *)
                                
                    (* idem a OPER pero debe hacer solo transferencia de datos *)
                    | MOVE of {assem: string,    
                                dst: temp,
                                src: temp}
    val format : (temp -> string) -> instr -> string
    val showInstr : instr -> string
    val src2List : instr -> temp list
    val dst2List : instr -> temp list
end
