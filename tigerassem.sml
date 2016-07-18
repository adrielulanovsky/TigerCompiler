structure tigerassem :> tigerassem =
struct
    type reg = string
    type temp = tigertemp.temp
    type label = tigertemp.label
    val showList = tigerutils.showList
                                    
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
    (* format(m)(i) formatea una instruccion de assembler a string; 
        m es una funcion que muestra la asignacion de registros (o quizas
        solo el nombre) de cada temp. *)

    fun format saytemp =
        let fun speak(assem,dst,src,jump) =
	            let fun saylab s = s
		            fun f(#"`":: #"s":: i::rest) = 
		                    (explode(saytemp(List.nth(src,ord i - ord #"0"))) @ f rest)
		              | f( #"`":: #"d":: i:: rest) = 
		                    (explode(saytemp(List.nth(dst,ord i - ord #"0"))) @ f rest)
		              | f( #"`":: #"j":: i:: rest) = 
		                    (explode(saylab(List.nth(jump,ord i - ord #"0"))) @ f rest)
		              | f( #"`":: #"`":: rest) = #"`" :: f rest
		              | f( #"`":: _ :: rest) = raise Fail "bad Assem format"
		              | f(c :: rest) = (c :: f rest)
		              | f nil = nil
	            in (*print("En tigerassem.format...\n\tASSEM=" ^ assem ^ "\n\tSRC=" ^ showList src ^ "\n\n\tDST=" ^ showList dst ^ "\n\n\t"^ implode(f(explode assem)) ^ "\n\n");*) implode(f(explode assem))
	            end
        in 
            fn(OPER{assem, dst, src, jump = NONE}) => "\t" ^ speak(assem, dst, src, [])
            | (OPER{assem, dst, src, jump = SOME j}) => "\t" ^ speak(assem, dst, src, j)
            | LABEL{assem, ...} => assem
            | MOVE{assem, dst, src} => if not (String.isSubstring "(" assem) andalso saytemp src = saytemp dst 
                                       then "" 
                                       else "\t" ^ speak(assem, [dst], [src], [])        
                                        (*"\t" ^ speak(assem, [dst], [src], [])*)
        end    
    (*fun format saytemp =
        let fun speak(assem, src, dst, jump) =
            let val saylab = tigertab.name
                fun f (#"`" :: #"s" :: i :: rest) = (explode(saytemp(safeNth(src, ord(i) - ord(#"o")))) @ f rest)
                  | f (#"`" :: #"d" :: i :: rest) = (explode(saytemp(safeNth(dst, ord(i) - ord(#"o")))) @ f rest)
                  | f (#"`" :: #"j" :: i :: rest) = (explode(saylab(safeNth(jump, ord(i) - ord(#"o")))) @ f rest)
                  | f (#"`" :: #"`" :: rest) = #"`" :: f rest
                  | f (#"`" :: _ :: rest) = raise Fail "Format de assembler incorrecto"
                  | f (c :: rest) = c :: f rest
                  | f [] = []
            in
                implode(f(explode assem))
            end
        in
            fn(OPER{assem, dst, src, jump = NONE}) => "\t" ^ speak(assem, dst, src, [])
            | (OPER{assem, dst, src, jump = SOME j}) => "\t" ^ speak(assem, dst, src, j)
            | LABEL{assem, ...} => assem
            | MOVE{assem, dst, src} => "\t" ^ speak(assem, [dst], [src], [])
        end*)
    and safeNth (xs, i) = if i < List.length xs 
                          then List.nth (xs, i) 
                          else raise Fail (
                              "Error: la lista " ^ 
                              (String.concatWith ", " xs) ^
                              " de longitud " ^ Int.toString(List.length xs) ^
                              " no tiene indice " ^ Int.toString(i))
                              
    fun showInstr (OPER {assem, dst, src, jump}) = "OPER{assem = " ^ assem ^ ", dst = " ^ 
            showList dst ^ ", src = " ^ tigerutils.showList src ^ ", jump = " ^ tigerutils.showOptionList jump ^ "}"
      | showInstr (MOVE {assem, dst, src}) = "MOVE{assem = " ^ assem ^ ", dst = " ^ dst ^ ", src = " ^ src ^ "}"
      | showInstr (LABEL {assem, lab}) = "LABEL{assem = " ^ assem ^ ", lab = " ^ lab ^ "}"          
      
    fun src2List (MOVE r) = [#src r]
      | src2List (OPER r) = #src r
    (*  | src2List _ = []*)
      
    fun dst2List (MOVE r) = [#dst r]
      | dst2List (OPER r) = #dst r
(*      | dst2List _ = []*)
end
