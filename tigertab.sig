signature tigertab =
sig

type ('a, 'b) Tabla
exception yaExiste of string
exception noExiste
exception noExisteS of string
val tabNueva : unit -> (''a, 'b) Tabla
val fromTab : (''a, 'b) Tabla -> (''a, 'b) Tabla
val name : 'a -> 'a
val tabEsta : ''a * (''a, 'b) Tabla -> bool
val tabInserta : ''a * 'b * (''a, 'b) Tabla -> (''a, 'b) Tabla
val tabRInserta : ''a * 'b * (''a, 'b) Tabla -> (''a, 'b) Tabla
val tabBusca : ''a * (''a, 'b) Tabla -> 'b option
val tabSaca : ''a * (''a, 'b) Tabla -> 'b
val tabAplica : ('a -> 'b) * (''c, 'a) Tabla -> (''c, 'b) Tabla
val tabAAplica : ('a -> ''c) * ('b -> 'd) * ('a, 'b) Tabla -> (''c, 'd) Tabla
val tabRAAplica : ('a -> ''b) * ('c -> 'd) * ('a, 'c) Tabla -> (''b, 'd) Tabla
val tabInserList : ('a, 'b) Tabla * ('a * 'b) list -> ('a, 'b) Tabla
val tabAList : ('a, 'b) Tabla -> ('a * 'b) list
val tabFiltra : ('b -> bool) * (''a, 'b) Tabla -> (''a, 'b) Tabla
val tabPrimer : ('b -> bool) * ('a, 'b) Tabla -> ('a * 'b)
val tabClaves : ('a, 'b) Tabla -> 'a list
val tabApp : ('b -> unit) -> (''a, 'b) Tabla -> (''a, 'b) Tabla
val inserta : ''a * 'b * (''a, 'b) Tabla ref -> unit
val busca : ''a * (''a, 'b) Tabla * string -> 'b
val fromList : (''a * 'b) list -> (''a, 'b) Tabla
val tab2Func : (string, string) Tabla -> (string -> string)
val printWith : (''a, 'b) Tabla -> (''a -> string) -> ('b -> string) -> unit 
end

