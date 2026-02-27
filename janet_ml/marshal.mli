val marshal
  :  ?unsafe:bool
  -> ?no_cycles:bool
  -> ?max_size:int
  -> env:Janet_table.t option
  -> Janet.t
  -> string

val marshal_symbol
  :  ?unsafe:bool
  -> ?no_cycles:bool
  -> ?max_size:int
  -> env:Janet_table.t
  -> string
  -> string

val unmarshal
  :  ?unsafe:bool
  -> ?no_cycles:bool
  -> ?env:Janet_table.t option
  -> string
  -> Janet.t
