(* [logk format x1 ... xk] passes [format] and the [k] parameters
   [x1 ... xk] to [fprintf stderr], then flushes the standard
   error channel. *)

val log0 : ('a, out_channel, unit) format -> unit
val log1 : ('a -> 'b, out_channel, unit) format -> 'a -> unit
val log2 : ('a -> 'b -> 'c, out_channel, unit) format -> 'a -> 'b -> unit
val log3 : ('a -> 'b -> 'c -> 'd, out_channel, unit) format -> 'a -> 'b -> 'c -> unit
val log4 : ('a -> 'b -> 'c -> 'd -> 'e, out_channel, unit) format -> 'a -> 'b -> 'c -> 'd -> unit

val die0 : ('a, out_channel, unit) format -> 'b
val die1 : ('a -> 'b, out_channel, unit) format -> 'a -> 'c
val die2 : ('a -> 'b -> 'c, out_channel, unit) format -> 'a -> 'b -> 'd
val die3 : ('a -> 'b -> 'c -> 'd, out_channel, unit) format -> 'a -> 'b -> 'c -> 'e
val die4 : ('a -> 'b -> 'c -> 'd -> 'e, out_channel, unit) format -> 'a -> 'b -> 'c -> 'd -> 'f
