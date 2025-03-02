module type S = sig

  (* The type of states. *)
  type state

  (* The type of symbols. *)
  type symbol

  (* The initial state. *)
  val start: state

  (* The transition function. It either moves to a new state
     or blocks. *)
  val transition: state -> symbol -> state option

end

