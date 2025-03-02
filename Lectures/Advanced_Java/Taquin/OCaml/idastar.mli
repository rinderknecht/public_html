module Make
    (G : ImplicitGraph.S)
    (M : Fsm.S with type symbol = G.label)
 : sig

  (* This function produces a shortest path from the start
     node to some goal node. It raises [Not_found] if no
     such path exists. *)
  val path: unit -> G.node list

end
