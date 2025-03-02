module Make (G : ImplicitGraph.S) : sig

  (* This function produces a shortest path from the start
     node to some goal node. It raises [Not_found] if no
     such path exists. *)
  val path: unit -> G.node list

end
