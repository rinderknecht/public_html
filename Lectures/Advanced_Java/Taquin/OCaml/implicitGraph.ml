(* This module defines an implicit representation for graphs where
   edges have integer costs, there is a distinguished start node, and
   there is a set of distinguished goal nodes. It is also assumed that
   some geometrical knowledge of the graph allows safely estimating
   the cost of shortest paths to goal nodes without actually looking
   for these paths. If no such knowledge is in fact available,
   [estimate] should be the constant zero function. *)

type cost = int

module type S = sig

  (* Graph nodes. *)
  type node

  (* Edge labels. *)
  type label

  (* The graph's start node. *)
  val start: node

  (* Whether a node is a goal node. *)
  val is_goal: node -> bool

  (* [successors n f] presents each of [n]'s successors, in
     an arbitrary order, to [f], together with the cost of
     the edge that was followed. *)
  val successors: node -> (label -> cost -> node -> unit) -> unit

  (* An estimate of the cost of the shortest path from the
     supplied node to some goal node. For algorithms such as
     A* and IDA* to find shortest paths, this estimate must
     be a correct under-approximation of the actual cost. *)
  val estimate: node -> cost

  (* [successors n cost f] presents each of [n]'s successors, in
     an arbitrary order, to [f], together with the cost of
     the edge that was followed, and together with an estimate
     for that successor. [cost] must be [estimate n]. In other
     words, cost estimates are performed incrementally. *)
  val successors_estimate: node -> cost -> (label -> cost -> node -> cost -> unit) -> unit

end

