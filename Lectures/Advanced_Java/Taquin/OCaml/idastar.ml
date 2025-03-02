(* This module implements IDA* search, following the pseudo-code
   in Reinefeld and Marsland (1993).

   The search is parameterized by a finite state machine that
   prunes paths, viewed as words over labels. Of course, the
   correctness of the search depends upon the correctness of
   the finite state machine: whenever a path is pruned, another
   path to some goal state, which is at least as short, must
   exist (and not be pruned). *)

open Log

module Make
    (G : ImplicitGraph.S)
    (M : Fsm.S with type symbol = G.label)
 = struct

  let infinity = max_int

  exception Solved of G.node list

  (* Depth-first search. *)

  let expanded =
    ref 0

   let overall_expanded =
     ref 0

  let rec dfs node estimate state bound =
    if G.is_goal node then
      raise (Solved [ node ])
    else
      try
	let new_bound = ref infinity in
	incr expanded;
	incr overall_expanded;
	G.successors_estimate node estimate (fun label cost son estimate ->
	  match M.transition state label with
	  | None ->
	      (* This path is pruned. *)
	      ()
	  | Some state ->
	      (* This path is accepted. *)
	      let bound = bound - cost in
	      let better_estimate = 
		if estimate <= bound then
		  dfs son estimate state bound
		else
		  estimate
	      in
	      if better_estimate <> infinity then
		let better_bound = cost + better_estimate in
		if better_bound < !new_bound then
		  new_bound := better_bound
	);
	!new_bound
      with Solved path ->
	raise (Solved (node :: path))

  (* Iterative deepening. *)

  let rec explore bound =

    log1 "Nouvelle phase d'exploration à la profondeur %d...\n" bound;
    expanded := 0;

    let bound = dfs G.start (G.estimate G.start) M.start bound in

    log1 "Phase terminée, exploré %d noeuds.\n" !expanded;

    if bound = infinity then
      raise Not_found
    else
      explore bound

  (* Main function. *)

  let path () =
    try
      explore (G.estimate G.start)
    with
    | Solved path ->
	log1 "En tout, exploré %d noeuds.\n" !overall_expanded;
	path
    | Not_found ->
	log1 "En tout, exploré %d noeuds.\n" !overall_expanded;
	raise Not_found

end
