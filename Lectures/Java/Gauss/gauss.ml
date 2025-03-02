exception Repeated_variable of string
exception Too_many_variables
exception Too_many_equations

(* 
  Function call [check (s)] checks for consistence of system
  [s]. It may raise [Repeated_variable], [Too_many_variables] or
  [Too_many_equations]. It returns a pair [(dim, vars)] where [dim] is
  the dimension of the system, and [vars] is the sorted list of
  the variables occurring in the system.
*)
let check (sys : Ast.system) : int * (string list) = 
  let rec merge_vars l1 l2 =
    match (l1,l2) with
      [], _ -> l2
    | _, [] -> l1
    | x1::l1', x2::l2' ->
        if x1 = x2
        then x1::(merge_vars l1' l2')
        else if x1 < x2
             then x1::(merge_vars l1' l2)
             else x2::(merge_vars l1 l2') in
  (*
    Function call [check_eq (e)] returns the list of the
    variables occurring in equation [e]. If some variable [v] is
    repeated, exception [Repeated_variable (v)] is raised.
  *)
  let check_eq (terms, const) : string list =
    let folder acc (var, coef) =
      if   List.mem var acc
      then raise (Repeated_variable var)
      else var::acc
    in List.fold_left folder [] terms in
  (* 
    Function call [check_sub_sys (acc) (eq)] checks equation [eq], get
    its variable names and merge this list to [acc].
  *)
  let check_sub_sys acc eq =
    let sorted_eq_vars = List.sort compare (check_eq (eq))
    in merge_vars (acc) (sorted_eq_vars) in
  (*
    The variable [sys_vars] is the ordered list of variable names
    present in the system [sys].
  *)
  let sys_vars : string list = 
    List.fold_left (check_sub_sys) [] sys in

  (*
    The variable [var_num] is the number of variables in the system
    [sys].
  *)
  let var_num = List.length (sys_vars) in
  (*
    The variable [eq_num] is the number of equations in the system
    [sys].
  *)
  let eq_num = List.length (sys)
in if var_num = eq_num 
   then (var_num, sys_vars)
   else if var_num < eq_num
        then raise Too_many_equations
        else raise Too_many_variables
  

let print_term (var, coef) =
  if coef < 0
  then 
    if coef = -1
    then (print_string " - "; print_string var)
    else (print_string " - "; print_int (abs (coef)); print_string var)
  else 
    if coef = 1
    then (print_string " + "; print_string var)
    else (print_string " + "; print_int (coef); print_string var)

let print_eq (terms, const) = 
  let rec print_terms = function
     [] -> print_string " = "; print_int const
  | term::others ->
      print_term term; print_terms others
in match terms with
     [] -> assert false
   | (var,coef)::others -> 
       begin
        (if coef = -1
         then (print_string "-"; print_string var)
         else 
           if coef = 1
           then print_string var
           else (print_int (coef); print_string var)
         );
         print_terms others
       end

let print_sys title sys =
  begin
    print_string title;
    print_newline ();
    List.iter (fun x -> print_eq x; print_newline ()) sys
  end
  
let get_sys filename : Ast.system =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let sys = Parser.system Lexer.token lexbuf
in close_in ic; sys


let sys_opt = ref (None : string option)
let sys_hdl (filename) = sys_opt := Some (filename)
let sys_doc = "<file>   Input file containing the linear system."

let usage = "Usage: " ^ Sys.argv.(0) ^ " <file>"
let anonymous filename = sys_opt := Some (filename)

(*
let () = Lexer.trace filename
*)

let () = Arg.parse [] anonymous usage

let sys = 
  match !sys_opt with
    None -> prerr_endline usage; exit (-1)
  | Some filename -> get_sys filename

let () = print_sys "Initial system" (sys)

let () = 
  let (dim, vars) = check (sys)
in begin
     print_string "Variables: ";
     List.iter (fun x -> print_string x; print_string " ") vars;
     print_newline ()
   end

(*
  Function call [normalise sys] returns a system equivalent to
  [sys] where the terms in the equations are sorted according to the
  variable names and the zero coefficient appear (i.e. the
  representation is no more sparse). The system [sys] must be conform
  according to [check].
*)
let normalise (sys : Ast.system) : Ast.system =
  let (dim, vars) = check (sys) in
  let rec normalise_term (prev_terms, remaining_vars) term =
    match (remaining_vars, term) with
      (v::other_vars, (var, coef)) -> 
        if v = var
        then (term::prev_terms, other_vars)
        else normalise_term ((v,0)::prev_terms, other_vars) term
    | _ -> assert false in
  let normalise_eq (terms, const) acc =
    let sorted_terms = List.sort compare terms in
    let (norm_terms, _) = 
      List.fold_left normalise_term ([],vars) sorted_terms
    in (List.rev norm_terms, const)::acc
in List.fold_right normalise_eq sys []

let normalised_sys = normalise (sys)

let () = print_sys "Normalised system" (normalised_sys)

let store (sys) =
  let n = List.length (sys) in
  let mat = Array.make_matrix n (n+1) 0 in
  let rec store_terms (i,j) = function
    [] -> ()
  | (var,coef)::other_terms ->
      begin
        mat.(i).(j) <- coef;
        store_terms (i,j+1) other_terms
      end in
  let store_equation (i) (terms, const) = 
    begin
      store_terms (i,0) terms;
      mat.(i).(n) <- const
    end in
  let rec store_system (i) = function
    [] -> ()
  | eq::other_eq -> 
      begin
        store_equation (i) (eq);
        store_system (i+1) other_eq
      end
in store_system 0 sys; mat

let v_sys = store (normalised_sys)

let print_v_sys title v_sys =
  let dim = Array.length (v_sys) 
in begin
     print_string title;
     print_newline ();
     for i = 0 to dim-1 do
       for j = 0 to dim-1 do
         print_int v_sys.(i).(j);
         print_string " "
       done;
       print_string "| ";
       print_int v_sys.(i).(dim);
       print_newline ()
     done
   end
  
let () = print_v_sys "Vectorized system" v_sys

(* Printing raw operations *)

let print_raw i = 
  begin print_string "L"; print_int i end

let print_raw_swap i0 i1 =
  begin
    print_string "Swapping: ";
    print_raw i0; print_string " <-> "; print_raw i1
  end

let print_raw_simpl i d =
  begin
    print_string "Simplifying: ";
    print_raw i; print_string " <- "; 
    print_raw i; print_string "/"; print_int d
  end

let print_pivot i j v_sys =
  begin
    print_string "Pivot is coefficient (";
    print_int i;
    print_string ",";
    print_int j;
    print_string ") = ";
    print_int v_sys.(i).(j);
    print_newline ()
  end

(* 
  L_{current_raw} <- p L_{pivot_raw} - q L_{current_raw} 
*)
let print_mult coef raw =
  if coef = 1
  then print_raw raw
  else if coef = -1
       then (print_string "-"; print_raw raw)
       else (print_int coef; print_string " x "; print_raw raw)

let print_combination (p,pivot_raw) (q,current_raw) =
  let op = if q > 0 then " - " else " + "
in begin
     print_string "Combining: ";
     print_raw current_raw; 
     print_string " <- ";
     print_mult p pivot_raw;
     print_string op;
     print_mult (abs q) current_raw
   end

(* GCD and simplifications *)

let gcd n m =
  let rec gcd_aux n m =
    if   n > m 
    then gcd_aux  m n
    else if   n = 0 
         then m
         else gcd_aux (m mod n) n
in gcd_aux (abs(n)) (abs(m))

let scm n m = (n*m) / (gcd n m)


let raw_gcd = Array.fold_left gcd 0

let sys_gcd v_sys =
  let simplify_raw acc raw =
    (Array.fold_left gcd 0 raw)::acc
in List.rev (Array.fold_left simplify_raw [] v_sys)

let raw_simplify i raw : unit =
  let d = raw_gcd raw
in if   d <> 1 && d <> 0
   then begin
          for i = 0 to Array.length(raw)-1 do
            raw.(i) <- raw.(i) / d;
          done;
          print_raw_simpl i d;
          print_newline ()
        end

let simplify = Array.iteri raw_simplify

let () = simplify (v_sys)
let () = print_v_sys "Simplified vectorized system" v_sys


(* Solving the system *)

exception No_pivot of int

let solve v_sys =
  let dim = Array.length (v_sys) in

  let swap_raws i0 i1 j =
    begin
      for k = j to dim do
        let tmp = v_sys.(i0).(k)
        in begin
             v_sys.(i0).(k) <- v_sys.(i1).(k);
             v_sys.(i1).(k) <- tmp;
           end
      done;
      print_raw_swap i0 i1;
      print_newline ()
    end in

  let find_pivot j =
    let f (i,acc) raw =
      if   i < j || raw.(j) = 0
      then (i+1,acc)
      else match acc with
             None -> (i+1, Some (i))
           | pivot -> (i+1, pivot)
    in match Array.fold_left f (0,None) v_sys with
         (_, Some p) -> p
       | _ -> raise (No_pivot (j)) in

(*  L_{current_raw} <- p L_{pivot_raw} - q L_{current_raw} *)
  let combine (p,pivot_raw) (q,current_raw) =
    for j = 0 to dim do
      v_sys.(current_raw).(j) <-
        p*v_sys.(pivot_raw).(j) - q*v_sys.(current_raw).(j)
    done in
  
  let gen_combine i j =
(*    prerr_endline ("gen_combine (" ^ string_of_int i 
                   ^ "," ^ string_of_int j ^ ")"); 
*)    if v_sys.(i).(j) <> 0
    then 
      let smallest_cm = scm v_sys.(j).(j) v_sys.(i).(j) in
        let coef_pivot = smallest_cm / v_sys.(j).(j) in
        let coef_current = smallest_cm / v_sys.(i).(j)
      in begin
           combine (coef_pivot,j) (coef_current,i);
           print_combination (coef_pivot,j) (coef_current,i);
           print_newline ();
           print_v_sys "Current system" v_sys
         end

in begin
     for j = 0 to dim-1 do
       let pivot_raw = find_pivot j in
         let () = print_pivot pivot_raw j v_sys
       in begin
            (if   pivot_raw <> j
             then begin
                    swap_raws j pivot_raw j;
                    print_v_sys "New system" v_sys
                  end
            );
            for i = 0 to j-1 do
              gen_combine i j
            done;
            for i = j+1 to dim-1 do
              gen_combine i j
            done;
            simplify v_sys;
            print_v_sys "Simplified system" v_sys
          end
     done;
     for i = 0 to dim-1 do
       if v_sys.(i).(i) < 0
       then begin
              v_sys.(i).(i) <- -v_sys.(i).(i);
              v_sys.(i).(dim) <- -v_sys.(i).(dim)
            end
     done
   end


let () = solve v_sys

let () = print_v_sys "Final system" v_sys
