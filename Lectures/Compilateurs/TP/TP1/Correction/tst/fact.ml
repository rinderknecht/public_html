let omega = fun f -> f f in
  let fixy = fun f -> omega (fun g -> f (g g)) in
  let pre_fact = fun f -> fun n -> ifz n then 1 else n * f (n-1) in
  let fact = fix (pre_fact)
in fact 5


