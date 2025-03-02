type atom =
[   `True
  | `False
  | `Var of string
]

type 'a expr =
[   `True
  | `False
  | `Var of string
  | `Not of 'a
  | `Or  of 'a * 'a
  | `And of 'a * 'a
]

type 'a ext_expr =
[   `True
  | `False
  | `Var  of string
  | `Not  of 'a
  | `Or   of 'a * 'a
  | `And  of 'a * 'a
  | `Mono of [< atom ] (* | `Not of [> #atom] | `And of 'a * 'a]*)
]

(*
let rec phi: ('a ext_expr as 'a) -> 'a = function
    `Or(x,y) -> `Or(phi(x), phi(y))
  | (#atom | `Not #atom) as a -> `Mono(a)
  | `Not(`Or(x, y)) -> phi(`And(`Not(x),`Not(y)))
  | `Not(`And(x, y)) -> phi(`Or(`Not(x),`Not(y)))
  | `Not(`Not(x)) -> phi(x)
  | `Mono(`And _) as m -> m
  | `And(#atom | `Not #atom as a,`Mono(m)) -> `Mono(`And(a, m))
  | `And(`Mono(m), (#atom | `Not #atom as a)) -> `Mono(`And(m, a))
  | `And(`Mono(m0), `Mono(m1)) -> `Mono(`And(m0,m1))
  | `And(x,`Or(y,z)) -> phi(`Or(`And(x,y),`And(x,z)))
  | `And(`Or(y,z),x) -> phi(`Or(`And(y,x),`And(z,x)))
  | `And(x,y) -> phi(`And(phi(x), phi(y)))
*)

(*
let rec (phi_fp: ('a expr as 'a) -> 'a) = fun expr -> phi phi_fp expr
*)

(*
let rec (clean: expr -> expr) = function
    Or(x, y) -> Or(clean(x), clean(y))
  | Mono(m) -> m
  | _ -> assert(false)

let dnf (x: expr) = clean(phi(x))

let e1 = And (Or(Not(And(Var "a", Var "b")), 
                 And(Not(Not(Var "b")), And(Var "a", Var "c"))
                ),
              Not(And(True, Var "b"))
             )
*)
