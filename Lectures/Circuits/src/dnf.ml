type expr =
    True
  | False
  | Var of string
  | Not of expr
  | Or of expr * expr
  | And of expr * expr
  | Mono of expr

let is_atom = function
    True
  | False
  | Var _
  | Not (True)
  | Not (False)
  | Not (Var _) -> true
  | _ -> false

let rec (phi: expr -> expr) = function
    Or(x, y) -> Or(phi(x), phi(y))
  | a when is_atom(a) -> Mono(a)
  | Not(Or(x, y)) -> phi(And(Not(x), Not(y)))
  | Not(And(x, y)) -> phi(Or(Not(x), Not(y)))
  | Not(Not(x)) -> phi(x)
  | Mono(m) -> Mono(m)
  | And(a, Mono(m)) when is_atom(a) -> Mono(And(a, m))
  | And(Mono(m), a) when is_atom(a) -> Mono(And(m, a))
  | And(Mono(m0),Mono(m1)) -> Mono(And(m0, m1))
  | And(x, Or(y, z)) -> phi(Or(And(x,y), And(x,z)))
  | And(Or(y, z), x) -> phi(Or(And(y,x), And(z,x)))
  | And(x, y) -> phi(And(phi(x), phi(y)))

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
