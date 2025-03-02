type 'a file = {mutable avant : 'a list; mutable arrière: 'a list};;

let nouv_file () = {avant=[];arrière=[]};;
let est_vide f = (f.avant = []) && (f.arrière = []);;
let longueur f = List.length (f.avant) + List.length (f.arrière);;
let ajoute f x = f.arrière <- x :: f.arrière;;

let retire f = match f.avant with
  x::suite -> f.avant <- suite; x
| []       -> begin match List.rev (f.arrière) with
                x::suite -> f.avant <- suite; f.arrière <- []; x
              | [] -> failwith "file vide!"
              end
;;

let premier f = 
  let x = retire f
in f.avant <- x :: f.avant; x
;;

let h2 = nouv_file();;
ajoute h2 1;;
let h3 = nouv_file();;
ajoute h3 1;;
let h5 = nouv_file();;
ajoute h5 1;;

let suivant () =
  let x2 = premier h2 
  and x3 = premier h3
  and x5 = premier h5 in
  let x = min (min x2 x3) x5 
in if x = x2 then (let _ = retire h2 in ());
   if x = x3 then (let _ = retire h3 in ());
   if x = x5 then (let _ = retire h5 in ());
   ajoute h5 (5*x);
   if x mod 5 > 0 
   then begin
          ajoute h3 (3*x);
          if x mod 3 > 0 then ajoute h2 (2*x)
        end;
   x
;;

for n = 1 to 100 do
  print_string "H("; print_int n; print_string ")=";
  print_int (suivant ());
  print_newline ()
done
;;
  
