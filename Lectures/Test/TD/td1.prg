pragma title("Tri de Shell");

generic
  type ELEMENT is private;   -- type des éléments à trier
  type VECTEUR_D_ELEMENTS is array (NATURAL range <>) of ELEMENT;
  with function "<" (elt1,elt2: ELEMENT) return BOOLEAN is <>;

[...]

procedure TRI_SHELL (VECTEUR: in out VECTEUR_D_ELEMENTS) is
  incr : natural := VECTEUR'length; -- suite des incréments dégressifs
  inf : constant natural := VECTEUR'first;
  L_element : ELEMENT;
  K : integer;

  begin
    while incr > 1 loop
      if incr < 5 then incr := 1; 
                  else incr := (5*incr-1)/11; 
      endif;

      for L in inf+incr..VECTEUR'last loop
        L_element := VECTEUR (L);
        K := L - incr;
        while K >= inf loop
          if L_element < Vecteur (k) 
          then VECTEUR (k+incr) := VECTEUR (k);
               k := k - incr;
          else
               exit;
          endif;
        end loop;
        
        VECTEUR (k+incr) := L_element;
      end loop;
    end loop;
  end TRI_SHELL

