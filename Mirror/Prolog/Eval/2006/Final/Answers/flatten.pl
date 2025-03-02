flatten([],[]).
flatten([L|Lists],B) :- catenate(L,Flat,B),flatten(Lists,Flat).
