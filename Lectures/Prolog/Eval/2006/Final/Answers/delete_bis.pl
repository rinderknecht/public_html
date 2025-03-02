delete(_,[],[]).
delete(X,[X|A],A).                       
delete(X,[Y|A],[Y,B]) :- delete(X,A,B).  
