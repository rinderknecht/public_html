catenate([],B,B).
catenate([X|A],B,[X|C]) :- catenate(A,B,C).
