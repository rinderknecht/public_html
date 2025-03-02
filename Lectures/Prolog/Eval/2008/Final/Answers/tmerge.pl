tmerge({Left,Right},Sorted) :-
  tmerge(Left,SLeft),
  tmerge(Right,SRight),
  merge(SLeft,SRight,Sorted).
tmerge(empty,[]).
tmerge(Leaf,[Leaf]).

merge(   [],    Q,    Q).
merge(    P,   [],    P).
merge([I|P],[J|Q],[I|R]) :- I < J, merge(P,[J|Q],R).
merge(    P,[J|Q],[J|R]) :- merge(P,Q,R).
