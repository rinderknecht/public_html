member(X, [X | _]).
member(X, [_ | Tail]) :- member(X, Tail).
