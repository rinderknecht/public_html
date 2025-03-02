% solution(Board) if Board is a list of non-attacking queens.
%
solution([]).

solution([.(X,Y) | Others]) :-
  solution(Others),
  member(Y,[1,2,3,4,5,6,7,8]),
  no_attack(.(X,Y), Others).

% no_attack(Q, Qlist) if queen at Q does not attack queens in Qlist
%
no_attack(_, []).

no_attack(.(X,Y), [.(X1,Y1) | Others]) :-
  Y =\= Y1,
  Y1 - Y =\= X1 - X,
  Y1 - Y =\= X - X1,
  no_attack(.(X,Y), Others).

% member(Item, Items) if Item is in Items
%
member(Item, [Item | _]).

member(Item, [_ | Tail]) :- member(Item, Tail).

% solution template
%
template([.(1,Y1),.(2,Y2),.(3,Y3),.(4,Y4),.(5,Y5),.(6,Y6),.(7,Y7),.(8,Y8)]).
