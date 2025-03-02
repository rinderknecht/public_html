diff(var(X),var(X),num(1)).                         % 1
diff(num(_),var(_),num(0)).                         % 2
diff(var(Y),var(X),num(0)) :- \+(X=Y).              % 3
diff(minus(F),X,minus(DF)) :- diff(F,X,DF).         % 4
diff(plus(F,G),X,plus(DF,DG)) :-                    % 5
  diff(F,X,DF), diff(G,X,DG).
diff(minus(F,G),X,minus(DF,DG)) :-                  % 6
  diff(F,X,DF), diff(G,X,DG).
diff(times(F,G),X,plus(times(F,DG),times(G,DF))) :- % 7
  diff(F,X,DF), diff(G,X,DG).
