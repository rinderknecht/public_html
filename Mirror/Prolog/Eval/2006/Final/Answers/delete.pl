delete(X,[X|A],A).                        % Rule 2
delete(X,[Y|A],[Y,B]) :- delete(X,A,B).   % Rule 4
