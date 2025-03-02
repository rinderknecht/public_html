-module(rev).
-export([rev/1]).
join([],L)     -> L;
join([H|T],L)  -> [H|join(T,L)].

rev([])    -> [];                      % Clause 3
rev([H|T]) -> join(rev(T),[H]).        % Clause 4
