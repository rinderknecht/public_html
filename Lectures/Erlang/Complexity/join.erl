-module(join).
-export([join/2]).
join([],L)    -> L;                        % Clause 1
join([H|T],L) -> [H|join(T,L)].            % Clause 2
