-module(rev_bis).
-export([rev_bis/1]).
rev_bis(L)        -> rev_join(L,[]).       % Clause 5

rev_join([],A)    -> A;                    % Clause 6
rev_join([H|T],A) -> rev_join(T,[H|A]).    % Clause 7
