-module(queue1).
-export([enqueue/2,dequeue/1,dequeue_bis/1]).

enqueue(E,Q) -> [E|Q].

% Recursive but not tail-recursive:
dequeue([E])   -> {E,[]};
dequeue([E|Q]) -> {F,R} = dequeue(Q), {F,[E|R]}.

rev(L)       -> rev(L,[]).
rev([],A)    -> A;
rev([H|T],A) -> rev(T,[H|A]).

% Not recursive:
dequeue_bis([H|T]) -> [E|R] = rev([H|T]), {E,rev(R)}.
