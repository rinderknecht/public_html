-module(queue2).
-export([enqueue/2,dequeue/1,dequeue1/1]).

enqueue(E,{S,T}) -> {[E|S],T}.

% Not tail-recursive:
%
dequeue({S,[E|T]})  -> {E,{S,T}};
dequeue({[H|S],[]}) -> [E|R] = rev([H|S]), {E,{[],R}}.

rev(L)       -> rev(L,[]).
rev([],A)    -> A;
rev([H|T],A) -> rev(T,[H|A]).

% Tail-recursive:
%
dequeue1({S,[E|T]})  -> {E,{S,T}};
dequeue1({[H|S],[]}) -> deq_aux1([H],S).

deq_aux1([I|A],   []) -> {I,{[],A}};
deq_aux1(    A,[I|S]) -> deq_aux1([I|A],S).
