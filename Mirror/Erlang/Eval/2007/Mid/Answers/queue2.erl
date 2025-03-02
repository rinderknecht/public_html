-module(queue2).
-export([enqueue/2,dequeue/1]).

rev(L)       -> rev(L,[]).
rev([],A)    -> A;
rev([H|T],A) -> rev(T,[H|A]).
    
enqueue(E,{S,T}) -> {[E|S],T}.

dequeue({S,[E|T]})  -> {E,{S,T}};
dequeue({[H|S],[]}) -> [E|R] = rev([H|S]), {E,{[],R}}.

    
