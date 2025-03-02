-module(queue1).
-export([enqueue/2,dequeue/1,dequeue1/1,dequeue2/1]).

enqueue(E,Q) -> [E|Q].

% Recursive but not tail-recursive:
%
dequeue([E])   -> {E,[]};
dequeue([E|Q]) -> {F,R} = dequeue(Q), {F,[E|R]}.

% Not recursive:
%
dequeue1([H|T]) -> [E|R] = rev([H|T]), {E,rev(R)}.

rev(L)       -> rev(L,[]).
rev([],A)    -> A;
rev([H|T],A) -> rev(T,[H|A]).

% Tail recursive:
%
dequeue2([H|T]) -> deq_aux1([H],T).

deq_aux1([I|A],   []) -> deq_aux2(I,[],A);
deq_aux1(    A,[I|T]) -> deq_aux1([I|A],T).

deq_aux2(I,A,   []) -> {I,A};
deq_aux2(I,A,[J|L]) -> deq_aux2(I,[J|A],L).
