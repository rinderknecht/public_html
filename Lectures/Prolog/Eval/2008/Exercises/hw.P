% SWI Prolog programs
% Christian Rinderknecht
% Friday, 9th September 2011

% Note: the abbreviation `iff' stands for `if and only if'.

%=====================================================================
% `mem(I,L)' is provable iff the item `X' belongs to the list `L'.
%
mem(X,[X|_]).
mem(X,[_|L]) :- mem(X,L).

% membis/2 is a less algorithmical definition of the membership
% predicate than mem/2. It is more declarative and abstract, for it
% states that `X' is in `L' if `L' is made of a list followed by `X',
% followed in turn by another list.
%
mem_bis(X,L) :- cat(_,[X|_],L).

%====================================================================
% `occ(X,L,M)' is provable iff the item `X' occurs `M' times in the
% list `L'. In order to have `M' work in both directions, that is,
% both as input and output, we cannot use the built-in predicate
% `is/2' (arithmetic), but, instead, we must load the clpfd library
% (Constraint Logic Programming over Finite Domains) and use its
% `#=/2' predicate in lieu of `is/2'. Furthermore, we use the built-in
% predicates `not/1' and `=/2' to express the goal `not unifiable'
% (otherwise occ(5,[5],0) would be provable). `Negation as failure'
% may lead to unsound deductions, as the following query:
%
% ?- occ(X,[a,b,b],2).
% false.
%
% The reason is that a free variable can always be bound:
%
% ?- not(X=a).
% false.
%
% Such binding of a free variable by `=/2' is called `floundering',
% and SWI-Prolog does not warn about it. (Statically undecidable,
% anyway.) Negation of ground goals is sound. Consider the following
% example taken from the SWI-Prolog manual:
%
%     bachelor(P) :- male(P), not(married(P)).
%     male(henry).
%     male(tom).
%     married(tom).
%
% Then
%
%     ?- bachelor(henry).
%     true
%     ?- bachelor(tom).
%     false
%     ?- bachelor(Who).
%     Who= henry ;
%     false
%     ?- not(married(Who)).
%     false. 
%
:- use_module(library(clpfd)).

occ(_,[],0).
occ(X,[X|P],M) :- occ(X,P,N), M #= N + 1.
occ(X,[Y|P],N) :- not(X=Y), occ(X,P,N).

%=====================================================================
% `last(L,X)' is provable iff `X' is the last item in the list `L'.
%
last([X],X).
last([_|L],X) :- last(L,X).

%=====================================================================
% `pen(L,I)' is provable iff item `I' is the penultimate item in L.
%
pen(I,[I,_]).
pen(I,[_|L]) :- pen(I,L).

%=====================================================================
% `cat(L,P,Q)' is provable iff `Q' is the list whose first items are
% those of `L' in the same order, and the following are those of `P'
% in the same order.
%
cat(   [],P,    P).
cat([I|L],P,[I|Q]) :- cat(L,P,Q).

%=====================================================================
% `rcat(L,P,Q)' is provable iff `Q' is the list whose first items
% are the items of `L' reversed, followed by the items of `P' in the
% same order.
%
rcat(   [],P,P). 
rcat([I|L],P,Q) :- rcat(L,[I|P],Q).

%=====================================================================
% `rev(P,Q)' is provable iff `Q' is the list containing all the items
% of `P' in reverse order.
%
% The number of inferences to answer the query "?- rev(-P,+Q)."
% is |P|+1.
%
rev(P,Q) :- rcat(P,[],Q).

% srev/2 is version slower than rev/2.
%
srev([],[]).
srev([I|P],Q) :- srev(P,R), cat(R,[I],Q).
