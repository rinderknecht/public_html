% Christian Rinderknecht
% 7 October 2009
%
% Note: The length of a list `L' is denoted by `|L|'.

%=====================================================================
% `len(L,Len)' is provable iff `Len' is the number of items in the
% list `L'.

% The number of inferences of "?- len1(L,N)." is |L|.
%
len1(   [],0).
len1([_|L],N) :- len1(L,M), N is M+1.

% Tail recursive version
%
% The number of inferences is |L|+1.
%
len(L,N) :- len__(0,L,N).

len__(A,[],A).
len__(A,[_|L],N) :- B is A+1, len__(B,L,N).

%=====================================================================
% In `rm_all(L,I,P)', `P' is the list containing all the items of
% list `L' in the same order, except all items matching `I'.

% The number of inferences to answer the query
% "?- rm_all(-L,-I,+P)." is |L|.
%
rm_all(   [],_,   []).
rm_all([I|L],I,    P) :- rm_all(L,I,P), !.
rm_all([J|L],I,[J|P]) :- rm_all(L,I,P).

%=====================================================================
% `rm_fst(L,I,P)' is provable iff `P' is the list `L' without its
% first occurrence of item `I'. If `I' is missing from `L', then P=L.
%
% The maximum number of inferences to answer the query
% "?- rm_fst(-L,-I,+P)." happens when the second clause is
% never used, which means that `I' is not in `L'. In this case, the
% number of inferences is |L|.
%
% Note: `P' shares the longest possible suffix with `L'
%
rm_fst(   [],_,   []).
rm_fst([I|L],I,    L) :- !.
rm_fst([J|L],I,[J|P]) :- rm_fst(L,I,P).

%=====================================================================
% In `rm_lst(L,I,P)', `P' is the list containing all the items of
% list `L', except the last occurrence of `I'.

% The worst case is when `I' is not in `L'. Under this circumstance,
% the number of inferences to compute `rm_lst2(L,I,M)' is 3*|L|+3:
%       1 inference by clause 1 of rm_lst1/3
%   |L|+1 inferences to compute `rev(L,P)'
% + |L|   inferences to compute `rm_fst(P,I,Q)' 
%           since |P| = |L|, where `rev(L,P)', and `I' is not in `L'
% + |L|+1 inferences to compute the body of the clause when
%           `rm_fst(P,I,Q)' is already computed and,
%           due to the worst case, |Q| = |L|.
%
rm_lst2(L,I,M) :- rev(L,P), rm_fst(P,I,Q), rev(Q,M).

% For a given length of `L', there is a worst case configuration of
% `L': when `I' is not in `L'. Indeed, under this assumption, the
% number of inferences of `rev(A,P)' in the clause 1 of rm_lst1__/4
% is maximum and clause 2 of rm_lst1__/4 is never used. More
% precisely, the number of inferences of `rm_lst1(L,I,P)' when `I'
% is not in `L' is 3*|L|+6:
%       1 inference  by clause 1 of rm_lst1/3
% + |L|+2 inferences for goal rev(L,Q) in the body
%           of clause 1 of rm_lst1/3
% + |L|   inferences by clause 3 of rm_lst1__/4 
%           since |P| = |L|, where rev(L,P)
% +     1 inference  by clause 1 of rm_lst1/4
% + |L|+2 inferences to compute `rev(A,P)' in the body of clause 1 of
%           rm_lst1__/4, since |A| = |L|.
%
rm_lst1(L,I,P) :- rev(L,Q), rm_lst1__([],Q,I,P).

rm_lst1__(A,   [],_,P) :- rev(A,P).
rm_lst1__(A,[I|L],I,P) :- rcat(L,A,P).
rm_lst1__(A,[J|L],I,P) :- rm_lst1__([J|A],L,I,P).

%=====================================================================
% `rep_fst(P,Q)' is provable iff the list Q contains the first item of
% list P, repeated as many times as there are items in P.
%
% The delay is 1+(n-1)=n, even if n=0.
%
rep_fst(   [],   []).
rep_fst([I|P],[I|Q]) :- repeat(I,P,Q).
repeat(_,   [],   []).
repeat(I,[_|P],[I|Q]) :- repeat(I,P,Q).

% This version uses temporarily more memory but is shorter.
% The delay is also n.
%
rep_fst1(     [],[]).
rep_fst1(    [I],[I]).
rep_fst1([I,_|P],[I|Q]) :- rep_fst1([I|P],Q).






% When ?- mem(-X,+L). the only substitution is L = [X|_] and the
% number of inferences is 0 (by the first clause of mem/2).
%
% When ?- mem(-X,-L). the worst case is when the item `X' occurs
% at the end of list `L'. Under this assumption, the number of
% inferences is |L|.
%
% When ?- mem(+X,-L). the binding for `X' will be the first item of
% `L', so the number of inferences is 0.
%


%=====================================================================
% `inter(L,P,Q)' is provable iff `Q' contains the elements of `L'
% which are in `P', in the order they occur in `L'.
%
% The maximum number of inferences for answering the query 
% "?- inter(-L,-P,+Q)." is when the number of inferences of
% "?- mem(-I,-P)." is maximum. This happens either when mem/1 fails,
% that is, when I is not in P, or when `I' is the last item in `P'. In
% both cases, the number of inferences for mem/1 is |P|. Then either
% the last clause of inter/3 is tried (and `I' is subsequently
% ignored) or the following goal "inter(L,P,Q)" is queried. In any
% case, the same goal "inter(L,P,Q)" is sought. In total, the number
% of inferences is |L|*|P|.
%
inter(   [],_,   []).
inter([I|L],P,[I|Q]) :- mem(I,P), inter(L,P,Q).
inter([_|L],P,    Q) :- inter(L,P,Q).

% Finding all the sublists of `L'.
%
sublists(L,R) :- findall(S,inter(L,L,S),R).

%=====================================================================
% `drop(L,N,Q)' is provable iff `Q' is the list made of the items of
% list `L' in the same order except the items occurring every `N'
% positions, the first item being counted as occurring at position
% 1.
%
% The number of inferences is |L|+2:
%       1 inference  by clause 1 of drop/3
% + |L|   inferences by clause 3 and 4 of drop__/4
% +     1 inference  by clause 1 of drop__/4
%
drop(L,N,Q) :- N >= 1, drop__(1,L,N,Q).

drop__(_,   [],_,   []).
drop__(N,[_|L],N,    Q) :- drop__(1,L,N,Q), !.
drop__(A,[I|L],N,[I|Q]) :- B is A+1, drop__(B,L,N,Q).

%=====================================================================
% `split(L,N,P,S)' is provable iff `cat(P,S,L)' is provable and
% |P|=N with N >= 0.
%
% The number of inferences is N+1:
%
%   1 inference  by clause 1 of split/4
% + N inferences by clause 2 of split__/4
%
% Note: `S' is shared within `L'.
%
split(L,N,P,S) :- N >= 0, split__(L,N,P,S).

split__(    L,0,   [],L).
split__([I|L],N,[I|P],S) :- M is N-1, split__(L,M,P,S).

%=====================================================================
% `rotate(L,N,M)' is provable iff list `M' contains the items of `L'
% such that the |L|-N last items of `L' come first (in the original
% order), followed by the N first items of `L' (in the original
% order).
%
% The number of inferences for either clause 1 or 2 of rotate/3 is
%       1 for the clause itself
% + |L|+1 for len(L,Len)
% that is: |L|+2.
%
% The number of inferences for clause 2 of rotate__/3 is
% M+1 = |L1|+1 for split(L,M,L1,L2)
%       |L2|   for cat(L2,L1,P)
%            1 for the clause itself
% that is: |L1|+|L2|+2 = |L|+2
%
% A total of 2*|L|+4 inferences.
%
rotate(L,N,P) :-
  N < 0, 
  len(L,Len),
  M is Len + N mod Len,
  rotate__(L,M,P).

rotate(L,N,P) :-
  len(L,Len),
  M is N mod Len,
  rotate__(L,M,P).

rotate__(L,0,L) :- !.
rotate__(L,M,P) :- split(L,M,L1,L2), cat(L2,L1,P).

%=====================================================================
% `flatten(L,P)' is provable iff `P' is a list containing the items of
% list `L' which are not lists themselves, in the original order (if
% an item `I' of `L' is a list, then its items come before the items
% after `I' in `L').
%
% The number of inferences is 
%   the number of empty lists (by clause 2)
% + the length of the result (by clause 5)
% + the sum of the lengths of the sublists (by clauses 3 and 4).
%
% The reason for the last summand is that clauses 3 and 4 decrement the
% length of the sublist ([I] desappears and [I|Q] becomes Q).
%
% For example, the number of steps to compute
% flatten([[[[[q,a,[0]],[]]]],e,3,[5]],P) is
% 1 + 6 + (1+3+2+1+1+1) = 16
%
flatten(       [],   []).
flatten(   [[]|L],    P) :- flatten(L,P),!.
flatten(  [[I]|L],    P) :- flatten([I|L],P),!.   %% Can be omitted
flatten([[I|Q]|L],    P) :- flatten([I,Q|L],P),!.
flatten(    [N|L],[N|P]) :- flatten(L,P).

%=====================================================================
% `compress(L,P)' is provable iff `P' is the list containing the items
% of list `L' in the same order but without repeating consecutive
% items.
%
% The number of inferences is |L|+1:
%
%     1 inference for clause 1 of compress/2
% + |L| inferences for clause 2 or 3 of compress/2
%
compress(         [],   []).
compress([I|P=[I|_]],    Q) :- compress(P,Q).
compress(      [I|P],[I|Q]) :- compress(P,Q).

%=====================================================================
% `pack(P,Q)' is provable iff `Q' is the list of the lists of repeated
% items of `P', in the same original order.

% Another version
%
packing([],[]).
packing([I|P],R) :- packing(P,[I],R).

packing(   [],    Q,  [Q]).
packing([I|P],[I|Q],    R) :- packing(P,[I,I|Q],R).
packing([J|P],    Q,[Q|R]) :- packing(P,[J],R).


% The maximum number of inferences to prove pack(L,P) occurs when none
% of the items in `L' are consecutively repeated. This way, clause
% pack/3 is used for each item in `L'. (Otherwise, if there are
% repeated items, they are processed two-by-two, so the number of
% inferences by clause 2 is halved.) The maximum number is then |L|.
%
%
pack1([],[]).
pack1([I,I|L],T) :- pack1(L,Q), pack__(I,Q,T),!.
pack1([I|L],[[I]|T]) :- pack1(L,T).

pack__(I,[[I|P]|R],[[I,I,I|P]|R]).
pack__(I,Q,[[I,I]|Q]).

% Another version
%
pack([],[]).
pack([I|P],R) :- pack(P,[I],R).

pack(   [],Q,        [Q]).
pack([I|P],Q=[I|_],    R) :- pack(P,[I|Q],R).
pack([J|P],Q,      [Q|R]) :- pack(P,[J],R).
     
% Another version (slower)
%
% The maximum number of inferences is achieved when list `A' in clause
% 1 of pack2__/3 is the longest. This happens when clause 2 of pack2__/3
% is not used, since it does not increase the size of the accumulator
% (`A'), therefore, the worst case is an input list `P' such that it
% does not contain repeated items. Under this circumstance, the number
% of user-defined steps is 2*|P|+2:
%       1 inference by clause 1 of pack1/2
% + |P|   inferences by clause 3 of pack2__/3
% +     1 inference  by clause 1 of pack2__/3
% + |P|   inferences to compute the body of clause 1 of pack2__/3
%
pack2(P,Q) :- pack2__([],P,Q).

pack2__(         A,   [],Q) :- rev(A,Q).
pack2__([[I|Is]|A],[I|P],Q) :- pack2__([[I,I|Is]|A],P,Q), !.
pack2__(         A,[I|P],Q) :- pack2__(     [[I]|A],P,Q).

%=====================================================================
% `encode(L,P)' is provable iff `P' is a list of pairs `{N,I}' of
% numbers `N' and items `I' from `P', such that `N' is the number of
% consecutively repeated occurrences of `I' in `P'.
%
% The number of inferences is maximum when the accumulative list `A'
% in the first clause of encode__/3 is the longest. This happens when
% the input list `L' does not contain consecutively repeated
% items. Under this assumption, the number of user-defined steps is
% 2*|P|+2:
%       1 inference by clause 1 of encode/2
% + |P|   inferences by clause 3 of encode__/3
% +     1 inference  by clause 1 of encode__/3
% + |P|   inferences to compute the body of clause 1 of encode__/3
%         (due to rev(A,P).
%
encode(L,P) :- encode__(L,[],P).

encode__(   [],        A,P) :- rev(A,P).
encode__([I|L],[{N,I}|A],P) :- M is N+1, encode__(L,[{M,I}|A],P),!.
encode__([I|L],        A,P) :- encode__(L,[{1,I}|A],P).

%=====================================================================
% `encode_opt(L)' is the same as `encode(L)' except that any pair
% `{1,I}' is replaced by `I'.
%
% The maximum number of inferences happens in the same circumstance as
% for encode/2 and is the same, i.e., 2*|P|+2.
%
encode_opt(L,P) :- encode_opt__(L,[],P).

encode_opt__(   [],          A,P) :- rev(A,P).
encode_opt__(  [I|L],[{N,I}|A],P) :-
    M is N+1, encode_opt__(L,[{M,I}|A],P),!.
encode_opt__([I,I|L],        A,P) :- encode_opt__(L,[{2,I}|A],P),!.
encode_opt__(  [I|L],        A,P) :- encode_opt__(L,[I|A],P).

%=====================================================================
% Queues

% A one-stack implementation
%
% The number of inferences to prove dequeue1(Qin,Qout,I) is |Qin|-1.
%
enqueue1(I,Qin,[I|Qin]).

dequeue1(    [I],      [],I).
dequeue1([J|Qin],[J|Qout],I) :- dequeue1(Qin,Qout,I).

% A two-stack implementation
%
% When proving dequeue2(Qin,Qout,I), the worst case happens when Qin =
% {In,[]}, because list In has to be reversed. This means that the
% maximum number of inferences is |In|+1. In the best case, this
% number is zero. Therefore, this two-stack implementation has to be
% preferred, in general, to the one-stack implementation.
%
enqueue2(I,{In,Out},{[I|In],Out}).

dequeue2({In,[I|Out]},{In,Out},I).
dequeue2({[I|In],[]},{[],Out},J) :- rev([I|In],[J|Out]).

%=====================================================================
% One-way Insertion sort (by increasing values)
%
% The maximum number of inferences is achieved when the recursive goal
% insert(I,T,S) is proved the maximum number of times (since the other
% clause is a fact). This happens when the condition J < I is true as
% often as possible, that is, when the input is already sorted in
% increasing order.
%
% Under this circumstance, let S(n) be the number of inferences to
% prove `isort(L,T)', where `n' is the length of list `L'. Let I(n) be
% the number of inferences needed to prove `insert(I,T,S)' where `n'
% is the length of `T'. We have
%
% Clause 1 of isort/2  =>   S(0) = 0
% Clause 2 of isort/2  => S(n+1) = 1 + S(n) + I(n) with n >= 0
% Clause 1 of insert/3 => I(n+1) = 1 + I(n)        with n >= 0
% Clause 2 of insert/3 =>   I(0) = 0
%
% It is easy to express I(n) in closed form:
%
% I(n) = n with n >=0
%
% By replacing this into the recurrence defining S(n), we get
%
%   S(0) = 0
% S(n+1) = 1 + S(n) + n  with n >= 0
%
% By writing the successive terms, the way to a closed form appears:
%
%  S(1) = 1 +   S(0) +     0
%  S(2) = 1 +   S(1) +     1
%  S(3) = 1 +   S(2) +     2
% ...........................
%  S(n) = 1 + S(n-1) + (n-1)
%
% By summing both sides and subtracting S(1) + ... + S(n-1), we draw
%
%   S(n) =  n +   S(0) + (0 + 1 + 2 + ... + (n-1)) with n > 0
%        =  n +      0 + n(n-1)/2
% 2*S(n) = 2n + n^2 - n = n^2 + n = n(n+1)
%   S(n) = n(n+1)/2 ~ (1/2)n^2 when n -> +infinity.
%
% When n=0, we have S(0) = 0(0+1)/2 = 0, which agrees with the fact
% that S(0) = 0, therefore the formula with derived is valid for 0
% as well.
%
isort(   [],[]).
isort([I|L],S) :- isort(L,T), insert(I,T,S).

insert(I,[J|T],[J|S]) :- J < I, !, insert(I,T,S).
insert(I,    T,[I|T]).

%=====================================================================
% Merge sort (by increasing values)
%
%
m_sort([],[]).
m_sort( L, S) :- mk_single(L,Sgl), mrg_all(Sgl,S).

% The number of inferences to prove `mk_single(L,Sgl)' is |L|.
%
mk_single(   [],     []).
mk_single([I|L],[[I]|S]) :- mk_single(L,S).

mrg_all([P],  P).
mrg_all(  L,Mrg) :- mrg_twowise(L,Q), mrg_all(Q,Mrg).

mrg_twowise([P,Q|S],[PQ|Mrg]) :- merge(P,Q,PQ), mrg_twowise(S,Mrg).
mrg_twowise(L,L).

% The maximum number of inferences to prove `merge(P,Q,R)', `P' and
% `Q' being already sorted by increasing values of their elements,
% happens when
%
merge(   [],    Q,    Q).
merge(    P,   [],    P).
merge([I|P],[J|Q],[I|R]) :- I < J, merge(P,[J|Q],R).
merge(    P,[J|Q],[J|R]) :- merge(P,Q,R).

%=====================================================================
% `half_of(L,LL)' is provable iff list `LL' is made of the catenation
% of list `L' twice.
%
half_of(L,LL) :- cat(L,L,LL).

%=====================================================================
% Maximum of two integers
%
max(X,Y,Z) :- X < Y, !, Z=Y.
max(X,_,X).

max1(X,Y,Y) :- X =< Y.
max1(X,Y,X) :- X > Y.


%=====================================================================
% all_mem/3 finds all the items who are members of a list.
%
all_mem(X,L,R) :- findall(X,mem(X,L),R).

%=====================================================================
% `max_item(L,Max)' is provable iff `Max' is the maximum integer of
% list `L'.
%
% The number of inferences for answering the query 
% "?- max_item(+L,-Max)." is |L|+2.
%
max_item([I|L],Max):- max_item__(L,I,Max).
 
max_item__([],I,I).
max_item__([J|L],I,Max):- I < J, !, max_item__(L,J,Max).
max_item__([_|L],I,Max):- max_item__(L,I,Max).
 
%=====================================================================
% Factorial

% With a cut
%
fact(N,R) :- N >= 0, fact1(N,1,R).

fact1(0,R,R) :- !.
fact1(N,A,R) :- M is N-1, B is N*A, fact1(M,B,R).

% Without cutting
%
fact2(N,R) :- fact3(N,1,R).

fact3(0,R,R).
fact3(N,A,R) :- N > 0, M is N-1, B is N*A, fact3(M,B,R).

%=====================================================================
% `prod(L,P)' is provabable iff `P' is the product of the integers in
% the list `L'.
%
% The number of inferences for answering the query "?- prod(+L,-P)."
% is |L| in the worst case (i.e., when there is no 0 in `L').
%
prod([0|_],0) :- !.
prod(  [N],N) :- !.
prod([I|L],J) :- prod(L,K), J is I*K.

%=====================================================================
% In `rem_last(L,I,P)', `P' is the list containing all the items of
% list `L', except the last occurrence of `I'.

% The number of inferences to answer the query
% "?- rem_last(-L,-I,+P)." is |L|+2.
%
% In the goal rem_last__(-L,-I,+_,+P) the third argument is either
% "none" if `I' is not in `L', or else "some". The algorithm is
% simple: if the head of the current list (starting with `L') is not
% `I' then copy `I' to `P' and process the tail of `L' (clause 4 of
% rem_last__/4); if the head is `I', then process the tail of `L' and
% if `I' was already occurring in it (`some', in clause 2 of
% rem_last__/4), then copy `I' to `P'; otherwise (clause 3 of
% rem_last__/4), `P' is `L' without the head (since it is the first
% occurrence of `I'). 
%
% Note: This version uses a minimum of memory since `P' shares the
% longest suffix with `L' (and, in the worst case for efficiency,
% L=P).
%
% Note that clauses 4 and 5 could be replaced by the following
%
% rem_last__([J|L],I,S,[J|P]) :- rem_last__(L,I,S,P).
%
% but this does not guarantee sharing in case `I' is not in `L'.
%
rem_last(L,I,P) :- rem_last__(L,I,_,P).

rem_last__(   [],_,none,   []).
rem_last__([I|L],I,some,[I|P]) :- rem_last__(L,I,some,P).
rem_last__([I|L],I,some,    L).
rem_last__([J|L],I,some,[J|P]) :- rem_last__(L,I,some,P).
rem_last__(    L,_,none,    L).

%=====================================================================
% `sum(T,S)' is provable iff `S' is the sum of the integers at each
% node of binary tree `T' (one integer per node).
%
sum({Left,N,Right},S) :- sum__({Left,N,Right},S).

sum__(empty,0).
sum__({Left,N,Right},S) :-
  sum__(Left,L), sum__(Right,R), S is N + L + R.
