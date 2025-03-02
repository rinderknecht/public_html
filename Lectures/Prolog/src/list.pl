% member(X,push(X,_)) -> true
% member(X,push(_,T)) -> member(X,T)
%
member(X,push(X,_)).
member(X,push(_,T)) :- member(X,T).

% Note that insert(a,push(b,push(a,empty)),push(b,push(a,empty)))
% is false. See `insert_if_missing'.
%
% ?- insert(a, push(a,empty), T).
%
insert(X,S,push(X,S)).
insert(X,push(Y,S),push(Y,T)) :- insert(X,S,T).

% delete_first(X,push(X,R)) -> R
% delete_first(X,push(Y,R)) -> push(Y,delete_first(X,R))
%
% Note that `delete_first(a,empty,empty)' is false.
%
% ?- delete_first(a,empty,empty).
% ?- delete_first(X,push(1,push(2,push(3,empty))),L).
% ?- delete_first(3, W, push(a,push(b,push(c,empty)))).
% ?- delete_first(a, push(a,push(a,empty)), T).
%
delete_first(X,push(X,S),S).
delete_first(X,push(Y,S),push(Y,T)) :- delete_first(X,S,T).

% `insert1' = `insert'
%
insert1(X,S,T) :- delete_first(X,T,S).

% `delete_first1' is `delete_first' plus the case `delete_first1(X,S,S)'
% where S does not contain X.
%
% ?- delete_first1(a, push(a,push(a,empty)),T).
%
delete_first1(_,empty,empty).
delete_first1(X,push(X,S),S).
delete_first1(X,push(Y,S),push(Y,T)) :- delete_first1(X,S,T).

delete_first2(_,empty,empty).
delete_first2(X,push(X,S),S):-!.
delete_first2(X,push(Y,S),push(Y,T)) :- delete_first2(X,S,T).

% Compare
% ?- delete_first(a,push(a,push(a,empty)),T).
% ?- delete_first1(a,push(a,push(a,empty)),T).
% ?- delete_first2(a,push(a,push(a,empty)),T).

delete_all(_,empty,empty).
delete_all(X,push(X,S),T) :- delete_all(X,S,T).
delete_all(X,push(Y,S),push(Y,T)) :- delete_all(X,S,T).

% catenate(empty,T)     -> T
% catenate(push(X,S),T) -> push(X,catenate(S,T))
%
% catenate(S,T,U) is true if `U' is the list made of `S' followed by
% `T'.
%
catenate([],T,T).
catenate([X|S],T,[X|U]) :- catenate(S,T,U).

% reverse(empty)     -> empty
% reverse(push(E,S)) -> catenate(reverse(S), push(E,empty))
%
% reverse1(S)            -> reverse2(S,empty)
% reverse2(pushd(E,S),T) -> reverse2(S,push(E,T))
%
reverse(empty,empty).
reverse(push(E,S),R) :- reverse(S,RS), catenate(RS,push(E,empty),R).

reverse1(S,T) :- reverse2(S,empty,T).
reverse2(empty,T,T).
reverse2(push(E,S),T,R) :- reverse2(S,push(E,T),R).

% max(X,Y,M) is true if `M' is the maximum of `X' and `Y'.
% Both arguments must be instantiated.
%
max(X,Y,X) :- X >= Y,!.
max(_,Y,Y).

max1(X,Y,X) :- X >= Y.
max1(X,Y,Y) :- Y > X.

% max_list1(N,L) is true if `N' is greater than any number in the list `L'.
% Both arguments need to be instanciated.
%
max_list1(_,[]).
max_list1(N,[P|L]) :- N >= P, max_list1(N,L).

% max_list(N,L) is true if `N' is the greatest number in the list `L'.
% Both arguments need to be instanciated.
%
max_list(N,L) :- member(N,L), max_list1(N,L).

% max_list2(N,L) is the same as max_list(N,L) except that `N' does not
% need to be instanciated.
% 
max_list2(N,[N]).
max_list2(R,[P,Q|L]) :- P >= Q, max_list2(R,[P|L]).
max_list2(R,[P,Q|L]) :- Q >= P, max_list2(R,[Q|L]).

% sum_list(L,S) is true if `S' is the sum of the numbers in `L'.
%
sum_list([N],N).
sum_list([N|L],S) :- sum_list(L,T), S is T + N.

% sublist(S,L) is true if `S' is a sublist of `L'.
%
sublist([],_).
sublist([P|S],[P|T]) :- sublist(S,T).
sublist(S,[_|T]) :- sublist(S,T).

% partition(Sum,Sublist,List) if `Sublist' is a sublist of `List'
% and the sum of the numbers in `Sublist' is `Sum'.
%
partition(0,[],_).
partition(A,[P|S],[P|T]) :- partition(B,S,T), A is B + P.
partition(A,S,[_|T]) :- partition(A,S,T).

% split(U,V,W) is true if `V' is the sublist of `U' made of positive
% or nul numbers and `W' is the sublist of `U' made of negative
% numbers.
%
split([],[],[]).% :-!.
split([X|T],[X|P],N) :-	X>=0, split(T,P,N). %X>=0,!,	split(T,P,N).
split([X|T],P,[X|N]) :-	split(T,P,N).

% flatten(S,F) is true if `F' is the list of items in the list of
% lists of items `S' (in the same order).
%
flatten([],[]).
flatten([L|LL],R) :- catenate(L,F,R), flatten(LL,F).
