-module(hw).
-compile(export_all).

%=====================================================================
% `mem(X,L)' is `true' if X is in L, `false'  otherwise.
%
mem(_,   []) -> false;
mem(X,[X|_]) -> true;
mem(X,[_|P]) -> mem(X,P).    

%=====================================================================
% `last(L)' is the last item in list `L'.
%
last(  [X]) -> X;
last([_|P]) -> last(P).

%=====================================================================
% `sum(L)' is the sum of the numbers in L. (Undefined if L is empty.)
%
sum(  [N]) -> N;
sum([N|P]) -> N + sum(P).

%=====================================================================
% `occ(X,L)' is the number of occurrences of X in the list L.
%
occ(_,   []) -> 0;
occ(X,[X|P]) -> 1 + occ(X,P);
occ(X,[_|P]) -> occ(X,P).
    
%=====================================================================
% `index(X,L,N)' is the index of the first occurrence of X in list L,
% the first item having index N (integer), or the atom `absent' if not
% found.
%
index(_,   [],_) -> absent;
index(X,[X|_],N) -> N;
index(X,[_|P],N) -> index(X,P,N+1).

%=====================================================================
% `indices(X,L,N)' is the list of the indices of all occurrences of X
% in list L, in increasing order, the first item having index N
% (integer).
%
indices(_,   [],_) -> [];
indices(X,[X|P],N) -> [N|indices(X,P,N+1)];
indices(X,[_|P],N) -> indices(X,P,N+1).

%=====================================================================
% `pen(L)' is the penultimate item in list L, if any.
%
penul([I,_]) -> I;
penul([_|L]) -> penul(L).

%=====================================================================
% `join(P,Q)' is the list whose first items are those of `P' in the
% same order, and the following are those of `Q' in the same order. In
% other words, `join(P,Q)' is the catenation of P and Q.
%
join(   [],Q) -> Q;
join([I|P],Q) -> [I|join(P,Q)].

%=====================================================================
% `rm_fst(I,L)' is the list `L' without the first occurrence of
% item `I'. If `I' is absent in `L', then `rm_fst(I,L)' is `L'.
%
% There are three cases:
%
% (1) The list is empty.
%     The delay is 1.
% (2) The list is not empty and the item is missing.
%     The delay is n + 1.
% (3) The list is not empty and the item occurs at position k, the
%     head being at position 0.
%     The delay is k + 1.
%
% The best case is then when the item is the head of the list.
%
% The worst case is then when the list is not empty and the item is
% missing.
%
% Note: `rm_fst(I,L)' shares the longest possible suffix with `L'
% except if I is not in L.
%
rm_fst(_,   []) -> [];
rm_fst(I,[I|L]) -> L;
rm_fst(I,[J|L]) -> [J|rm_fst(I,L)].

%=====================================================================
% `rev_join(P,Q)' is the list whose first items are the items of `P'
% reversed, followed by the items of `Q' in the same order.
%
% The delay of `rev_join(P,Q)' is n+1, where `n' is the length of L.
%
rev_join(   [],Q) -> Q;
rev_join([I|P],Q) -> rev_join(P,[I|Q]).

%=====================================================================
% `rev(L)' is the list containing all the items of `L' reversed.
%
% Let `n' be the length of `L'.

% The delay of rev(L) is n+2.
%
rev(L) -> rev_join(L,[]).  % Clause 1

% The delay of `srev(L)' is (n+1)(n+2)/2.
%
srev(   []) -> [];
srev([I|L]) -> join(srev(L),[I]).

%=====================================================================
% Filtering the unique items
%

