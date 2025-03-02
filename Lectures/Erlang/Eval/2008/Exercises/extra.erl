

%=====================================================================
% `len(L)' is the number of items in `L'.
%
% Let `n' be the length of L.

% The delay of `len(L)' is n+1.
%
len(   []) -> 0;
len([_|L]) -> 1 + len(L).

% The delay of len_tf(L) is n+2.
%
len_tf(L) -> len__(L,0).

len__(   [],N) -> N;
len__([_|L],N) -> len__(L,N+1).

%=====================================================================
% `join(P,Q)' is the list whose first items are those of `P' in the
% same order, and the following are those of `Q' in the same order.
%
% Let `n' be the length of P.

% The delay of join_tf(P,Q) is 2n+2.
%
join_tf(P,Q) -> join_tf__(P,Q,[]).

join_tf__(   [],Q,A) -> rev_join(A,Q);
join_tf__([I|P],Q,A) -> join_tf__(P,Q,[I|A]).

% The delay of join_tf1(P,Q) is 2n+2.
%
join_tf1(P,Q) -> join_tf1__(P,Q,[]).

join_tf1__(   [],Q,   []) -> Q;
join_tf1__(   [],Q,[I|A]) -> join_tf1__([],[I|Q],A);
join_tf1__([I|P],Q,    A) -> join_tf1__(P,Q,[I|A]).


% Continuation-Passing Style of rev/1.
%
rev3(L) -> rev3_x(L,fun(X) -> X end).

rev3_x(   [],K) -> K([]);
rev3_x([I|L],K) -> rev3_x(L,fun(X) -> join1(X,[I],K) end).

join1(L,M,K) -> join1_x([],L,M,K).

join1_x(   [],    [],M,K) -> K(M);
join1_x([I|A],    [],M,K) -> join1_x(    A,[],[I|M],K);
join1_x(    A, [I|L],M,K) -> join1_x([I|A], L,    M,K).

% Defunctionalisation of rev3/1
%
rev4(L) -> rev4_x(L,[]).
    
rev4_x(   [],K) -> apply4(K,   []);
rev4_x([I|L],K) -> rev4_x(L,[I|K]).
    
apply4(   [],X) -> X;
apply4([I|K],X) -> join4(X,[I],K).

join4(L,M,K) -> join4_x([],L,M,K).

join4_x(   [],    [],M,K) -> apply4(K,M);
join4_x([I|A],    [],M,K) -> join4_x(    A,[],[I|M],K);
join4_x(    A, [I|L],M,K) -> join4_x([I|A], L,    M,K).

%=====================================================================
% `rm_fst(I,L)' is the list `L' without its first occurrence of
% item `I'. If `I' is absent in `L', then `rm_fst(I,L)' is `L'.

% There are three cases:
% 
% (1) The list is empty.
%     The delay is 1.
% (2) The list is not empty and the item is missing.
%     The delay is 2n+4.
% (3) The list is not empty and the item occurs at position k.
%     The delay is 2k+3.
%
% The best case is then when the list is not empty and the item is the
% head.
%
% The worst case is then when the list is not empty and the item is
% missing.
%
% Note: `rm_fst_tf(I,L)' shares the longest possible suffix with `L'.
%
rm_fst_tf(I,L) -> rm_fst_tf__(I,L,[]).

rm_fst_tf__(_,   [],A) -> rev(A);
rm_fst_tf__(I,[I|L],A) -> rev_join(A,L);
rm_fst_tf__(I,[J|L],A) -> rm_fst_tf__(I,L,[J|A]).

%=====================================================================
% `rm_all(I,L)' is the list containing all the items of list `L' in
% the same order, except all items matching `I'.
%
% Let us assume that the length of `L' is `n'.

% The delay of `rm_all(I,L)' is n+1.
%
rm_all(_,   []) -> [];
rm_all(I,[I|L]) -> rm_all(I,L);
rm_all(J,[I|L]) -> [I|rm_all(J,L)].

% The delay of rm_all_tf(I,L) is 2n+4.
%
rm_all_tf(I,L) -> rm_all_tf__(I,L,[]).

rm_all_tf__(_,   [],A) -> rev(A);
rm_all_tf__(I,[I|L],A) -> rm_all_tf__(I,L,A);
rm_all_tf__(I,[J|L],A) -> rm_all_tf__(I,L,[J|A]).
    
%=====================================================================
% `rm_lst(L,I)' is the list containing all the items of list `L',
% except the last occurrence of `I'.
%
% Let us assume that the length of `L' is `n'.

% There are three cases:
%
% (1) The list is empty.
%     The delay is 6.
% (2) The list is not empty and the item is missing.
%     The delay is 3n+6.
% (3) The list is not empty and the item occurs at position k.
%     The delay is 3n-k+4.
%
% The best case thus happens when the list is not empty and the item
% is the last, as the delay becomes 2n+5.
%
% The worst case hence is when the list is not empty and the item is
% missing, as the delay becomes 3n+6.
%
rm_lst(I,L) -> rev(rm_fst(I,rev(L))).

% The delay of this version is n + last - first + 1, where n is the
% length of the list, "last" is the index of the last occurrence of I
% and "fist" is index of the first occurrence of I (by definition,
% the head of the list has index 0).
%
% As a consequence, the best case happens when all the occurrences are
% consecutive and the best delay is n + O, where O is the number of
% occurrences. The worst case happens when there is at least one
% occurrence at the head and the end of the list, so the worst delay
% is 2n. Note that if all the items in the list are occurrences, the
% best delay equals the worst delay, as expected.
%
rm_lst1(_,   []) -> [];
rm_lst1(I,[I|P]) -> rm_lst1__(I,P,P);
rm_lst1(I,[J|P]) -> [J|rm_lst1(I,P)].

rm_lst1__(_,   [],P) -> P;
rm_lst1__(I,[I|_],P) -> [I|rm_lst(I,P)];
rm_lst1__(I,[_|Q],P) -> rm_lst1__(I,Q,P).

% `rm_lst2(I,L)' shares the longest suffix with `L'.
%
rm_lst2(I,L) -> 
  case rm_lst2__(I,L) of
    {some,M} -> M;
    none     -> L
  end.

rm_lst2__(I,[I|L]) ->
  case rm_lst2__(I,L) of
    {some,M} -> {some,[I|M]};
    none     -> {some,L}      % Maximising suffix
  end;
rm_lst2__(I,[J|L]) ->
  case rm_lst2__(I,L) of
    {some,M} -> {some,[J|M]};
    none     -> none
  end;
rm_lst2__(_,[]) -> none.

%=====================================================================
% Intersection of two lists
%
inter([],_) -> [];
inter( L,M) -> inter(L,M,[]).

inter(   [],    _,_) -> [];
inter([_|L],   [],P) -> inter(L,P,[]);
inter([I|L],[I|M],P) -> [I|inter(L,M,P)];
inter(    L,[J|M],P) -> inter(L,M,[J|P]).

%=====================================================================
% Predicate for palindromes
%
% Note the hidden delay in the patter of eq/2.
%
palindrome(L) -> eq(L,rev(L)).

eq(P,P) -> true;
eq(_,_) -> false.
    
% Another version
%
palindrome1(L) -> are_equal(split(L)).

split(L) ->
    Len = len(L),
    case Len rem 2 of
      0 -> split(even,Len div 2,[],L);
      1 -> split(odd,Len div 2,[],L)
    end.

split(even,0,R,S) ->
    {R,S};
split(odd,0,R,[_|S]) ->
    {R,S};
split(Mode,HalfLen,Left,[Item|Right]) ->
    split(Mode,HalfLen-1,[Item|Left],Right).

are_equal({L,L}) -> yes;
are_equal(    _) -> no.

%=====================================================================
% Repeating the first item of the list
%
% The delay is 1+((n-1)+1)=n+1.
%
rep_fst(   [])  -> [];
rep_fst([I|L])  -> [I|repeat(I,L)].
repeat(_,   []) -> [];
repeat(I,[_|L]) -> [I|repeat(I,L)].

% This version uses temporarily more memory but is shorter.
% The delay is also n+1.
%
rep_fst1([I,_|L]) -> [I|rep_fst1([I|L])]; % Two pushes instead of one
rep_fst1(L) -> L. % TEST?

%=====================================================================
% Repeating the last item of the list
%
% The delay is 1+(n)+(n+1)=2n+2.
%
rep_lst([]) -> [];
rep_lst( L) -> rep_fst(last(L)).

% Using a list reversal leads to a delay of (n+2)+(n+1)=2n+3.
%
rep_lst1(L) -> rep_fst(rev(L)).

%=====================================================================
% `compress(L)' is the list containing the items of list `L' in the
% same order but without repeating consecutive items.
%
% The number of steps is n+1.
%
% Note the use of the alias P in the pattern [I|P=[I|_]] instead of
% [I,I|L]. This provides maximum sharing between the input and the
% output. Otherwise we would write
%
% compress([I,I|L]) -> compress([I|L]); % One push instead of none
%
compress(         []) -> [];
compress([I|L=[I|_]]) -> compress(L);
compress(      [I|L]) -> [I|compress(L)].

%=====================================================================
% `pack(L)' is the list of the lists of repeated items of `L', in the
% same original order.
%
% The delay is 1+((n-1)+1)=n+1.
%
% Note the usage of an alias Q in the pattern Q=[I|_] instead of
% [I|Q]. This allows maximum sharing between the input and the
% output. Otherwise, we would write
%
% pack([I|P],[I|Q]) -> pack(P,[I,I|Q]); % Two pushes instead of one
%
pack(   []) -> [];
pack([I|L]) -> pack(L,[I]).

pack(   [],      Q) -> [Q];
pack([I|P],Q=[I|_]) -> pack(P,[I|Q]);
pack([J|P],      Q) -> [Q|pack(P,[J])].

% Another version
%
pack1(     []) -> [];
pack1([I,I|P]) -> pack__(I,pack(P));
pack1(  [I|P]) -> [[I]|pack(P)].

pack__(I,[P=[I|_]|Q]) -> [[I,I|P]|Q];
pack__(I,          Q) -> [[I,I]|Q].

% This version does not work if the list contains at least a list,
% for example [a,[b]].
%
pack_err(          []) -> [];
pack_err([[I|Is],I|L]) -> pack_err([[I,I|Is]|L]);
pack_err(  [[I|Is]|L]) -> [[I|Is]|pack_err(L)];
pack_err(       [I|L]) -> pack_err([[I]|L]).

%=====================================================================
% `encode(L)' is the list which contains pairs `{N,I}' of numbers `N'
% and items `I' from `L', such that `N' is the number of consecutively
% repeated occurrences of `I' in `L'.
%
% The delay is n+1, where `n' is the length of the input list.
%
% Note the usage of an alias `L' in the pattern [I|L=[I|_]], to avoid
% a push, hence save memory. Otherwise we should write
%
% enc_bis([I,I|L],N) -> enc_bis([I|L],N+1); % One push instead of none
%
encode([]) -> [];
encode( L) -> enc(L,0).

enc(         [],0) -> [];
enc([I|L=[I|_]],N) -> enc(L,N+1);
enc([I|      L],N) -> [{N+1,I}|enc(L,0)].

% This is another version, whose delay is also n+1.
%
encode_bis(   []) -> [];
encode_bis([I|L]) -> enc_bis(L,{1,I}).

enc_bis(   [],    K) -> [K];
enc_bis([I|L],{N,I}) -> enc_bis(L,{N+1,I});
enc_bis([J|L],    K) -> [K|enc_bis(L,{1,J})].

% This version is in tail form.
%
% The maximum number of user-defined steps is achieved when the number
% of steps to compute `rev(A)', in the body of clause 1 of encode/2,
% is maximum. This happens when items are not consecutively repeated
% in `L', that is, when clause 2 of encode/2 is never employed. Under
% this circumstance, the maximum number of steps is 2*len(L)+4:
%          1 step  by clause 1 of encode/1
% +   len(L) steps by clause 3 of encode/2
% +        1 step  by clause 1 of encode/2
% + len(L)+2 steps to compute the body of clause 1 of encode/2.
%
% The maximum number of pushes happens when the maximum number of
% steps is reached, that is 2*len(L):
%   len(L) pushes by clause 3 of encode/2
% + len(L) pushes to compute the body of clause 1 of encode/2
%
encode_tf(L) -> encode_tf([],L).

encode_tf(        A,   []) -> rev(A);
encode_tf([{N,I}|A],[I|L]) -> encode_tf([{N+1,I}|A],L);
encode_tf(        A,[I|L]) -> encode_tf([{1,I}|A],L).
     
% With a supplementary argument
%
encode1(L) -> encode1(L,[],0).

encode1([I,I|L],A,N) -> encode1([I|L],A,N+1);
encode1([I,J|L],A,N) -> encode1([J|L],[{N+1,I}|A],0);
encode1(    [I],A,N) -> rev([{N+1,I}|A]).

%=====================================================================
% `encode_opt(L)' is the same as `encode(L)' except that any pair
% `{1,I}' is replaced by `I'. The code is based on encode/1 and
% its delay is n+1.
%
% Not the usage of the alias L in the pattern L=[I|_] instead of
% [I|L]. This allows maximum sharing between the input and the
% output. Otherwise we would write
%
% enc_opt([I,I|L],N) -> enc_opt([I|L],N+1); % One push instead of none
%
encode_opt([]) -> [];
encode_opt( L) -> enc_opt(L,0).

enc_opt(         [],0) -> [];
enc_opt([I|L=[I|_]],N) -> enc_opt(L,N+1);
enc_opt([I|      L],0) -> [I|enc_opt(L,0)];
enc_opt([I|      L],N) -> [{N+1,I}|enc_opt(L,0)].

% Erroneous version: if the list contains a pair {N,I} followed by I.
%
encode_opt_err(L) -> encode_opt_err([],L).

encode_opt_err(        A,     []) -> rev(A);
encode_opt_err([{N,I}|A],  [I|L]) -> encode_opt_err([{N+1,I}|A],L);
encode_opt_err(        A,[I,I|L]) -> encode_opt_err([{2,I}|A],L);
encode_opt_err(        A,  [I|L]) -> encode_opt_err([I|A],L).

%=====================================================================
% `drop(L,K)' is the list made of the items of list `L' in the same
% order, except the items occurring every number `K', the first item
% being counted as occurring at position 1. The call is undefined if
% K =< 0.
%
% The delay is 1+(n+1)=n+2.
%
drop(L,K) when K > 0 -> drop(L,K,1).

drop(   [],_,_) -> [];
drop([_|L],K,K) -> drop(L,K,1);
drop([I|L],K,M) -> [I|drop(L,K,M+1)].

% This version is in tail form.
%
% The maximum number of steps is achieved when `rev(A)' is the
% slowest, i.e., when the number of pushes on `A' is the greatest,
% that is when clause 3 of drop_tf/4 is used as much as possible, which
% means that the period `P' is greater than the list of `L'. Under
% this circumstance, the number of steps is 2*len(L)+4:
%          1 step  by clause 1 of drop_tf/2
% + len(L)   steps by clauses 2 and 3 of drop_tf/4
% +        1 step  by clause 1 of drop_tf/4
% + len(L)+2 steps for the body of clause 1 of drop_tf/4.
%
% The maximum number of pushes happens in the same case as the maximum
% number of steps, since the only pushes occur in the body of clause 3
% of drop_tf/4. This number is 2*len(L):
%   len(L) pushes by clause 3 of drop_tf/4
% + len(L) pushes to compute the body of clause 1 of drop_tf/4.
%
drop_tf(L,P) -> drop_tf([],1,L,P).

drop_tf(A,_,   [],_) -> rev(A);
drop_tf(A,P,[_|L],P) -> drop_tf(A,1,L,P);
drop_tf(A,N,[I|L],P) -> drop_tf([I|A],N+1,L,P).

%=====================================================================
% Maximum of two integers
%
max_int(A,B) when A =< B -> B;
max_int(A,B) when A > B  -> A.
    
%=====================================================================
% `max_f(F,A,B)' is max{F(X) | A =< X =< B}.

% Not tail form
%
max_f1(F,B,B)            -> F(B);
max_f1(F,A,B) when A < B -> max_int(F(A),max_f1(F,A+1,B)).

% Almost in tail form
%
% It is not useful to transform the code so that the embedded call to
% max/2 is top-level, since max/2 is a function that terminates in one
% step. Also, this will not render the source in tail form, since an
% embedded call to `F' remains. The stack usage remains low (in spite
% `F' may use a lot of stack space), so it is probably not worth the
% trouble.
%
max_fun(F,A,B) when A =< B -> max_fun(F,A+1,B,F(A)).
    
max_fun(F,B,B,M) -> max_int(M,F(B));
max_fun(F,A,B,M) -> max_fun(F,A+1,B,max_int(M,F(A))).

%=====================================================================
% `max_abs(F,A,B)' is smallest `U' such that
% F(U) = max{F(X) | A =< X =< B}.

% Not in tail form
%
max_abs1(_,B,B) -> B;
max_abs1(F,A,B) when A < B ->
  M = max_abs1(F,A+1,B),
  case F(A) >= M of
    true  -> A;
    false -> M
  end.

% Almost in tail form
%
max_f(F,A,B) -> case F(A) =< F(B) of
                  true  -> B;
                  false -> A
                end.
    
max_abs(F,A,B) when A =< B -> max_abs(F,A+1,B,A).
    
max_abs(F,B,B,M) -> max_f(F,B,M);
max_abs(F,A,B,M) -> max_abs(F,A+1,B,max_f(F,A,M)).

%=====================================================================
% `split(L,K)' is a pair of lists, the first containing the first `K'
% items of `L' in the original order, the second containing the
% remaining items of `L' in the original order. If K < 0 or n < K or
% n=0 then `split(L,K)' is undefined.
%
% The delay is K + 1.
%
split(    L,0) -> {[],L};
split([I|L],K) -> {P,M}=split(L,K-1), {[I|P],M}.

% Another version
%
% The maximum number of steps occurs when `rev(A)' is the longest,
% that is, when the split1 is made at the penultimate position:
% `N=len(L)-1' (and len(A) = N). This number is then 2*len(L)+2:
%            1 step  by clause 1 of split1/2
% + len(L)-1   steps by clause 2 of split1/3
% +          1 step  by clause 1 of split1/3
% + len(L)-1+2 steps for computing rev(A).
%
% The maximum number of pushes happens in the same case as the maximum
% number of steps: 2*len(L) - 1:
%   len(L)-1 pushes by clause 2 of split1/3
% +        1 push   for [I|L] in the body of clause 1 of split1/3 (if
%                   the compiler does not optimise it, since `[I|L]'
%                   is invariant) 
% + len(L)-1 pushes for rev(A) in the body of clause 1 of split1/3.
%
% This version is NOT in tail form (the call `rev(A)' is inside a
% pair).
%
split1(L,N) when N > 0 -> split1([],L,N).

split1(A,P=[_|_],0) -> {rev(A),P};
split1(A,  [I|L],N) -> split1([I|A],L,N-1).

%=====================================================================
% `flatten(L)' is a list containing the items of list `L' which are
% not lists themselves, in the original order (if an item `I' of `L'
% is a list, then its items come before the items after `I' in `L').
%
flatten(       []) -> [];
flatten(   [[]|Q]) -> flatten(Q);
flatten([[I|P]|Q]) -> join(flatten([I|P]),flatten(Q));
flatten(    [I|Q]) -> [I|flatten(Q)].

% This is a version in tail form.
%
% The number of user-defined steps is the sum of the sublists lengths,
% due to clauses 3 and 4 of flatten_tf/2, plus the number of non-list and
% empty list items, by clauses 1 and 5 of flatten_tf/2. (Or: sum of the 
% lengths of the sublists, plus len(flatten_tf(L)), plus the
% number of empty lists.) Then one step for clause 1 of flatten_tf/2 and
% len(flatten_tf(L)) + 2 steps, plus one step for clause 1 of flatten_tf/1.
% Total: 
%   sum of sublists lengths
% + len(flatten_tf(L))
% + #[]
% + 1
% + len(flatten_tf(L)) + 2
% + 1
% That is: sum of sublists lengths + 2*len(flatten_tf(L)) + 4 + #[]
%
% For example, the number of steps to compute
% flatten_tf([[[[[q,a,[0]],[]]]],e,3,[5]]) is
% (1+3+2+1+1+1) + 2*6 + 4 + 1 = 26
%
flatten_tf(L) -> flatten_tf([],L).

flatten_tf(A,       []) -> rev(A);
flatten_tf(A,   [[]|L]) -> flatten_tf(    A,      L);
flatten_tf(A,  [[I]|L]) -> flatten_tf(    A,  [I|L]); %% Can be omitted
flatten_tf(A,[[I|M]|L]) -> flatten_tf(    A,[I,M|L]);
flatten_tf(A,    [N|L]) -> flatten_tf([N|A],      L).

% This version is not in tail form and slower than flatten/1 if there
% are not many empty lists in the input. Otherwise, it may be faster.
%
flatten1(       []) -> [];
flatten1(   [[]|L]) -> flatten1(L);
flatten1([[I|M]|L]) -> join(flatten1([I|M]),flatten1(L));
flatten1(    [N|L]) -> [N|flatten1(L)].

% Another version
%
flatten2(List) -> flatten2([],List).

flatten2(A,       []) -> rev(A);
flatten2(A,   [[]|L]) -> flatten2(A,L);
flatten2(A,[[I|M]|L]) -> flatten2(flatten2(A,[I|M]),L);
flatten2(A,    [N|L]) -> flatten2([N|A],L).

% Another version
%
flatten3(       []) -> [];
flatten3(   [[]|L]) -> flatten3(L);
flatten3([[I|M]|L]) -> flatten3([I,M|L]);
flatten3(    [N|L]) -> [N|flatten3(L)].
    
% This version is in tail form and uses simpler patterns.
% Credit: Jeong Jeong Hi
%
flatten4(L) -> flatten4(L,[],[]).

flatten4(   [],[],Q) -> rev(Q);
flatten4(   [], P,Q) -> flatten4(P,   [],    Q);
flatten4(  [I], P,Q) -> flatten4(I,    P,    Q);
flatten4([I|L], P,Q) -> flatten4(I,[L|P],    Q);
flatten4(    I, P,Q) -> flatten4(P,   [],[I|Q]).

% In Continuation-Passing Style
%
flatten_k(L)           -> flatten_k(L,fun(V) -> V end).
flatten_k(       [],K) -> K([]);
flatten_k(   [[]|Q],K) -> flatten_k(Q,K);
flatten_k([[I|P]|Q],K) -> 
  flatten_k([I|P],fun(V) -> flatten_k(Q, fun(W) -> join_k(V,W,K) end) end);
flatten_k(    [I|Q],K) -> flatten_k(Q,fun(V) -> K([I|V]) end).
join_k(   [],Q,K)      -> K(Q);
join_k([I|P],Q,K)      -> join_k(P,Q,fun(V) -> K([I|V]) end).

% Variant of flatten4/1
%
flatten5(L) -> flatten5(L,[]).

flatten5(   [],[]) -> [];
flatten5(   [], P) -> flatten5(P,[]);
flatten5(  [I], P) -> flatten5(I,P);
flatten5([I|L], P) -> flatten5(I,[L|P]);
flatten5(    I, P) -> [I|flatten5(P,[])].

% Variant
%
flatten6(T) -> flatten6(T,[]).

flatten6([],S) -> S;
flatten6([T1|T2],S) -> flatten6(T1,flatten6(T2,S));
flatten6(X,S) -> [X|S].    

% With a counter
%
flatten6c(T) -> flatten6c(T,[],1).

flatten6c([],S,C) -> {S,C+1};
flatten6c([T1|T2],S,C) -> {V,D}=flatten6c(T2,S,C+1),flatten6c(T1,V,D);
flatten6c(X,S,C) -> {[X|S],C+1}.

%=====================================================================
% compress
%
% This version is in tail form.
%
% The maximum number of steps is achieved when `L' contains no
% repeated item (so rev(A) is the slowest), that is 2n+4:
%     1 step  by clause 1 of compress/1
% + n   steps by clause 3 of compress/2
% +   1 step  by clause 1 of compress/2
% + n+2 steps to compute the body of clause 1 of compress/2.
%
% The maximum number of pushes occurs under the same circumstances,
% i.e., 2*len(L):
%   len(L) pushes by clause 3 of compress/2
% + len(L) pushes to compute the body of clause 1 of compress/2.
%
compress1(L) -> compress1([],L).

compress1(      A,   []) -> rev(A);
compress1([I|_]=A,[I|L]) -> compress1(A,L);
compress1(      A,[I|L]) -> compress1([I|A],L).

%=====================================================================
% One-way insertion to sort increasingly a list of integers
%
% Note that `Su' stands for `Sorted up' (increasingly).
%
% The maximum number of user-defined steps is achieved when the
% recursive call to insert/2 is computed the maximum number of
% times. This happens when the guard is true as often as possible,
% that is, when the input items are already sorted in decreasing
% order (because the insertion is done _after_ the sorting of the
% sublist, and has to reach the end of the sorted).
%
% Under this circumstance, let S(n) be the delay of `isort(L)' where n
% is the length of list `L'. Let T(n) be the delay of `insert(I,S)'
% where n is the length of list `S'. We have
%
% Clause 1 of isort/1 => S(0) = 1
% Clause 2 of isort/1 => S(n+1) = 1 + S(n) + T(I,n)  with n >= 0
% because len(isort(L)) = len(L);
% Clause 1 of insert/2 => T(0) = 1
% Clause 2 of insert/2 => T(n+1) = 1 + T(n)
% Clause 3 of insert/2 is never used in the worst case.
%
% It is easy to express T(n) in terms of n alone:
%
% T(n) = n + 1, for all n >= 0.
%
% By replacing this into the recurrence defining S(n), we get
%
% S(0) = 1
% S(n+1) = 2 + n + S(n) with n >= 0
%
% By writing the successive terms, the solution appears:
%
% S(1) = 2 +     0 +   S(0) =  3
% S(2) = 2 +     1 +   S(1) =  6
% S(3) = 2 +     2 +   S(2) = 10
% ....................................
% S(n) = 2 + (n-1) + S(n-1) with n > 0
%
% By summing both sides and subtracting S(1) + ... + S(n-1), we draw
%
% S(n) = 2n + (0 + 1 + ... + (n-1)) + S(0)
%      = 2n + n(n-1)/2 + 1
% 2*S(n) = 4n + n^2 - n + 2 = n^2 + 3n + 2
%
% Finally: S(n) = (n^2 + 3n + 2)/2.
%
% Checking that S(n) is always an integer: `n' is either even (n = 2p)
% or odd (n = 2p + 1):
%
% 2*S(2p) = (2p)^2 + 3(2p) + 2 = 4p^2 + 6p + 2 = 2(2p^2 + 3p + 1)
% 2*S(2p+1) = (2p+1)^2 + 3(2p+1) + 2 = (4p^2 + 4p + 1) + 6p + 3 + 2
%           = 4p^2 + 10p + 6 = 2(2p^2 + 5p + 3)
%
isort(   []) -> [];
isort([I|L]) -> insert(I,isort(L)).

insert(I,[J|Su]) when I > J -> [J|insert(I,Su)];
insert(I,    Su)            -> [I|Su].

% Tail form (general method)
%
isort_tf(L)                   -> isort(L,[]).
isort(   [],A)                -> iappk([],A);
isort([I|L],A)                -> isort(L,[{k1,I}|A]).
insert(I,[J|Su],A) when I > J -> insert(I,Su,[{k2,J}|A]);
insert(I,    Su,A)            -> iappk([I|Su],A).
iappk(V,[{k2,J}|A])           -> iappk([J|V],A);
iappk(V,[{k1,I}|A])           -> insert(I,V,A);
iappk(V,        [])           -> V.

% First, the atom k1 is not necessary in the definition of isort_tf/1,
% since all other values in the accumulator have a tag. Second, the
% second clause of isort/2 just reverses the input list, but this is a
% loss of time since we expect
%
% isort_tf(L) = isort_tf(rev(L)).
%
% We can therefore save time by not reversing the input list, which
% implies that we don't tag its items neither. Of course, the number
% of comparisons is the same.
%
isort_tf1([])                  -> [];
isort_tf1([I|L])               -> insert1(I,[],L).
insert1(I,[J|Su],A) when I > J -> insert1(I,Su,[{k2,J}|A]);
insert1(I,    Su,A)            -> iappk1([I|Su],A).
iappk1(V,[{k2,J}|A])           -> iappk1([J|V],A);
iappk1(V,     [I|A])           -> insert1(I,V,A);
iappk1(V,        [])           -> V.

% Continuation-Passing Style
%
isort_k(L) -> isort_k(L,fun(V) -> V end).

isort_k(   [],K) -> K([]);
isort_k([I|L],K) -> isort_k(L,fun(V) -> insert_k(I,V,K) end).
insert_k(I,[J|Su],K) when I > J -> insert_k(I,Su,fun(V) -> K([J|V]) end);
insert_k(I,    Su,K)            -> K([I|Su]).

% By adding an accumulator, insert/2 becomes tail recursive.
% Note that `Sd' stands for `Sorted down' (decreasingly).
%
isort2(   []) -> [];
isort2([I|L]) -> insert2([],I,isort2(L)).
     
insert2(Sd,I,    [])            -> rev([I|Sd]);
insert2(Sd,I,[J|Su]) when I > J -> insert2([J|Sd],I,Su);
insert2(Sd,I,    Su)            -> rev_join(Sd,[I|Su]).

% By adding an accumulator, isort2/1 becomes almost tail recursive.
%
isort3(L) -> isort3([],L).

isort3(Su,   []) -> Su;
isort3(Su,[I|L]) -> isort3(insert2([],I,Su),L).

% By replacing the call `isort3(insert2([],I,Su),L)' by a call to a
% new insertion function, the call to sort can be delayed until the
% insertion is done. This is not a tail form yet, since the calls to
% isort4 contains either a call to rev/1 or rev_join/2.
%
isort4(L) -> isort4([],L).

isort4(Su,[]) -> Su;
isort4(Su, L) -> ins_sort4([],Su,L).

ins_sort4(Sd,    [],[I|L])            -> isort4(rev([I|Sd]),L);
ins_sort4(Sd,[J|Su],[I|L]) when I > J -> ins_sort4([J|Sd],Su,[I|L]);
ins_sort4(Sd,    Su,[I|L])         -> isort4(rev_join(Sd,[I|Su]),L).
    
% Let us replace the call to rev/1 by a call to rev_join/2, so the
% two non-tail calls become alike:
%
isort5(L) -> isort5([],L).

isort5(Su,[]) -> Su;
isort5(Su, L) -> ins_sort5([],Su,L).

ins_sort5(Sd,    [],[I|L])            -> isort5(rev_join(Sd,[I]),L);
ins_sort5(Sd,[J|Su],[I|L]) when I > J -> ins_sort5([J|Sd],Su,[I|L]);
ins_sort5(Sd,    Su,[I|L])         -> isort5(rev_join(Sd,[I|Su]),L).

% By replacing the call to `isort5(rev_join(...))' by a new function
% call `rev_app_sort6(...)', the call to sort can be done at the end,
% we get a tail form.
%
isort6(L) -> isort6([],L).

isort6(Su,[]) -> Su;
isort6(Su, L) -> ins_sort6([],Su,L).

ins_sort6(Sd,    [],[I|L])            -> rev_app_sort6(Sd,[I],L);
ins_sort6(Sd,[J|Su],[I|L]) when I > J -> ins_sort6([J|Sd],Su,[I|L]);
ins_sort6(Sd,    Su,[I|L])            -> rev_app_sort6(Sd,[I|Su],L).

rev_app_sort6(    [],Su,L) -> isort6(Su,L);
rev_app_sort6([I|Sd],Su,L) -> rev_app_sort6(Sd,[I|Su],L).

% Why isort6/1 does not call directly ins_sort6/3? The only problem is
% the case when L=[], which matches no head in ins_sort6/3. Therefore,
% let us add this case and call directly. This version is shorter.
%
% The maximum number of steps to compute `isort7(L)' occurs when the
% recursive call is made as much as possible. Since it takes place in
% the body of clause 3 of isort7/3, whose guard is `I > J', this
% implies that the worst case happens when the items in `L' are
% already sorted in increasing order. Under this circumstance, the
% number of steps is computed as follows.
%
% Let S(n) be the number of steps to compute `isort7(L)' with "n"
% being len(L); let R(d,u,n) be the number of steps to compute
% `isort7(Sd,Su,L)' with d being len(Sd), u being len(Su) and n being
% len(L); finally, let S(d,u,n) be the number of steps to compute
% `rev_app_sort7(Sd,Su,L) with the same convention as for S(d,u,n).
%
% The examination of the clauses lead immediately to the equations
%
% Clause 1 of isort7/1 => S(n) = 1 + R(0,0,n)
% Clause 1 of isort7/3 => R(0,u,0) = 1
% Clause 2 of isort7/3 => R(d,0,n+1) = 1 + T(d,1,n)
% Clause 3 of isort7/3 => R(d,u+1,n) = 1 + R(d+1,u,n) with n > 0
% Clause 4 of isort7/3 is never used.
% Clause 1 of rev_app_sort7/3 => T(0,u,n) = 1 + R(0,u,n)
% Clause 2 of rev_app_sort7/3 => T(d+1,u,n) = 1 + T(d,u+1,n)
%
% It is easier to start with `T(d,u,n)'. Indeed,
%
% T(0,u,n) = 1 + R(0,u,n)
% T(1,u,n) = 1 + T(0,u+1,n)   = 2 + R(0,u+1,n)
% T(2,u,n) = 1 + T(1,u+1,n)   = 3 + R(0,u+2,n)
% .............................................
% T(d,u,n) = 1 + T(d-1,u+1,n) = d + 1 + R(0,u+d,n)
%
% In particular, T(d,1,n) = d + 1 + R(0,d+1,n).
%
% We can now express `R' in terms of itself alone:
% R(d,0,n+1) = 1 + 1 + d + R(0,d+1,n)
%            = 2 + d + R(0,d+1,n)
% So:
%
% R(0,u,0) = 1
% R(d,0,n) = 2 + d + R(0,d+1,n-1) with n > 0
% R(d,u,n) = 1 + R(d+1,u-1,n)     with n > 0, u > 0
%
% R(d,u,n) = 1 + R(d+1,u-1,n)     with n > 0, u > 0
%          = 2 + R(d+2,u-2,n)
%          = ...
%          = u + R(d+u,0,n)                  
%          = u + (2 + d+u + R(0,d+u+1,n-1))
%          = 2 + d + 2u + R(0,d+u+1,n-1)
%
% So we have
% 
% R(0,u,0) = 1
% R(d,0,n) = 2 + d + R(0,d+1,n-1)        with n > 0
% R(d,u,n) = 2 + d + 2u + R(0,d+u+1,n-1) with n > 0, u > 0
%
% When u=0 in the third equation, we find the second, so we don't need
% the second:
%
% R(0,u,0) = 1
% R(d,u,n) = d + 2(u+1) + R(0,d+u+1,n-1) with n > 0
% 
% Let us take d=0:
%
% R(0,u,0) = 1
% R(0,u,n) = 2(u+1) + R(0,u+1,n-1) with n > 0
%
% This definition is complete so we can let R'(u,n) = R(0,u,n):
%
% R'(u,0) = 1
% R'(u,n) = 2(u+1) + R'(u+1,n-1) with n > 0
% 
% That is
%
% R'(u,0) = 1
% R'(u,1) = 2(u+1) + R'(u+1,0) = 2(u+1) + 1
% R'(u,2) = 2(u+1) + R'(u+1,1) = 2(u+1) + 2(u+2) + 1
% R'(u,3) = 2(u+1) + R'(u+1,2) = 2(u+1) + 2(u+2) + 2(u+3) + 1
% .............................................................
% R'(u,n) = 2(n*u + 1 + 2 + ... + n) + 1
%         = 2(n*u + n(n+1)/2) + 1
%         = 2n*u + n(n+1) + 1
%
% Then
%
% R'(0,n) = n^2 + n + 1
%
% We wanted
%
% S(n) = 1 + R(0,0,n) = 1 + R'(0,n) = 1 + n^2 + n + 1
%      = n^2 + n + 2.
%
% The maximum number of steps to compute `isort7(L)' is len(L)^2 +
% len(L) + 2.
%
% A heuristic methods could alternatively be used. Just implement in
% Erlang the functions computing the maximum number of steps:
%
% s(N) -> 1 + r(0,0,N).
% r(0,U,0) -> 1.
% r(D,0,N) when N > 0 -> 1 + t(S,1,N-1);
% r(D,U,N) when U > 0,N > 0 -> 1 + r(D+1,U-1,N).
% t(0,U,N) -> 1 + r(0,U,N);
% t(D,U,N) when D > 0 -> 1 + t(D-1,U+1,N).
%
% Then, after trying several values, make a graphic and guess that
% s(N) is a parabola, i.e., s(N) = a*N^2 + b*N + c, for some constants
% `a', `b' and `c'.
%
% By computing three values, we can determine the constants by
% Gaussian elimination.
%
isort7(L) -> isort7([],[],L).

isort7([],    Su,   [])            -> Su;
isort7(Sd,    [],[I|L])            -> rev_app_sort7(Sd,[I],L);    
isort7(Sd,[J|Su],[I|L]) when I > J -> isort7([J|Sd],Su,[I|L]);
isort7(Sd,    Su,[I|L])            -> rev_app_sort7(Sd,[I|Su],L).

rev_app_sort7(    [],Su,L) -> isort7([],Su,L);
rev_app_sort7([I|Sd],Su,L) -> rev_app_sort7(Sd,[I|Su],L).

% Insertion sort for any data type with a comparison function
%
% Not in tail form.
%
isort_g(   [],   _) -> [];
isort_g([I|L],Comp) -> insert_g(I,isort_g(L,Comp),Comp).

insert_g(I,    [],   _) -> [I];
insert_g(I,[J|Su],Comp) ->
  case Comp(I,J) of
    true  -> [I,J|Su];
    false -> [J|insert_g(I,Su,Comp)]
  end.

% Increasing and decreasing sort (instanciation of the comparison)
% They are stable because the comparison includes equality.
%
isort_up(L) -> isort_g(L, fun (X,Y) -> X =< Y end).

isort_down(L) -> isort_g(L, fun (X,Y) -> X >= Y end).

%=====================================================================
% Sorting a deck of cards by insertion
%
% A card is defined by a pair {F,S} where `F' is the face and `S' is
% the suit. The face is any value amongst the increasingly ordered
% series "ace, 2, 3, 4, 5, 6, 7, 8, 9, jack, queen, king." The
% suite is any value amongst the increasingly ordered series "club,
% diamond, heart, spade".
%
% A card is smaller than another if the suit of the former comes
% before the suit of the latter. If the suits are the same, then faces
% are compared.
%
lt_card(         Card,         Card) -> true;
lt_card({Face1, Suit},{Face2, Suit}) -> lt_face(Face1,Face2);
lt_card({    _,Suit1},{    _,Suit2}) -> lt_suit(Suit1,Suit2).

% lt_face/2 is never called with two equal arguments.
%
% The range of the numerical faces (2,3,4,5,6,7,8,9) is not checked
% for the sake of simplicity.
%
lt_face( ace,    _) -> true;
lt_face(   _,  ace) -> false;
lt_face(   _, king) -> true;
lt_face(jack,queen) -> true;
lt_face(   N,    P) when is_integer(N), is_atom(P)
                    -> true;
lt_face(   N,    M) when is_integer(N), is_integer(M)
                    -> N < M;
lt_face(   _,    _) -> false.

% lt_suit/2 is never called with two equal arguments.
%
% The kind of suit is not checked for the sake of simplicity.
%
lt_suit(   club,    _) -> true;
lt_suit(diamond,heart) -> true;
lt_suit(diamond,spade) -> true;
lt_suit(  heart,spade) -> true;
lt_suit(      _,    _) -> false.

% Sorting the deck
%
sort_deck(L) -> isort_g(L,fun lt_card/2).    

%=====================================================================
% Sorting on keys when the input is a mapping from keys to contents.
%
% Not in tail form
%
isort_kv1(          [],   _) -> [];
isort_kv1([KeyVal|Map],Comp) ->
  insert_kv1(KeyVal,isort_kv1(Map,Comp),Comp).

insert_kv1(KeyVal,[],_) -> [KeyVal];
insert_kv1({Key1,Val1},[{Key2,Val2}|Map],Comp) ->
  case Comp(Key1,Key2) of
    true  -> [{Key1,Val1},{Key2,Val2}|Map];
    false -> [{Key2,Val2} | insert_kv1({Key1,Val1},Map,Comp)]
  end.

% NOT Tail form!
%
isort_kv(L,Comp) -> isort_kv([],[],L,Comp).

isort_kv([],    Su,   [],    _) -> Su;
isort_kv(Sd,    [],[I|L], Comp) -> isort_kv_aux(Sd,[I],L,Comp);
isort_kv(Sd,[J|Su],[I|L], Comp) ->
  case Comp(I,J) of
    true  -> isort_kv_aux(Sd,[I,J|Su],L,Comp);
    false -> isort_kv([J|Sd],Su,[I|L],Comp)
  end.

isort_kv_aux(    [],Su,L,Comp) -> isort_kv([],Su,L,Comp);
isort_kv_aux([I|Sd],Su,L,Comp) -> isort_kv_aux(Sd,[I|Su],L,Comp).

%=====================================================================
% Two-way insertion to sort integers increasingly
% 
% In tail form.
%
isort_2w(L) -> isort_2w([],[],L).

isort_2w(   [],    B,   [])            -> B;
isort_2w([I|A],    B,   [])            -> isort_2w(    A,[I|B],   []);
isort_2w(    A,[J|B],[K|C]) when J < K -> isort_2w([J|A],    B,[K|C]);
% alternative following body: isort_2w([K],B,C)
isort_2w(   [],    B,[K|C])            -> isort_2w(   [],[K|B],    C);
isort_2w([I|A],    B,[K|C]) when K < I -> isort_2w(    A,[I|B],[K|C]);
isort_2w(    A,    B,[K|C])            -> isort_2w([K|A],    B,    C).

%=====================================================================
% Sorting increasingly by insertion based upon the list lengths.
%
% Stable sort.

% Not in tail form.
%
len_sort1(L,  up)-> strip1(isort_kv(tag1(L),fun (M,N) -> M =< N end));
len_sort1(L,down)-> strip1(isort_kv(tag1(L),fun (M,N) -> N =< M end)).

tag1(       []) -> [];
tag1([L|Lists]) -> [{len(L),L}|tag1(Lists)].

strip1(         []) -> [];
strip1([{_,L}|Map]) -> [L|strip1(Map)].

% Function len_sort1/2 is not in tail form due to the fact that tag1/1
% and strip1/1 are not in tail form and because of the embedded calls
% in `strip1(isort_kv(tag1(...),..))'.
% It is possible to obtain a tail form by transforming the program
% step by step.
%
% First, let us convert to tail form strip1/1.
%
len_sort2(L,  up) ->
  strip2([],isort_kv(tag1(L),fun (M,N) -> M =< N end));
len_sort2(L,down) ->
  strip2([],isort_kv(tag1(L),fun (M,N) -> N =< M end)).

strip2(A,         []) -> rev(A);
strip2(A,[{_,L}|Map]) -> strip2([L|A],Map).

% Then let us add an accumulating argument to tag1/1.
%
len_sort3(L,  up) ->
  strip2([],isort_kv(tag2([],L),fun (M,N) -> M =< N end));
len_sort3(L,down) ->
  strip2([],isort_kv(tag2([],L),fun (M,N) -> N =< M end)).

tag2(A,       []) -> rev(A);
tag2(A,[L|Lists]) -> tag2([{len(L),L}|A],Lists).

% Let us transform tag2/2 into tail form by defining a function that
% will perform len/1 and, when done, call the new tag2/2. So, an
% accumlating argument must be added to compute the length for each
% list and another to keep the original list (to be used after its
% length has been computed).
%
len_sort4(L,  up) ->
  strip2([],isort_kv(tag3([],L),fun (M,N) -> M =< N end));
len_sort4(L,down) ->
  strip2([],isort_kv(tag3([],L),fun (M,N) -> N =< M end)).

tag3(A,       []) -> rev(A);
tag3(A,[L|Lists]) -> tag_len3(0,L,A,[L|Lists]).
    
tag_len3(Len,   [],A,[L|Lists]) -> tag3([{Len,L}|A],Lists);
tag_len3(Len,[_|M],A,    Lists) -> tag_len3(Len+1,M,A,Lists).


% At this point, only the bodies of len_sort4/2 are not in tail
% form. Let us start to replace the embedded call
% `isort_kv(tag3(...),...)' by one call to a new function tag_sort5/3
% that will perform the composition of both functions tag3/2 and
% isort_kv/2.
%
len_sort5(L,  up) ->
  strip2([],tag_sort5([],L,fun (M,N) -> M =< N end));
len_sort5(L,down) ->
  strip2([],tag_sort5([],L,fun (M,N) -> N =< M end)).

tag_sort5(A,       [],Comp) -> isort_kv(rev(A),Comp);
tag_sort5(A,[L|Lists],Comp) -> tag_len5(0,L,A,[L|Lists],Comp).
    
tag_len5(Len,   [],A,[L|Lists],Comp) ->
  tag_sort5([{Len,L}|A],Lists,Comp);
tag_len5(Len,[_|M],A,    Lists,Comp) ->
  tag_len5(Len+1,M,A,Lists,Comp).

% A new embedded call appears in the body of the first clause of
% tag_sort5/3: `isort_kv(rev(A),Comp)'. We have the following
% property:
% 
%   isort_kv(rev(A),Comp) = isort_kv(A,Comp)
%
% So, tag_sort5/3 can be rewritten into tag_sort6/3 this way:
%
len_sort6(L,  up) ->
  strip2([],tag_sort6([],L,fun (M,N) -> M =< N end));
len_sort6(L,down) ->
  strip2([],tag_sort6([],L,fun (M,N) -> N =< M end)).

tag_sort6(A,       [],Comp) -> isort_kv(A,Comp);
tag_sort6(A,[L|Lists],Comp) -> tag_len6(0,L,A,[L|Lists],Comp).

tag_len6(Len,   [],A,[L|Lists],Comp) ->
  tag_sort6([{Len,L}|A],Lists,Comp);
tag_len6(Len,[_|M],A,    Lists,Comp) ->
  tag_len6(Len+1,M,A,Lists,Comp).

% The embedded call `strip2([],tag_sort6(...))' remains. It can be
% rewritten in the same vein as
%
len_sort7(L,  up) ->
  strip_tag_sort7([],L,fun (M,N) -> M =< N end);
len_sort7(L,down) ->
  strip_tag_sort7([],L,fun (M,N) -> N =< M end).

strip_tag_sort7(A,       [],Comp) -> strip2([],isort_kv(A,Comp));
strip_tag_sort7(A,[L|Lists],Comp) -> tag_len7(0,L,A,[L|Lists],Comp).

tag_len7(Len,   [],A,[L|Lists],Comp) ->
  strip_tag_sort7([{Len,L}|A],Lists,Comp);
tag_len7(Len,[_|M],A,    Lists,Comp) ->
  tag_len7(Len+1,M,A,Lists,Comp).

% This is an almost tail form version.
%
% A new embedded call appeared: `strip2([],isort_kv(...))'.
%
len_sort8(L,  up) ->
  strip_tag_sort8([],L,fun (M,N) -> M =< N end);
len_sort8(L,down) ->
  strip_tag_sort8([],L,fun (M,N) -> N =< M end).

strip_tag_sort8(A,       [],Comp) -> strip_sort8(A,Comp);
strip_tag_sort8(A,[L|Lists],Comp) -> tag_len8(0,L,A,[L|Lists],Comp).

tag_len8(Len,   [],A,[L|Lists],Comp) ->
  strip_tag_sort8([{Len,L}|A],Lists,Comp);
tag_len8(Len,[_|M],A,    Lists,Comp) ->
  tag_len8(Len+1,M,A,Lists,Comp).

strip_sort8(L,Comp) -> strip_sort8([],[],L,Comp).

strip_sort8([],    Su,   [],    _) -> strip2([],Su);
strip_sort8(Sd,    [],[I|L], Comp) -> strip_sort8_aux(Sd,[I],L,Comp);
strip_sort8(Sd,[J|Su],[I|L], Comp) ->
  case Comp(I,J) of % not tail form here
    true  -> strip_sort8_aux(Sd,[I,J|Su],L,Comp);
    false -> strip_sort8([J|Sd],Su,[I|L],Comp)
  end.

strip_sort8_aux(    [],Su,L,Comp) -> strip_sort8([],Su,L,Comp);
strip_sort8_aux([I|Sd],Su,L,Comp) -> strip_sort8_aux(Sd,[I|Su],L,Comp).

% In almost tail form plus an optimisation: the list is not wholly
% tagged before being sorted. Instead, each tagged list is inserted as
% it right place in the current ordered tagged output list. This saves
% memory (one copy of the input) and time (a reverse after the tagging
% is not done, which supposes that the comparison in the insertion
% sort is the negation of the original one).
%
len_sort(L,  up) -> ls_aux([],L,fun (M,N) -> N =< M end);
len_sort(L,down) -> ls_aux([],L,fun (M,N) -> M =< N end).

ls_aux(S,       [],   _) -> strip([],S);
ls_aux(S,[L|Lists],Comp) -> tag_ins(0,L,S,[L|Lists],Comp).

strip(A,         []) -> A;
strip(A,[{_,L}|Map]) -> strip([L|A],Map).

tag_ins(Len,   [],S,[L|Lists],Comp) -> ins_ls([],{Len,L},S,Lists,Comp);
tag_ins(Len,[_|M],S,    Lists,Comp) -> tag_ins(Len+1,M,S,Lists,Comp).

ins_ls(Sd,KV,[],Lists,Comp) ->
  ls_rev_app(Sd,[KV],Lists,Comp);
ins_ls(Sd,{K1,V1},[{K2,V2}|Map],Lists,Comp) ->
  case Comp(K1,K2) of  % not tail form here
    true  -> ls_rev_app(Sd,[{K1,V1},{K2,V2}|Map],Lists,Comp);
    false -> ins_ls([{K2,V2}|Sd],{K1,V1},Map,Lists,Comp)
  end.

ls_rev_app(   [],M,Lists,Comp) -> ls_aux(M,Lists,Comp);
ls_rev_app([I|L],M,Lists,Comp) -> ls_rev_app(L,[I|M],Lists,Comp).

%=====================================================================
% Bottom-Up Merge Sort
%
% It is difficult to prove that the delay is in O(n*log(n)). It is
% based on the recurrence equation
%
%  S(n) = S(L(n/2)) + S(U(n/2)) + n,
%
% where L(n/2) = p if n = 2p or n = 2p + 1, and
%       U(n/2) = L(n/2) + 1.
%
% This equation arises in problems which can be divided in two parts
% of exact same size or almost same size, and whose two parts can be
% solved separately and then recombined with a delay of `n' to obtain
% the solution of the initial problem (recursively).
%
m_sort([]) -> [];
m_sort( L) -> merge_all(mk_single(L)).

mk_single(   []) -> [];
mk_single([I|L]) -> [[I]|mk_single(L)].

merge_all([P]) -> P;
merge_all(  L) -> merge_all(merge2by2(L)).

merge2by2([P,Q|S]) -> [merge(P,Q)|merge2by2(S)];
merge2by2(      L) -> L.

merge(   [],      Q)            -> Q;
merge(    P,     [])            -> P;
merge([I|P],Q=[J|_]) when I < J -> [I|merge(P,Q)];
merge(    P,  [J|Q])            -> [J|merge(P,Q)].

% Tail form
%
m_sort_tf(L)                         -> m_sort_tf__(L,[]).
m_sort_tf__([],A)                    -> mappk([],A);
m_sort_tf__( L,A)                    -> mk_single_tf(L,[k57|A]).
mk_single_tf(   [],A)                -> mappk([],A);
mk_single_tf([I|L],A)                -> mk_single_tf(L,[{k1246,[I]}|A]).
merge_all_tf([P],A)                  -> mappk(P,A);
merge_all_tf(  L,A)                  -> merge2by2_tf(L,[k57|A]).
merge2by2_tf([P,Q|S],A)              -> merge_tf(P,Q,[{k3,S}|A]);
merge2by2_tf(      L,A)              -> mappk(L,A).
merge_tf(   [],    Q,A)              -> mappk(Q,A);
merge_tf(    P,   [],A)              -> mappk(P,A);
merge_tf([I|P],Q=[J|_],A) when I < J -> merge_tf(P,Q,[{k1246,I}|A]);
merge_tf(    P,[J|Q],A)              -> merge_tf(P,Q,[{k1246,J}|A]).
mappk(V,[k57|A])                     -> merge_all_tf(V,A);
mappk(V,[{k3,S}|A])                  -> merge2by2_tf(S,[{k1246,V}|A]);
mappk(V,[{k1246,J}|A])               -> mappk([J|V],A);
mappk(V,[])                          -> V.

% Continuation-Passing Style (thus tail form)
%
m_sort_k(L) -> m_sort_k(L,fun(V) -> V end).

m_sort_k([],K) -> K([]);
m_sort_k( L,K) -> mk_single_k(L,fun(V) -> merge_all_k(V,K) end).

mk_single_k(   [],K) -> K([]);
mk_single_k([I|L],K) -> mk_single_k(L,fun(V) -> K([[I]|V]) end).

merge_all_k([P],K) -> K(P);
merge_all_k(  L,K) -> merge2by2_k(L,fun(V) -> merge_all_k(V,K) end).

merge2by2_k([P,Q|S],K) ->
  merge_k(P,Q,fun(V) -> merge2by2_k(S,fun(W) -> K([V|W]) end) end);
merge2by2_k(      L,K) -> K(L).

merge_k(   [],      Q,K)            -> K(Q);
merge_k(    P,     [],K)            -> K(P);
merge_k([I|P],Q=[J|_],K) when I < J -> merge_k(P,Q,fun(V) -> K([I|V]) end);
merge_k(    P,  [J|Q],K)            -> merge_k(P,Q,fun(V) -> K([J|V]) end).

%=====================================================================
% Factorial

% Not tail form.
%
fact(0)            -> 1;
fact(N) when N > 0 -> N * fact(N-1).

% Tail form using an ad hoc accumulator (not the general method)
%
fact_tf1(N) when N >= 0 -> fact_tf1(N,1).
fact_tf1(0,A)           -> A;
fact_tf1(N,A)           -> fact_tf1(N-1,N*A).

% Tail form using the general method with accumulator
%
fact_tf2(N) when N >= 0 -> fact_tf2(N,[]).
fact_tf2(0,A)           -> appk1(1,A);
fact_tf2(N,A)           -> fact_tf2(N-1,[{k1,N}|A]).
appk1(V,[{k1,N}|A])     -> appk1(N*V,A);
appk1(V,        [])     -> V.

% Tail form in Continuation-Passing Style.
%
fact1(N) when N >= 0 -> fact1(N,fun (X) -> X end).
fact1(0,K)           -> K(1);
fact1(N,K)           -> fact1(N-1,fun (M) -> K(N*M) end).

%=====================================================================
% Subtractions of integers in a list (right-associative)
%
diff([M,N]) -> M - N;
diff([M|L]) -> M - diff(L).

%=====================================================================
% `prod(L)' is the product of the integers in the list `L'.

% Not in tail form.
%
prod1(  [N]) -> N;
prod1([N|L]) -> N * prod1(L).

% In tail form
%
prod([N|L]) -> prod(N,L).

prod(A,   []) -> A;
prod(A,[N|L]) -> prod(N*A,L).
        
% In tail form
%
prod2(L) -> prod2(1,L).

prod2(A,  [N]) -> N*A;
prod2(A,[N|L]) -> prod2(N*A,L).

% Tail form and optimising for zero
%
prod3([0|_]) -> 0;
prod3([N|L]) -> prod3(N,L).

prod3(_,[0|_]) -> 0;
prod3(A,  [N]) -> N*A;
prod3(A,[N|L]) -> prod3(N*A,L).

%=====================================================================
% `rotate(L,N)' is the list containing the items of `L' such that the
% len(L) - N last items of `L' come first (in the original order),
% followed by the N first items of `L' (in the original order).
%

% This is version is not in tail form
%
rotate([],_)            -> [];
rotate( L,N) when N < 0 -> Len = len(L), rotate([],L,Len + N rem Len);
rotate( L,N)            -> rotate([],L,N rem len(L)).

rotate([],    L,0) -> L;
rotate( A,    L,0) -> join(L,rev(A));
rotate( A,[I|L],N) -> rotate([I|A],L,N-1).

% This version is in tail form.
%
rotate1(L,N) -> rot1(L,N,L,0).

rot1(L,N,   [],Len) when N < 0 -> rot2([],L,Len + N rem Len);
rot1(L,N,   [],Len)            -> rot2([],L,N rem Len);
rot1(L,N,[_|C],Len)            -> rot1(L,N,C,Len+1).

rot2([],    L,0) -> L;
rot2( A,    L,0) -> rot3(L,A,[]);
rot2( A,[I|L],N) -> rot2([I|A],L,N-1).

rot3(L,   [],B) -> join(L,B);
rot3(L,[I|A],B) -> rot3(L,A,[I|B]).

% Reusing split/2.
%
rotate2(L,N) when N < 0 ->
  Len = len(L), rotate2_(L,(Len + N rem Len) rem Len);
rotate2(L,N) ->
  rotate2_(L,N rem len(L)).

rotate2_(L,0) -> L;
rotate2_(L,N) ->
   {Pre,Suf} = split(L,N), join(Suf,Pre).

%=====================================================================
% `dupli(L)' is the list containing all the items of list `L' in the
% same order but consecutively duplicated.

% This version is in tail form.
%
% The number of user-defined steps is 2*len(L)+3:
%     1 step  by clause 1 of dupli/1
% +   len(L) steps by clause 2 of dupli/2
% + len(L)+2 steps to compute the body of clause 1 of dupli/2
%
% The number of pushes is 3*len(L):
%   2*len(L) by clause 2 of dupli/2
% +   len(L) to compute the body of clause 1 of dupli/2
%
dupli(L) -> dupli([],L).

dupli(A,   []) -> rev(A);
dupli(A,[I|L]) -> dupli([I,I|A],L).

% This version is not in tail form.
%
% The number of steps is len(L) + 1.
% The number of pushes is 2*len(L).
%
dupli1(   []) -> [];
dupli1([I|L]) -> [I,I|dupli(L)].

%=====================================================================
% `repli(N,L)' is the list containing the items of list `L' in the
% same order but successively duplicated `N' times (in particular,
% `repli(0,L)' is the empty list).

% This version is not in tail form.
%
% Additionally to the list accumulator, `N' must be duplicated in
% order to remember the original value and reload it when an item has
% been duplicated the number of expected times, and the following (if
% any) have to be processed `N' times also.
%
% The number of user-defined steps is (N+2)*len(L)+4:
%          1 step  by clause 1 of repli2/2
% + len(L)+2 steps to compute `rev(L)' in the body of clause 1 of
%                  repli2/2
% + N*len(L) steps by clause 3 of repli2/4
% +   len(L) steps by clause 2 of repli2/4
% +        1 step  by clause 1 of repli2/4
%
% The number of pushes is 3*len(L):
%     len(L) pushes to compute `rev(L)' in the body of clause 1 of
%                   repli2/2
% + 2*len(L) pushes by clause 3 of repli2/4 (or len(L) if the compiler
%                   is smart)
%
repli2(N,L) when N >=0 -> repli2([],N,N,rev(L)).

repli2(A,N,N,   []) -> A;
repli2(A,0,N,[_|L]) -> repli2(A,N,N,L);
repli2(A,P,N,[I|L]) -> repli2([I|A],P-1,N,[I|L]).

% The same but in tail form:
%
repli(N,L) when N >= 0 -> repli1([],N,L).

repli1(A,N,   []) -> repli2([],N,N,A);  % See above.
repli1(A,N,[I|L]) -> repli1([I|A],N,L).

% A version in tail form which is slower because the list reversal is
% done at the end instead of the beginning.
%
% The number of steps is (2*N+1)*len(L)+4:
%          1 step  by clause 1 of repli3/2
% + N*len(L) steps by clause 3 of repli3/4
% +   len(L) steps by clause 2 of repli3/4
% +        1 step  by clause 1 of repli3/4
% + N*len(L)+2 steps to compute the body of clause 1 of repli3/4
%
% The number of pushes is (N+2)*len(L):
%   2*len(L) by clause 3 of repli3/4
% + N*len(L) to compute `rev(A)' in the body of clause 1 of repli3/4
% 
repli3(N,L) when N >= 0 -> repli3([],N,N,L).

repli3(A,N,N,   []) -> rev(A);
repli3(A,0,N,[_|L]) -> repli3(A,N,N,L);
repli3(A,P,N,[I|L]) -> repli3([I|A],P-1,N,[I|L]).

%=====================================================================
% foldl(F,A,[I1,I2,...,In]) = F(In,F(...F(I2,F(I1,A))...))
% foldl(F,A,[]) = A
%
% Tail form.
%
foldl(_,A,   []) -> A;
foldl(F,A,[I|L]) -> foldl(F,F(I,A),L).

%=====================================================================
% foldr(F,A,[I1,I2,...,In]) = F(I1,F(I2,...,F(In,A))...)
% foldr(F,A,[]) = A
%
% Not in tail form.
%
foldr2(_,A,   []) -> A;
foldr2(F,A,[I|L]) -> F(I,foldr2(F,A,L)).

% Not in tail form.
%
foldr1(F,A,L) -> foldl(F,A,rev(L)).

% In tail form
%
foldr(F,A,L) -> foldr([],F,A,L).

foldr(   [],_,A,   []) -> A;
foldr([I|B],F,A,   []) -> foldr(    B,F,F(I,A),[]);
foldr(    B,F,A,[I|L]) -> foldr([I|B],F,     A, L).

%=====================================================================
% map(F,[I1,I2,...,In]) = [F(I1),F(I2),...,F(In)]
%
% Not in tail form.
%
map1(_,   []) -> [];
map1(F,[I|L]) -> [F(I)|map1(F,L)].

% Almost in tail form.
%
map(F,L) -> map_aux([],F,L).

map_aux(A,_,   []) -> hw:rev(A);
map_aux(A,F,[I|L]) -> map_aux([F(I)|A],F,L).

%=====================================================================
% Some redefinitions by means of folds.
%
push(I,L) -> [I|L].
    
len2(L) -> foldl(fun (_,A) -> A+1 end,0,L).

join3(L,M) -> foldr(fun push/2,M,L).
                              
rev_join1(L,M) -> foldl(fun push/2,M,L).

rev2(L) -> foldl(fun push/2,[],L).

map2(F,L) -> foldr(fun (I,A) -> [F(I)|A] end,[],L).

compress2(L) -> foldr(fun comp2_aux/2,[],L).

comp2_aux(I,[I|A]) -> [I|A];
comp2_aux(I,    A) -> [I|A].

pack2(L) -> foldr(fun pack2_aux/2,[],L).

pack2_aux(I,[[I|Is]|A]) -> [[I,I|Is]|A];
pack2_aux(I,         A) ->      [[I]|A].

encode2(L) -> foldr(fun encode2_aux/2,[],L).

encode2_aux(I,[{N,I}|A]) -> [{N+1,I}|A];
encode2_aux(I,        A) ->   [{1,I}|A].

% encode_opt/1 cannot be defined by means of foldr/3 because it
% requires the knowledge of two consecutive items in the input
% list. Instead we must define it based upon encode_opt1/1.

encode_opt2(L) -> foldr(fun encode_opt2_aux/2,[],L).

encode_opt2_aux(I,[{N,I}|A]) -> [{N+1,I}|A];
encode_opt2_aux(J,[{1,I}|A]) -> [{1,J},I|A];
encode_opt2_aux(I,        A) ->   [{1,I}|A].


dupli2(L) -> foldr(fun (I,A) -> [I,I|A] end,[],L).


repli4(N,L) when N >= 0 ->
  {Rep,_,_} = foldr(fun repli4_aux/2,{[],N,N},L), Rep.
                             
repli4_aux(_,{A,0,N}) -> {A,N,N};
repli4_aux(I,{A,P,N}) -> repli4_aux(I,{[I|A],P-1,N}).


isort8(L) -> foldl(fun insert8/2,[],L).

insert8(I,[J|Su]) when I > J -> [J|insert8(I,Su)];
insert8(I,    Su)            -> [I|Su].

%=====================================================================
% Sum and products of integers in a list
%
sum([P|L]) -> foldl(fun (N,A) -> N+A end,P,L).
    
% The product of items in a list cannot be optimised if computed by
% means of a fold because the computation is supposed to stop in case
% of a zero, but this is not possible: the fold must exhaust the whole
% input list.
%
prod4([P|L]) -> foldl(fun (N,A) -> N*A end,P,L).

%=====================================================================
% Filtering a list

% Not tail form.
%
filter1(_,   []) -> [];
filter1(P,[I|L]) -> case P(I) of
                      true  -> [I|filter1(P,L)];
                      false -> filter1(P,L)
                    end.

% Tail form.
%
filter(P,L) -> filter(P,L,[]).

filter(_,   [],A) -> rev(A);
filter(P,[I|L],A) -> case P(I) of
                       true  -> filter(P,L,[I|A]);
                       false -> filter(P,L,    A)
                     end.

%=====================================================================
% Zipping two lists together

% Not in tail form.
%
zip1(   [],   []) -> [];
zip1([I|L],[J|M]) -> [{I,J}|zip1(L,M)].

% In tail form.
%
zip(L,M) -> zip([],L,M).

zip(A,   [],   []) -> rev(A);
zip(A,[I|L],[J|M]) -> zip([{I,J}|A],L,M).

%=====================================================================
% P12
%
decode(L) -> decode([],L).

decode(A,       []) -> rev(A);
decode(A,[{1,I}|L]) -> decode([I|A],L);
decode(A,[{N,I}|L]) -> decode([I|A],[{N-1,I}|L]).

%=====================================================================
% Removing items in a list at odd indexes
%
del_odd(L) -> del_odd([],L).

del_odd(A,     []) -> rev(A);
del_odd(A,    [_]) -> rev(A);
del_odd(A,[_,I|L]) -> del_odd([I|A],L).

del_odd1(     []) -> [];
del_odd1(    [_]) -> [];
del_odd1([_,I|L]) -> [I|del_odd1(L)].

%=====================================================================
% Shuffle two decks of cards
%

%
shuffle(   [],Q) -> Q;
shuffle([I|P],Q) -> [I|shuffle(Q,P)].

% Perfect shuffle (not tail form)
%
shuffle_per(   [],[]) -> [];
shuffle_per([I|L],[J|M]) -> [I,J|shuffle_per(L,M)].
    
% Non-perfect shuffle (not tail form)
%
shuffle1(   [],    L) -> L;
shuffle1(    L,   []) -> L;
shuffle1([I|L],[J|M]) -> [I,J|shuffle1(L,M)].

% Non-perfect shuffle (tail form, with rev_join/2)
%
shuffle2(L,M) -> shuffle3([],L,M).

shuffle3(A,   [],    M) -> rev_join(A,M);
shuffle3(A,    L,   []) -> rev_join(A,L);     % or shuffle3(A,[],L)
shuffle3(A,[I|L],[J|M]) -> shuffle3([J,I|A],L,M).

% Non-perfect shuffle (tail form, standalone)
%     
shuffle4(L,M) -> shuffle4([],L,M).

shuffle4(   [],   [],    M) -> M;
shuffle4([I|A],   [],    M) -> shuffle4(      A,[],[I|M]);
shuffle4(    A,    L,   []) -> shuffle4(      A,[],    L);
shuffle4(    A,[I|L],[J|M]) -> shuffle4([J,I|A], L,    M).
                 
shuffle_qua(   [], M) -> M;
shuffle_qua(    L,[]) -> L;
shuffle_qua([I|L], M) -> [I|shuffle_qua(M,L)].

shuffle_qua2(   [],L) -> L;
shuffle_qua2([I|L],M) -> [I|shuffle_qua2(M,L)].

shuffle_quin(L,M) -> shuffle_quin_aux([],L,M).

shuffle_quin_aux(A,   [], M) -> rev_join(A,M);
shuffle_quin_aux(A,    L,[]) -> rev_join(A,L);
shuffle_quin_aux(A,[I|L], M) -> shuffle_quin_aux([I|A],M,L).

shuffle_sex(L,M) -> shuffle_sex_aux([],L,M).

shuffle_sex_aux(A,   [],M) -> rev_join(A,M);
shuffle_sex_aux(A,[I|L],M) -> shuffle_sex_aux([I|A],M,L).

% Shuffle three decks of cards
%
shuffle_3d([],[],   []) -> [];
shuffle_3d( P, Q,[I|R]) -> [I|shuffle_3d(Q,R,P)];
shuffle_3d( P, Q,   []) -> shuffle_3d(Q,[],P).

shuffle_3d1(P,Q,R) -> shuffle3d1([],P,Q,R).

shuffle3d1(A,[],[],   []) -> rev(A);
shuffle3d1(A, P, Q,[I|R]) -> shuffle3d1([I|A],Q,R,P);
shuffle3d1(A, P, Q,   []) -> shuffle3d1(A,Q,[],P).
    
%=====================================================================
% Merging two sorted (<) lists 

% Not in tail form.
%
merge1([],L2) ->
  L2;
merge1(L1,[]) ->
  L1;
merge1([I1|L1],[I2|L2]) when I1 =< I2 ->
  [I1|merge1(L1,[I2|L2])];
merge1([I1|L1],[I2|L2]) ->
  [I2|merge1([I1|L1],L2)].

% In tail form.
%
merge_bis(L1,L2) -> merge(L1,L2,[]).

merge([],L2,A) ->
  hw:rev_join(A,L2);
merge(L1,[],A) ->
  hw:rev_join(A,L1);
merge([I1|L1],[I2|L2],A) when I1 < I2 ->
  merge(L1,[I2|L2],[I1|A]);
merge([I1|L1],[I2|L2],A) ->
  merge([I1|L1],L2,[I2|A]).

%=====================================================================
% mapfold combines the operations of map/2 and foldl/3 into one
% pass. An example, summing the elements in a list and double them at
% the same time:
% 
%> mapfoldl(fun(X, Sum) -> {2*X, X+Sum} end, 0, [1,2,3,4,5]).
%{[2,4,6,8,10],15}
%
mapfoldl(Fun, Acc, List) when is_function(Fun,2), is_list(List) ->
 NewFun = fun (Item,{Map,Cumul}) -> 
            {NewItem,NewCumul} = Fun(Item,Cumul),
            {[NewItem|Map],NewCumul}
          end,
 {M,F} = foldl(NewFun, {[],Acc}, List),
 {rev(M),F}.

%=====================================================================
% P03
%
nth([Item|Items],Index) when Index > 0 ->
     nth__([Item|Items],Index).

nth__([Item|_],1) ->
    Item;
nth__([_|Items],Index) ->
    nth__(Items,Index-1).

%=====================================================================
%
slice(List,From,To) when is_list(List), 0 < From, From =< To ->
    slice([],List,From,To);
slice(List,From,To) when is_list(List), 0 < To, To =< From ->
    slice([],List,To,From).
slice(Acc,_,1,0) ->
    rev(Acc);
slice(Acc,[Item|Items],1,To) ->
    slice([Item|Acc],Items,1,To-1);
slice(Acc,[_|Items],From,To) ->
    slice(Acc,Items,From-1,To-1).

%=====================================================================
%
remove_at(List,Index) when is_list(List), Index > 0 ->
    remove_at([],List,Index).

remove_at(RevPref,[_|Items],1) ->
    rev_join(RevPref,Items);
remove_at(RevPref,[Item|Items],Index) ->
    remove_at([Item|RevPref],Items,Index-1).

%=====================================================================
%
insert_at(Extra,List,Index) when is_list(List), Index > 0 ->
    insert_at([],Extra,List,Index).

insert_at(RevPref,Extra,Suffix,1) ->
    rev_join(RevPref,[Extra|Suffix]);
insert_at(RevPref,Extra,[Item|Items],Index) ->
    insert_at([Item|RevPref],Extra,Items,Index-1).

%=====================================================================
%
range(From,To) when From =< To ->
    range([],From,To).

range(Acc,To,To) ->
    rev([To|Acc]);
range(Acc,From,To) ->
    range([From|Acc],From+1,To).

%=====================================================================
% P23
%
% We shall require that all the items have different indexes.
%
rnd_select(List,Num) when is_list(List), Num > 0 ->
     Len = len(List),
     if Num == Len -> List;
        Num < Len  -> rnd_select([],Len,List,Num)
     end.

rnd_select(Selected,_,_,0) ->
    Selected;
rnd_select(Selected,Len,List,Num) ->
    {Item,NewList} = cut_at(List,random:uniform(Len)),
    rnd_select([Item|Selected],Len-1,NewList,Num-1).

cut_at(List,Index) ->
    cut_at([],List,Index).

cut_at(RevPref,[Item|Items],1) ->
    {Item,rev_join(RevPref,Items)};
cut_at(RevPref,[Item|Items],Index) ->
    cut_at([Item|RevPref],Items,Index-1).

%=====================================================================
% P24
%
lotto(N,Max) -> rnd_select(range(1,Max),N).

%=====================================================================
% P25
%
rnd_permute(List) -> rnd_select(List,len(List)).

%=====================================================================
% P26
%
% All items are considered pairwise different.
% Combinations are all the time implemented as lists.
% There is no sharing of sub-combinations.
% Each combination contains items in the same order as given.
% The resulting list of combinations is not sorted lexicographically
% (the total order is the order of the initial items).
%
combine(_,0)            -> [];
combine(L,N) when N > 0 -> combine([],L,N).

combine(Comb,[],_) ->
    Comb;
combine([],L,1) ->
    map(fun (I) -> [I] end,L);
combine(Comb,[I|L],N) ->
    SubComb = combine([],L,N-1),
    NewComb = foldl(fun (M,A) -> [[I|M]|A] end, Comb, SubComb),
    combine(NewComb,L,N).
                
% Same as before but combinations are sorted lexicographically in
% increasing order.
%
combine1(_,0)            -> [];
combine1(L,N) when N > 0 -> rev(combine1([],L,N)).
 
combine1(Comb,[],_) ->
    Comb;
combine1([],L,1) ->
    foldl(fun (I,A) -> [[I]|A] end,[],L);
combine1(Comb,[I|L],N) ->
    SubComb = combine1([],L,N-1),
    NewComb = foldr(fun (M,A) -> [[I|M]|A] end, Comb, SubComb),
    combine1(NewComb,L,N).

% Combinations are not recomputed.
%
combine2(_,0) ->
    [];
combine2(L,N) when N > 0 ->
    Len = len(L),
    case Len >= N of
      true  -> reorder(combine3(Len-N+1,N,L));
      false -> []
    end.

combine3(_,1,L) ->
    map(fun (I) -> [[I]] end, L);
combine3(Range,Depth,L=[_|Items]) when Depth > 1 ->
    SubComb = combine3(Range,Depth-1,Items),
    {_,_,Comb} = foldl(fun mk_comb/2, {Range,SubComb,[]}, L),
    rev(Comb).

mk_comb(_,A={0,_,_}) ->
    A;
mk_comb(_,A={_,[],_}) ->
    A;
mk_comb(Item,{Range,SubComb=[_|MoreSubComb],OrdComb}) ->
    NewComb = 
        foldl(fun (I,A) -> rev_join(distribute(Item,I),A) end, 
              [], 
              SubComb),
    {Range-1, MoreSubComb, [NewComb|OrdComb]}.

distribute(I,Combs) ->
    Fun = fun (Comb,A) -> [[I|Comb]|A] end,
    foldl(Fun,[],Combs).

reorder(Lists) -> reorder_aux([],rev(Lists)).

reorder_aux(A,       []) -> A;
reorder_aux(A,[L|Lists]) -> reorder_aux(rev_join(L,A),Lists).

%=====================================================================
% `sigma(F,L)' is the sum F(I1)+F(I2)+...+F(In) where L =
% [I1,I2,...,In] and `sigma(F,[])' is undefined.
%

% Not tail form
%
sigma1(_,  [N]) -> N;
sigma1(F,[N|L]) -> F(N) + sigma1(F,L).

% Almost tail form.
%
sigma2(F,[N|L]) -> sigma2(F,[N|L],[]).

sigma2(_,   [],A) -> A;
sigma2(F,[N|L],A) -> sigma2(F,L,F(N)+A).

% Using functionals
%
sigma3(F,[N|L]) -> sum(map(F,[N|L])).
    
% Optimised so that no intermediate list is built.
%
% Note the warning from the Erlang compiler if
%
% sigma(F,[N|L]) -> foldl(fun (N,A) -> F(N)+A end,0,[N|L]).
% 
% Warning: variable 'N' shadowed in 'fun'
%
sigma(F,[I|L]) -> foldl(fun (N,A) -> F(N)+A end,0,[I|L]).
                            
%=====================================================================
% Iteration

% Bounded
%
iter(_,0,X)            -> X;
iter(F,N,X) when N > 0 -> iter(F,N-1,F(X)). % or F(iter(F,N-1,X))

% Unbounded
%
while(F,P,X) -> case P(X) of
                  true  -> X;
                  false -> while(F,P,F(X))
                end.

% Applications
%
power(M,N) when N >= 0 -> iter(fun(P) -> M*P end,N,1).

% F(n) = F(n-1) + F(n-2); F0 = F1 = 1
% f(Fn+1,Fn) = (Fn+2,Fn+1)
%
fib(N) -> {R,_} = iter(fun ({X,Y}) -> {X+Y,X} end,N,{1,0}), R.
