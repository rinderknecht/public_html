-module(hw).
-compile(export_all).
%-export([find/2,lrm_fst/2,rm_lst/2]).

% Searching a word in a text
%
find(Word,Text) -> find(Word,Text,Word,Text,0).

find(   [],    _,   _,    _,N) -> {factor,N};
find(    _,   [],   _,    _,_) -> absent;
find([I|W],[I|T],Word, Text,N) -> find(W,T,Word,Text,N);
find(    _,    _,Word,[_|T],N) -> find(Word,T,Word,T,N+1).

% Removing the last occurrence of a word in a text. Note how the
% patterns of rm_pref/4 are statically incomplete but are dynamically
% complete, that is, the match failure cannot happen if called through
% lrm_fst/2.
%
lrm_fst(Word,Text) -> rm_pref(Word,Text,Word,Text).

rm_pref(   [],    T,   _,    _) -> T;
rm_pref(    _,   [],   _, Text) -> Text;
rm_pref([I|W],[I|T],Word, Text) -> rm_pref(W,T,Word,Text);
rm_pref(    _,    _,Word,[J|T]) -> [J|rm_pref(Word,T,Word,T)].

% Prefix
%
prefix(   [],    _) -> true;
prefix([I|W],[I|T]) -> prefix(W,T);
prefix(    _,    _) -> false.    

% Slow version.
%
suffix(  [],   _) -> true;
suffix(Word,Text) -> suffix(Word,Text,Word,Text).

suffix(   [],   [],   _,    _) -> true;
suffix([I|W],[I|T],Word, Text) -> suffix(W,T,Word,Text);
suffix([_|_],[_|_],Word,[_|T]) -> suffix(Word,T,Word,T);
suffix(    _,    _,   _,    _) -> false.

% Fast version.
%
suffix_fast(Word,Text) -> prefix(rev(Word),rev(Text)).
    
% Fast filtering out of the last occurrence of some item in a list.
%
rm_lst(_,   []) -> [];
rm_lst(I,[I|L]) -> rm_lst__(I,L,L);
rm_lst(I,[J|L]) -> [J|rm_lst(I,L)].

rm_lst__(_,   [],L) -> L;
rm_lst__(I,[I|_],L) -> [I|rm_lst(I,L)];
rm_lst__(I,[_|P],L) -> rm_lst__(I,P,L).

% Enqueuing
%
enq(I,{  [],   []}) -> {[],[I]};
enq(I,{Rear,Front}) -> {[I|Rear],Front}.

% Dequeuing
%
deq({  [],      [I]}) -> {{[],[]},I};
deq({Rear,      [I]}) -> {{[],rev(Rear)},I};
deq({Rear,[I|Front]}) -> {{Rear,Front},I}.

% List reversal
%
rev(L) -> rev_cat(L,[]).

rev_cat(   [],Q) -> Q;
rev_cat([I|P],Q) -> rev_cat(P,[I|Q]).

% Catenation of lists.
%
cat(   [],Q) -> Q;
cat([I|P],Q) -> [I|cat(P,Q)].

% Splitting of lists.
%
split(    L,0) -> {[],L};
split([I|L],K) -> {P,M}=split(L,K-1), {[I|P],M}. 

% Inorder
%
inorder(T) -> inorder(T,[]).

inorder(empty,P) -> P;
inorder({Root,Left,Right},P) ->
  inorder(Left,[Root|inorder(Right,P)]).
