-module(merge).
-compile(export_all).

% rev_append(List1,List2) reverses the list List1 and appends it to
% List2.
%
rev_append([],Suffix) ->
    Suffix;
rev_append([Item|Prefix],Suffix) ->
    rev_append(Prefix,[Item|Suffix]).

% Merging two sorted (<) lists (not tail-recursive)
%
merge([],List2) ->
    List2;
merge(List1,[]) ->
    List1;
merge([Item1|List1],[Item2|List2]) when Item1 < Item2 ->
    [Item1|merge(List1,[Item2|List2])];
merge([Item1|List1],[Item2|List2]) ->
    [Item2|merge([Item1|List1],List2)].

% Same but with tail recursion
%
merge_bis(List1,List2) when is_list(List1), is_list(List2) ->
    merge(List1,List2,[]).
merge([],List2,Acc) ->
    rev_append(Acc,List2);
merge(List1,[],Acc) ->
    rev_append(Acc,List1);
merge([Item1|List1],[Item2|List2],Acc) when Item1 < Item2 ->
    merge(List1,[Item2|List2],[Item1|Acc]);
merge([Item1|List1],[Item2|List2],Acc) ->
    merge([Item1|List1],List2,[Item2|Acc]).
