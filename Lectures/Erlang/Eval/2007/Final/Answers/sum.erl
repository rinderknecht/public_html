-module(sum).
-compile(export_all).


sum({nil,Root,Right}) ->
    Root + sum(Right);
sum({Left,Root,nil}) ->
    sum(Left) + Root;
sum({Left,Root,Right}) ->
    sum(Left) + Root + sum(Right).

% With one accumulator
%
sum_bis({Left,Root,Right}) ->
    sum_bis({Left,Root,Right},0).
sum_bis(nil,A) ->
    A;
sum_bis({Left, Root, Right},A) ->
    sum_bis(Right, Root + sum_bis(Left,A)).

% Tail recursive version
%
sum_ter({Left,Root,Right}) ->
    sum_ter({Left,Root,Right},0,[]).
sum_ter(nil,Sum,[]) ->
    Sum;
sum_ter(nil,Sum,[Tree|Forest]) ->
    sum_ter(Tree,Sum,Forest);
sum_ter({Left,Root,Right},Sum,Forest) ->
    sum_ter(Left,Root+Sum,[Right|Forest]).
