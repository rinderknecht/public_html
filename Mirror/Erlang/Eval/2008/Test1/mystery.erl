-module(mystery).
-export([f/1,g/1]).

f(T) -> f([],[],T).

f(Nodes,[],empty) ->
  Nodes;
f(Nodes,[Left|Forest],empty) ->
  f(Nodes,Forest,Left);
f(Nodes,Forest,{Left,Root,Right}) ->
  f([Root|Nodes],[Left|Forest],Right).

g(T) -> g([],[],T).
    
g(Nodes,[],empty) ->
  Nodes;
g(Nodes,[{Left,Root,empty}|Forest],empty) ->
  g([Root|Nodes],Forest,Left);
g(Nodes,Forest,{Left,Root,Right}) ->
  g(Nodes,[{Left,Root,empty}|Forest],Right).
