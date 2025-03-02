-module(bool1).
-export([f/2]).
f(X,X) -> false;
f(_,_) -> true.
