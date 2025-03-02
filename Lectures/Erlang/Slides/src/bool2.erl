-module(bool2).
-export([f/2]).
f(X,X) -> false;
f(X,Y) -> true.
    
