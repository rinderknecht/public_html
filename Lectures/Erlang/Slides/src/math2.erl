-module(math2).
-export([double/1]).

double(X) -> mult(X,2).
mult(X,Y) -> X * Y.
