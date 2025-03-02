-module(bool).
-export([f/2]).
f(true,true)   -> false;
f(true,false)  -> true;
f(false,true)  -> true;
f(false,false) -> false.
