-module(list1).
-export([len/1]).

len([])    -> 0;
len([_|T]) -> 1 + len(T).
