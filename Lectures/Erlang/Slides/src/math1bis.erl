-module(math1bis).
-export([fact/1]).

fact(N) -> N * fact(N-1);
fact(0) -> 1.
