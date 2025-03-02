-module(counter).
-export([start/0,incr/1,value_of/1,stop/1]).

start() -> spawn (fun() -> loop(0) end).

% Client
%
incr(Counter) -> Counter ! incr.

value_of(Counter) ->
  Counter ! {self(), value},
  receive
    {Counter,Value} -> Value
  end.

stop(Counter) -> Counter ! stop.

% Server
%
loop(Val) ->
  receive
    incr         -> loop(Val + 1);
    {From,value} -> From ! {self(),Val}, loop(Val);
    stop         -> true;                    
    _            -> loop(Val)
  end.
