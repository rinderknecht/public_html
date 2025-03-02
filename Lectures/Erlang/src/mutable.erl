-module(mutable).
-compile(export_all).

new_cell(X) -> spawn(fun() -> cell(X) end).
cell(Value) ->
   receive
      {set, NewValue} -> cell(NewValue);
      {get, Pid}      -> Pid!{return, Value}, cell(Value);
      {dispose}       -> {}
   end.
set_cell(Cell, NewValue) -> Cell!{set, NewValue}.
get_cell(Cell) ->
   Cell!{get, self()},
   receive
      {return, Value} -> Value
   end.
dispose_cell(Cell) -> Cell!{dispose}.
