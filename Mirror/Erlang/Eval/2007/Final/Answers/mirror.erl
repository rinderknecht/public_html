-module(mirror).
-compile(export_all).

mirror(nil) ->
    nil;
mirror({Left,Root,Right}) ->
    {mirror(Right),Root,mirror(Left)}.
