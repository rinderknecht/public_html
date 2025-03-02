-module(tms).
-compile(export_all).

mrg(   [],T) -> T;
mrg([X|S],T) -> mrg0(X,S,T).

mrg0(X,S,   []) -> [X|S];
mrg0(X,S,[Y|T]) -> case X > Y of
                     true  -> [Y|mrg0(X,S,T)];
                     false -> [X|mrg0(Y,T,S)]
                   end.

tms(   []) -> [];
tms([X|T]) -> tms0(X,T).

tms0(X,   []) -> [X];
tms0(X,[Y|T]) -> cutr([X],[Y|T],T).

cutr(S,T,   []) -> mrg(tms(S),tms(T));
cutr(S,T,[_|U]) -> cutr0(S,T,U).

cutr0(S,T,   []) -> mrg(tms(S),tms(T));
cutr0(S,T,[_|U]) -> cutr1(S,T,U).

cutr1(S,   [],_) -> tms(S);
cutr1(S,[Y|T],U) -> cutr([Y|S],T,U).
