-module(hw).
-compile(export_all).

is_BST(T) -> case is_BST__(T) of {_,_} -> true;
                                     X -> X
             end.

is_BST__(ext)                   -> true;
is_BST__({int,Root,Left,Right}) -> 
  case is_BST__(Left) of
    true -> case is_BST__(Right) of    true -> {Root,Root};
              {Rmin,Rmax} when Root =< Rmin -> {Root,Rmax};
                                          _ -> false
            end;
    {Lmin,Lmax} when Lmax =< Root ->
      case is_BST__(Right) of
               true when Lmax =< Root -> {Lmin,Root};
        {Rmin,Rmax} when Root =< Rmin -> {Lmin,Rmax};
                                    _ -> false
      end;
    _ -> false
  end.

% Addition with repetition (to the right)
%
add0(I,ext) -> {I,ext,ext};
add0(I,{int,Root,Left,Right}) when I < Left ->
  {int,Root,add0(I,Left),Right};
add0(I,{int,Root,Left,Right}) ->
  {int,Root,Left,add0(I,Right)}.

% Addition without repetition, but root-to-leaf duplication
%
add1(I,ext) -> {I,ext,ext};
add1(I,T={int,I,_,_}) -> T;
add1(I,{int,Root,Left,Right}) when I < Left ->
  {int,Root,add1(I,Left),Right};
add1(I,{int,Root,Left,Right}) ->
  {int,Root,Left,add1(I,Right)}.

% Addition without repetition and no root-to-leaf duplication
% (first-order, tail-form)
%
add2(I,T) -> add2(I,T,[],T).
    
add2(I,ext,A,_) -> rebuild(A,{I,ext,ext});
add2(I,{int,I,_,_},_,T) -> T;
add2(I,{int,Root,Left,Right},T) when I < Left ->
  add2(I,Left,[{left,Root,Right}|A],T);
add2(I,{int,Root,Left,Right}) ->
  add2(I,Right,[{right,Root,Left}|A],T).

rebuild(                   [],T) -> T;
rebuild([{left,Root,Right}|A],T) -> rebuild(A,{int,Root,T,Right});
rebuild([{right,Root,Left}|A],T) -> rebuild(A,{int,Root,Left,T}).
