-module(calder_trees).
-compile(export_all).

% Checking whether a given binary tree is a Calder tree
%
equi({Left,Right}) -> 
  case equi(Left) of
    true  -> case equi(Right) of
               true  -> weight(Left) == weight(Right);
               false -> false
             end;
    false -> false
  end;
equi(_) -> true.

weight({Left,Right}) -> weight(Left) + weight(Right);
weight(           N) -> N.

%%% Optimised

equi_opt(T) ->
  case eqw(T) of
    {_,true} -> true;
       false -> false
  end.          

eqw({Left,Right}) -> 
  case eqw(Left) of
    {WL,true} -> case eqw(Right) of
                    {WL,true} -> {2*WL,true};
                            _ -> false
                 end;
        false -> false
  end;
eqw(N) -> {N,true}.
                       
%% In CPS

eq_CPS(T) -> eqw_CPS(T,fun({true,_}) -> true;
                          (false)    -> false
                       end).

eqw_CPS({Left,Right},K) ->
  eqw_CPS(Left,
          fun(false)     -> false;
             ({true,WL}) -> eqw_CPS(Right,
                                    fun({true,WR}) -> case WL of
                                                        WR -> K({true,2*WL});
                                                         _ -> false
                                                      end;
                                       (false)     -> false
                                    end)
          end);
eqw_CPS(N,K) -> K({true,N}).


%% Sorting the leaves increasingly

sort({Left,Right}) -> merge(sort(Left),sort(Right));
sort(           N) -> [N].

merge(   [],      Q)            -> Q;
merge(    P,     [])            -> P;
merge([I|P],Q=[J|_]) when I < J -> [I|merge(P,Q)];
merge(    P,  [J|Q])            -> [J|merge(P,Q)].
