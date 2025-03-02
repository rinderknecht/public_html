% Christian Rinderknecht
% 14 December 2010

-module(bt).
-compile(export_all).  % I am lazy. Don't do this.

%=====================================================================
% Product of integer nodes in a binary tree

% In tail form with an optimisation for zero-valued nodes.
%
% The worst case happens when there is no zero in `T'. In this case,
% the number of steps is 2n+2, where n is the size of `T'. See
% heigth/1.
%
tprod({Left,Root,Right}) -> tprod([],1,{Left,Root,Right}).

tprod([],Prod,empty) ->
  Prod;
tprod([Tree|Forest],Prod,empty) ->
  tprod(Forest,Prod,Tree);
tprod(_,_,{_,0,_}) ->
  0;
tprod(Forest,Prod,{Left,Root,Right}) ->
  tprod([Right|Forest],Root*Prod,Left).

%=====================================================================
% Perfect binary tree

perf(T) -> case perf__(T) of
            {true,_} -> true;   % We ignore the height
            false    -> false
           end.

perf__(empty)          -> {true,-1}; % So height of {empty,_,empty} is 0
perf__({Left,_,Right}) ->
  case perf__(Left) of
    {true,L} -> case perf__(Right) of
                  {true,L} -> {true,L+1};  % Implicit equality on L
                         _ -> false
                end;
    false    -> false
  end.
%=====================================================================
% Complete binary tree

% not in tail form
%
is_complete1(          empty) -> false;
is_complete1({empty,_,empty}) -> true;
%is_complete1({empty,_,    _}) -> false;
%is_complete1(    {_,_,empty}) -> false;
is_complete1( {Left,_,Right}) -> is_complete1(Left) andalso
                                 is_complete1(Right).
% In tail form
%
is_complete(empty) -> true;
is_complete(Tree)  -> is_complete([],Tree).

is_complete([],{empty,_,empty}) ->
  true;
is_complete([Tree|Forest],{empty,_,empty}) ->
  is_complete(Forest,Tree);
is_complete(_,{empty,_,_}) ->
  false;
is_complete(_,{_,_,empty}) ->
  false;
is_complete(Forest,{Left,_,Right}) ->
  is_complete([Right|Forest],Left).
    
%=====================================================================
% `foldup(F,I,T)' traverses the binary tree `T' in a depth-first,
% bottom-up manner. At each node, two recursive traversals on
% the left and right child are done __independently of each
% other__. When returning from these traversals, the results are
% passed together with the node in question to the function `F'.

% Not in tail form
%
% The number of steps is 2n+1, where n is the size of `T' (n steps
% for the internal nodes, n+1 for the external nodes), plus the number
% of steps of each application of `F' to its arguments (n
% applications). 
%
foldup(_,I,empty) -> I;
foldup(F,I,{Left,Root,Right}) ->
  F(foldup(F,I,Left),Root,foldup(F,I,Right)).

% A generalisation: folding up trees and forest recursively on n-ary
% trees.
%
foldt(F,G,{Root,Subtrees}) -> F(Root,foldt(F,G,Subtrees));
foldt(_,G,          empty) -> G([]);
foldt(F,G,         Forest) -> G(hw:map(fun(T)->foldt(F,G,T) end,Forest)).

% Partial Continuation-Passing Style.
%
foldup1(F,I,T) -> foldup1_x(F,I,T,fun (X) -> X end).

foldup1_x(_,I,empty,K) -> K(I);
foldup1_x(F,I,{Left,Root,Right},K) ->
  foldup1_x(F,I,Left,
            fun (L) -> foldup1_x(F,I,Right,
                                     fun (R) -> K(F(L,Root,R)) end)
            end).

% Defunctionalisation of the partial CPS. (UNTESTED)
%
foldup2(F,I,T) -> foldup2_x(F,I,T,id).

foldup2_x(_,I,empty,K) ->
    foldup2_app(K,I);
foldup2_x(F,I,{Left,Root,Right},K) ->
    foldup2_x(F,I,Left,{cont1,F,I,Root,Right,K}).

foldup2_app(id,I) -> I;    
foldup2_app({cont1,F,I,Root,Right,K},L) ->
  foldup2_x(F,I,Right,{cont2,L,F,Root,K});
foldup2_app({cont2,L,F,Root,K},R) ->
  K(F(L,Root,R)).

% Same but using a stack of first-order continuations. (UNTESTED)
%
foldup3(F,I,T) -> foldup3_x(F,I,T,[]).

foldup3_x(_,I,empty,K) ->
    foldup3_app(K,I);
foldup3_x(F,I,{Left,Root,Right},K) ->
    foldup3_x(F,I,Left,[{cont1,F,I,Root,Right}|K]).

foldup3_app([],I) -> I;    
foldup3_app([{cont1,F,I,Root,Right}|K],L) ->
  foldup3_x(F,I,Right,[{cont2,L,F,Root}|K]);
foldup3_app([{cont2,L,F,Root}|K],R) ->
  K(F(L,Root,R)).

%=====================================================================
% `height(T)' is the height of binary tree T. The height of the empty
% tree is 0.

% Not in tail form
%
% The number of steps to compute `height1(T)' is the size of `T' plus
% the number of empty nodes. For a given binary tree extended with
% empty nodes (so that each node has always two children), if there
% are n internal nodes, then there are n+1 empty nodes. (This applies
% even if n=0.) So the number of steps is 2n+1.
%
height1(         empty) -> 0;
height1({Left,_,Right}) -> 1+hw:max_int(height1(Left),height1(Right)).

% Not in tail form
%
% The number of steps of `heigth3(T)' is the size of `T', since the
% recursive calls never apply to an empty tree.
%
height3(          empty) -> 0;
height3({empty,_,empty}) -> 1;
height3({empty,_,Right}) -> 1 + height3(Right);
height3( {Left,_,empty}) -> 1 + height3(Left);
height3( {Left,_,Right}) -> 1+hw:max_int(height3(Left),height3(Right)).

% Using foldup/3.
%
% Same number of steps as heigth1/1.
%
height2(T) ->
  foldup(fun (Lup,_,Rup) -> 1 + hw:max_int(Lup,Rup) end,0,T).

% Almost in tail form.
%
% The number of steps is 2n+2, where n is the size of `T':
%
%   1  step by clause 1 of heigth/1
% + n steps by clause 3 of heigth_x/3 (one for each internal nodes)
% + n steps by clause 2 of heigth_x/3 (one for each empty node
%           except the rightmost)
% + 1  step by clause 1 of heigth_x/3 (for the rightmost empty node).
%
height(T) -> height_x([],0,0,T).

height_x([],Max,Depth,empty) ->
  hw:max_int(Max,Depth);
height_x([{D,Tree}|Forest],Max,Depth,empty) ->
  height_x(Forest,hw:max_int(Max,Depth),D,Tree);
height_x(Forest,Max,Depth,{Left,_,Right}) ->
  height_x([{Depth+1,Right}|Forest],Max,Depth+1,Left).    

% height/1 cannot be defined by means of foldl/3 or fold3/3.

%=====================================================================
% `sum(T)' returns the sum of the integer nodes in `T'. (If some node
% contains some data other than integer, the result is unspecified.)

% In tail form
%
% The number of steps is 2n+2, where n is the size of `T'. See
% height/1.
%
sum({Left,Root,Right}) -> sum([],0,{Left,Root,Right}).

sum([],Sum,empty) ->
  Sum;
sum([Tree|Forest],Sum,empty) ->
  sum(Forest,Sum,Tree);
sum(Forest,Sum,{Left,Root,Right}) ->
  sum([Right|Forest],Root+Sum,Left).

% Not in tail form
%
% The number of steps is 2n+2, where n is the size of `T'. See
% height1/1.
%
sum1({Left,Root,Right}) -> sum1_({Left,Root,Right}).

sum1_(            empty) -> 0;
sum1_({Left,Root,Right}) -> Root + sum1_(Left) + sum1_(Right).

% Tail form by means of foldl/3.
%
% The number of steps is 2n+1, where n is the size of `T'.
%
sum2({Left,Root,Right}) ->
  hw:foldl(fun (Node,A) -> Node + A end,0,{Left,Root,Right}).

% Using foldup/3.
%
sum3({Left,Root,Right}) ->
  foldup(fun (Lup,Node,Rup) -> Lup + Node + Rup end,
         0,
         {Left,Root,Right}).

%=====================================================================
% `size(T)' is the number of nodes in the binary tree `T'.

% Not in tail form
%
% The number of steps is 2n+1, where n is the size of `T'.
%
size1(         empty) -> 0;
size1({Left,_,Right}) -> 1 + size1(Left) + size1(Right).

% Tail form by means of foldl/3.
%
% The number of steps is 2n+1, where n is the size of `T'.
%
size2(T) -> hw:foldl(fun (_,A) -> 1 + A end,0,T).

% Using foldup/3.
%
size3(T) -> foldup(fun (Lup,_,Rup) -> Lup + 1 + Rup end,0,T).

% In tail form
%
% The number of steps is 2n+2, where n is the size of `T'. See
% height/1.
%
size({Left,Root,Right}) -> size([],0,{Left,Root,Right}).

size([],Size,empty) ->
  Size;
size([Tree|Forest],Size,empty) ->
  size(Forest,Size,Tree);
size(Forest,Size,{Left,_,Right}) ->
  size([Right|Forest],1+Size,Left).

%=====================================================================
% `flip(T)' is the binary tree which flips binary tree `T'.

% Not in tail form.
%
flip1(empty)             -> empty;
flip1({Left,Root,Right}) -> {flip1(Right),Root,flip1(Left)}.

% Using a foldup
%
% The number of steps is 2n+1, where n is the number of nodes.
%
flip2(T) -> foldup(fun (Lup,Root,Rup) -> {Rup,Root,Lup} end,empty,T).

% In tail form
%
flip(Tree) -> flip([],Tree).

flip(Acc,empty) ->
  flip_app(Acc,empty);
flip(Acc,{Left,Root,Right}) ->
  flip([{left,Root,Right}|Acc],Left).

flip_app([],Tree) ->
  Tree;
flip_app([{left,Root,Right}|Acc],RevL) ->
  flip([{right,Root,RevL}|Acc],Right);
flip_app([{right,Root,RevL}|Acc],RevR) ->
  flip_app(Acc,{RevR,Root,RevL}).

%=====================================================================
% `preorder(T)' is the list of the nodes of `T' traversed in preorder,
% that is, root first, then left subtree (recursively) and finally
% right subtree. If the tree is interpreted as some algebraic formula,
% then `preorder(T)' is often referred to as the __prefix notation__
% of the formula.

% Not in tail form.
%
% The maximum number of steps occurs when hw:join/2 requires the
% maximum number of steps, and since this number is 2n+2, where n is
% the length of the first argument, it implies that `inorder1(Left)'
% should be as big as possible, hence `Left'. The way to achieve that
% is to put all the nodes on the left side of the binary tree, so that
% it is isomorphic to a (left leaning) list,
% e.g. {{empty,a,empty},b,empty}.
%
% Let n be the size of this degenerate tree; let S(n) be the maximum
% number of steps of `preorder1/1' then
%
%   S(0) = 1                           by clause 1
% S(n+1) = 1 + S(n) + S(0) + (2*n + 2) by clause 2
%        = S(n) + 2*n + 4
%
% This leads to
%
% S(n) = S(0) + 2*n(n-1)/2 + 4*n = n^2 + 3n + 1.
%
preorder1(empty) ->
  [];
preorder1({Left,Root,Right}) ->
  [Root|hw:join(preorder1(Left),preorder1(Right))].

% Not in tail form, using foldup/3.
%
% Same maximum number of steps as preorder1/1, same worst case.
%
% Note that it is a little bit more efficient to write
% ... [Root|hw:join(Lup,Rup)] ...
% rather than
% ... hw:join([Root|Lup],Rup) ...
%
% The way to assess the maximum number of steps is as follows. We know
% that
%
%      1 step  by clause 1 of preorder2/1
% + 2n+1 steps by foldup/3 (n for the internal nodes, n+1 for the
%              external empty nodes)
% + 2*0 + 2 steps by hw:join([],[])
% + 2*1 + 2 steps by hw:join([_],[])
% + ...
% + 2*(n-1) + 2 steps by hw:join([....],[]) (at the root)
%
% Total is 2n+2 + 2n(n-1)/2 + 2n, that is  n^2 + 3n + 2.
%
preorder2(T) ->
  foldup(fun (Lup,Root,Rup) -> [Root|hw:join(Lup,Rup)] end,[],T).

% Another version, using hw:flatten/1.
%
% The good news is double: the efficiency is greater than using
% hw:join at each node and there is no worst case.
%
% There is one step by clause 1 of preorder7/1.
% 
% The number of steps of foldup/3 is 2n+1, where n is the number of
% (internal) nodes.
%
% The number of steps of hw:flatten(L) is the sum of the lengths of
% all the sublists in `L' plus two times the length of the resulting
% flat list, plus the number of empty lists, plus 4. Since there are
% n+1 empty nodes, which are mapped to [] by foldup (initial value,
% i.e., the second argument to foldup/3), this entails that there are
% n+1 empty lists to account for. The result contains all the nodes of
% the tree, thus `n'. For each node, a list of 3 elements is created,
% thus this leads to 3(n-1) for the sum of the lengths of these lists,
% from which we exclude the root since we only count the proper
% sublists. (Thus this line of reckoning is valid only if the tree
% contains at least one node.) So the total number of steps for
% hw:flatten is
%
% 3*(n-1) + 2n + (n+1) + 4 = 6n + 2, for n > 0.
%
% Adding the above mentionned steps, we get the total
%
% (6n + 2) + 1 + (2n+1) = 8n + 4 steps, for n > 0.
%
% The case of n = 0 must be considered separately. We find that the
% number of steps for preorder7(empty) is 6.
%
preorder7(T) ->
  hw:flatten(foldup(fun(Lup,Root,Rup)->[Lup,Root,Rup] end,[],T)).

% What about the following?
% 
% preorder11(T) ->
%   foldup(fun(Lup,Root,Rup)->hw:flatten([Lup,Root,Rup]) end,[],T).

% The number of steps to compute preorder5(T) where n is the size of
% `T' of this version is
%
%     1 step  by clause 1 of preorder5/1
% +   n steps by clause 3 of preorder5_/1
% + n+1 steps by clause 2 of preorder5_/1
% +   1 step  by clause 1 of preorder5_/1
%
% that is 2n+3
%
preorder5(T) -> preorder5_([T]).

preorder5_([]) -> [];
preorder5_([empty|Forest]) -> preorder5_(Forest);
preorder5_([{Left,Root,Right}|Forest]) ->
  [Root|preorder5_([Left,Right|Forest])].

% The number of steps of this version is
%
%     1 step  by clause 1 of preorder6/1
% +   n steps by clause 3 of preorder6_/2
% + n+1 steps by clause 2 of preorder6_/2
% +   1 step  by clause 1 of preorder6_/2
% + n+2 steps to compute the body of clause 1 of preorder6_/2
%
% that is, 3n+5 steps.
%
preorder6(T) -> preorder6_([],[T]).

preorder6_(Nodes,[]) ->
  hw:rev(Nodes);
preorder6_(Nodes,[empty|Forest]) ->
  preorder6_(Nodes,Forest);
preorder6_(Nodes,[{Left,Root,Right}|Forest]) ->
  preorder6_([Root|Nodes],[Left,Right|Forest]).
    
% Same as preorder6/1, but avoiding the empty nodes (except the
% root). Therefore, the number of steps should be 1 for the empty tree
% and (3n+5)-(n+1)=2n+4 if n>0.
%
preorder(empty) -> [];
preorder(    T) -> preorder_([],[T]).

preorder_(Nodes,[]) ->
  hw:rev(Nodes);
preorder_(Nodes,[{empty,Root,empty}|Forest]) ->
  preorder_([Root|Nodes],Forest);
preorder_(Nodes,[{empty,Root,Right}|Forest]) ->
  preorder_([Root|Nodes],[Right|Forest]); 
preorder_(Nodes,[{Left,Root,empty}|Forest]) ->
  preorder_([Root|Nodes],[Left|Forest]);
preorder_(Nodes,[{Left,Root,Right}|Forest]) ->
  preorder_([Root|Nodes],[Left,Right|Forest]).

% The number of steps is 3n+4:
%
%     1 step  by clause 1 of preorder8/1
% + n+2 steps to compute the call to hw:rev/1
% + n+1 steps by clause 1 of preorder8_/1 (external nodes once)
% + n   steps by clause 2 of preorder8_/1 (internal nodes once)
%
preorder8(T) -> hw:rev(preorder8_([],T)).

preorder8_(Nodes,empty) ->
  Nodes;
preorder8_(Nodes,{Left,Root,Right}) ->
  preorder8_(preorder8_([Root|Nodes],Left),Right).

% Same as preorder8/1 but without reversing the nodes at the end,
% since this is done on the execution stack:
%
% 2n+2 steps.
%
preorder9(T) -> preorder9_([],T).

preorder9_(Nodes,empty) ->
  Nodes;
preorder9_(Nodes,{Left,Root,Right}) ->
  [Root|preorder9_(preorder9_(Nodes,Right),Left)].

% This is a version improved from preorder8/1 by avoiding the visit of
% the external nodes (thus saving n+1 steps).
%
% The number of steps is then (3n+4)-(n+1)=2n+3 when n > 0, else 1.
%
% The function is not in tail form.
%
preorder10(empty) -> [];
preorder10(    T) -> hw:rev(preorder10_([],T)).

preorder10_(Nodes,{empty,Root,empty}) ->
  [Root|Nodes];
preorder10_(Nodes,{empty,Root,Right}) ->
  preorder10_([Root|Nodes],Right);
preorder10_(Nodes,{Left,Root,empty}) ->
  preorder10_([Root|Nodes],Left);
preorder10_(Nodes,{Left,Root,Right}) ->
  preorder10_(preorder10_([Root|Nodes],Left),Right).

% The number of steps of this version in tail form is
%
% 1 + 1 + n + n + n = 3n + 2.
%
preorder11(T) -> preorder11_([],[],T).

preorder11_(Nodes,[],empty) ->
  Nodes;
preorder11_(Nodes,[{Left,Root,empty}|Forest],empty) ->
  preorder11_(Nodes,[Root|Forest],Left);
preorder11_(Nodes,[Root|Forest],empty) ->
  preorder11_([Root|Nodes],Forest,empty);
preorder11_(Nodes,Forest,{Left,Root,Right}) ->
  preorder11_(Nodes,[{Left,Root,empty}|Forest],Right).

%=====================================================================
% `postorder(T)' is the list of the nodes of `T' traversed in
% postorder, that is, left subtree (recursively) first, then right
% subtree and finally the root. If the tree is interpreted as some
% algebraic formula, then `postorder(T)' is often referred to as the
% __postfix notation__ of the formula.

% Not in tail form.
%
% The number of steps is S(n). We have
%
%     S(0) = 1 by clause 1 of postorder1/1
% S(p+q+1) = 1 by clause 2 of postorder1/1
%          + S(p) to compute `postorder1(Left)' where p=size(Left)
%          + 2p+2 to compute `hw:join(postorder1(Left),...)' when
%                 `postorder1(Left)' is already computed
%          + S(q) + 2q+2 where q = size(Right)
%          = S(p) + S(q) + 2p + 2q + 5,
% that is
%     S(n) = S(p) + S(q) + 2n + 3  since p+q+1=n by definition.
%
% The sum S(p) + S(q) is maximum when n is split the most unevenly,
% that is, when the tree is degenerated into a list. The minimum
% happens when the tree is divided into two parts of equal size, i.e.,
% when the binary tree is complete, leading to a logarithmic number of
% steps (since it is proportional to the heigth). This is true every
% time the toll (here 2n + 3) is linear in the size, not constant.
% Therefore, let us take for example p=n and q=0 (i.e., a left leaning
% tree) as the worst case. Then
%
% S(n) = S(n-1) + S(0) + 2n + 3
%      = S(n-1) + 2n + 4
%
% By rewriting the equations for several cases and summing
% side-by-side, we have
%
%   S(1) =   S(0) +    2*1 + 4
%   S(2) =   S(1) +    2*2 + 4
%   S(3) =   S(2) +    2*3 + 4
% .............................
% S(n-1) = S(n-2) + 2(n-1) + 4
%   S(n) = S(n-1) + 2n     + 4
%
% By simplifying the summation, we have
%
% S(n) =   S(0) + 2(1+2+..+n) + 4n
%      =      1 + 2n(n+1)/2 + 4n
%      = n^2 + 5n + 1.  
%
postorder1(empty) ->
  [];
postorder1({Left,Root,Right}) ->
  hw:join(postorder1(Left),hw:join(postorder1(Right),[Root])).

% Using foldup/3
%
postorder2(T) ->
  foldup(fun(Lup,Root,Rup) ->hw:join(Lup,hw:join(Rup,[Root])) end,
         [],
         T).                 

% Not in tail form and very inefficient.
%
% The maximum number of steps happens in the same case as for
% inorder1/1. Assuming that n is the size of the degenerated tree, the
% number of steps is S(n). We have
%
% S(0) = 1 by clause 1 of postorder/1
% S(n+1) =   1      by clause 2 of postorder/1
%          + 2n^2+2 to compute `hw:join(postorder(Left),...)'
%          + S(n)   to compute `postorder1(Left)'
%          + 1      to compute `postorder(Right)' since Right=empty
%          + 2n^2+2 to compute `hw:join(hw:join(...,...),...)'
%        = S(n) + 4n^2 + 6
%
% By writing successive terms, it comes that
%
%   S(1) =   S(0) +    4*0^2 + 6
%   S(2) =   S(1) +    4*1^2 + 6
%   S(3) =   S(2) +    4*2^2 + 6
% ...............................
% S(n-1) = S(n-2) + 4(n-2)^2 + 6
%   S(n) = S(n-1) + 4(n-1)^2 + 6
%
% and summing both side and simplifying by S(1) + S(2) + ... + S(n-1):
%
%   S(n) =   S(0) + 4(1^2+2^2+..+(n-1)^2) + 6*n
%
% Let T(n) = 1^2 + 2^2 + ... + n^2.
%
% We have
%
% (x+1)^3       = x^3 + 3x^2 + 3x + 1
% (x+1)^3 - x^3 =       3x^2 + 3x + 1
%  n                   n          n             n
% Sum (x+1)^3 - x^3 = Sum 3x^2 + Sum 3x      + Sum 1
% x=1                 x=1        x=1           x=1
%
% (n+1)^3 - 1       = 3*T(n)   + 3(n(n+1)/2) +  n
% n^3 + 3n^2 + 3n   = 3*T(n)   + (3/2)n(n+1) +  n
% 2n^3 + 6n^2 + 6n  = 6*T(n)   + 3n(n+1)     + 2n
%                   = 6*T(n)   + 3n^2 + 5n
% 2n^3 + 3n^2 + n   = 6*T(n)
% 
% That is: T(n) = (2n^3 + 3n^2 + n)/6.
%
% By using this formula in the equation for S(n), we have now
%
%   S(n) = 1 + 4*(2n^3 + 3n^2 + n)/6 + 6n
% 3*S(n) = 3 + 12*(2n^3 + 3n^2 + n)/6 + 18n
%        = 3 + 4n^3 + 6n^2 + 2n + 18n
%        = 4n^3 + 6n^2 + 20n + 3
%
% That is
%
%   S(n) = (4n^3 + 6n^2 + 20n + 3)/3 ~ (4/3)*n^3 when n -> +infinity.
%
postorder5(empty) ->
  [];
postorder5({Left,Root,Right}) ->
  hw:join(hw:join(postorder5(Left),postorder5(Right)),[Root]).  

% The number of steps of this version is
%
%   1 step  by clause 1 of postorder3/1
% + 1 step  by clause 1 of postorder3_/1 (the leftmost external node)
% + n steps by clause 2 of postorder3_/1 (the left subtrees)
% + n steps by clause 3 of postorder3_/1 (the right subtrees)
%
% that is, 2n + 2.
%
postorder3(T) -> postorder3_([],[],T).

postorder3_(Nodes,[],empty) ->
  Nodes;
postorder3_(Nodes,[Left|Forest],empty) ->
  postorder3_(Nodes,Forest,Left);
postorder3_(Nodes,Forest,{Left,Root,Right}) ->
  postorder3_([Root|Nodes],[Left|Forest],Right).
 
% The number of steps of this version is
%
%     1 step  by clause 1 of postorder4/1
% +   n steps by clause 3 of postorder4_/2
% + n+1 steps by clause 2 of postorder4_/2
% +   1 step  by clause 1 of postorder4_/2
%
% that is, 2n + 3.
%
postorder4(T) -> postorder4_([],[T]).

postorder4_(Nodes,[]) ->
  Nodes;
postorder4_(Nodes,[empty|Forest]) ->
  postorder4_(Nodes,Forest);
postorder4_(Nodes,[{Left,Root,Right}|Forest]) ->
  postorder4_([Root|Nodes],[Right,Left|Forest]).

% Tail form, using foldr/3.
%
% The number of steps is 2n+2, where n is the size of `T'.
%
postorder6(T) -> hw:foldr(fun (Node,A) -> [Node|A] end,[],T).

% The number of steps is 2n+2:
%
%     1 step  by clause 1 of postorder7/1
% + n+1 steps by clause 1 of postorder7_/1 (external nodes once)
% + n   steps by clause 2 of postorder7_/1 (internal nodes once)
%
postorder7(T) -> postorder7_([],T).

postorder7_(Nodes,empty) ->
  Nodes;
postorder7_(Nodes,{Left,Root,Right}) ->
  postorder7_(postorder7_([Root|Nodes],Right),Left).

% Same as postorder/3 but without visiting the external (empty)
% nodes. Therefore, the number of steps is
%
% (2n+3)-(n+1) = n + 2 when n > 0, else 1.
%
postorder(empty) -> [];
postorder(    T) -> postorder_([],[T]).

postorder_(Nodes,[]) ->
  Nodes;
postorder_(Nodes,[{empty,Root,empty}|Forest]) ->
  postorder_([Root|Nodes],Forest);
postorder_(Nodes,[{empty,Root,Right}|Forest]) ->
  postorder_([Root|Nodes],[Right|Forest]);
postorder_(Nodes,[{Left,Root,empty}|Forest]) ->
  postorder_([Root|Nodes],[Left|Forest]);
postorder_(Nodes,[{Left,Root,Right}|Forest]) ->
  postorder_([Root|Nodes],[Right,Left|Forest]).

%=====================================================================
% `inorder(T)' is the list of the nodes of `T' traversed in inorder,
% that is, left subtree first (recursively), then root and finally
% right subtree (recursively). When the tree represents some algebraic
% formula, `inorder(T)' is called the __infix notation__ of the
% formula.

% Not in tail form and not efficient.
%
% The maximum number of steps occurs when hw:join/2 requires the
% maximum number of steps, and since this number is 2n+2, where n is
% the length of the first argument, it implies that `inorder1(Left)'
% should be as big as possible, hence `Left'. The way to achieve that
% is to put all the nodes on the left side of the binary tree, so that
% it is isomorphic to a (left leaning) list,
% e.g. {{empty,a,empty},b,empty}.
%
% Let n be the size of this degenerate tree; let S(n) be the maximum
% number of steps of `inorder1/1' then
%
%   S(0) = 1                           by clause 1
% S(n+1) = 1 + S(n) + S(0) + (2*n + 2) by clause 2
%        = S(n) + 2*n + 4
%
% This leads to
%
% S(n) = S(0) + 2*n(n-1)/2 + 4*n = n^2 + 3n + 1.
%
inorder1(empty) ->
  [];
inorder1({Left,Root,Right}) ->
  hw:join(inorder1(Left),[Root|inorder1(Right)]).

% Using foldup/3
%
inorder2(T) ->
  foldup(fun(Lup,Root,Rup) -> hw:join(Lup,[Root|Rup]) end,
         [],
         T).                    

% The number of steps of this version is
%
%     1 step  by clause 1 of inorder3/1
% +   1 step  by clause 1 of inorder3_/2
% + n+1 steps by clause 2 of inorder3_/2
% +   n steps by clause 3 of inorder3_/2
% +   n steps by clause 4 of inorder3_/2
%
% that is, 3n+3 steps.
%
% It is possible to get 2n+2 steps by not visiting the external nodes
% (empty), but this leads to six clauses. Instead, using two
% accumulators allows the same effect with less clauses.
% See inorder/1.
%
inorder3(T) -> inorder3_([],[T]).

inorder3_(Nodes,[]) ->
  Nodes;
inorder3_(Nodes,[empty|Forest]) ->
  inorder3_(Nodes,Forest);
inorder3_(Nodes,[{Left,Root,Right}|Forest]) ->
  inorder3_(Nodes,[Right,Root,Left|Forest]);    
inorder3_(Nodes,[Root|Forest]) ->
  inorder3_([Root|Nodes],Forest).    

% A faster version, in tail form.
%
% The idea is to use an accumulator containing subtrees to be visited
% in the traversal. Since these subtrees are pushed on the
% accumulator and we want a left-to-right ordering of the nodes, we
% must start visiting the rightmost nodes and come back later to visit
% the left siblings. An argument contains the result, which is the
% list of nodes in inorder.
%
% There is no worst case, as all the nodes are visited exactly twice:
% first, on the way down to the right subtree (clause 3 of
% inorder3_/3); second, on the way up to the left subtree (clause 2
% of inorder_/3). Clause 1 of inorder_/3 is used exactly once, at
% the very end.
%
% So the number of steps is
%
%         1 step  by clause 1 of inorder/1
% + size(T) steps by clause 2 of inorder_/3
% + size(T) steps by clause 3 of inorder_/3
% +       1 step  by clause 1 of inorder_/3
%
% Hence the number of steps is 2n+2, where n is the size of `T'.
%
inorder(T) -> inorder_([],[],T).
    
inorder_(Nodes,[],empty) ->
  Nodes;
inorder_(Nodes,[{Left,Root,empty}|Forest],empty) ->
  inorder_([Root|Nodes],Forest,Left);
inorder_(Nodes,Forest,{Left,Root,Right}) ->
  inorder_(Nodes,[{Left,Root,empty}|Forest],Right).

% Cf. preorder8/1. The number of steps is 3n+4.
%
inorder4(T) -> hw:rev(inorder4_x([],T)).

inorder4_x(Nodes,empty) ->
  Nodes;
inorder4_x(Nodes,{Left,Root,Right}) ->
  inorder4_x([Root|inorder4_x(Nodes,Left)],Right).

% Variant
%
inorder5(empty) -> [];
inorder5({{T1,X,T2},Y,T3}) -> inorder5({T1,X,{T2,Y,T3}});
inorder5({empty,Y,T3}) -> [Y|inorder5(T3)].

% With a counter:
%
inorder5c(T) -> inorder5c__(T,0).
inorder5c__(empty,C) -> {[],C+1};
inorder5c__({{T1,X,T2},Y,T3},C) -> inorder5c__({T1,X,{T2,Y,T3}},C+1);
inorder5c__({empty,Y,T3},C) -> {V,D}=inorder5c__(T3,C+1), {[Y|V],D}.

% Variant
%
inorder6(T) -> inorder6(T,[]).
inorder6(empty,S) -> S;
inorder6({T1,X,T2},S) -> inorder6(T1,[X|inorder6(T2,S)]).
    
% With a counter
%
inorder6c(T) -> inorder6c(T,[],1).
inorder6c(empty,S,C) -> {S,C+1};
inorder6c({T1,X,T2},S,C) -> {V,D}=inorder6c(T2,S,C+1), inorder6c(T1,[X|V],D).

%=====================================================================
% Some properties
%
%  preorder(flip(T)) = rev(postorder(T))
%   inorder(flip(T)) = rev(inorder(T))
% postorder(flip(T)) = rev(preorder(T))

%=====================================================================
% Level order (breadth-first traversal)
%
% Let `h' be the height of the input tree. Then the number of steps is
%
%      1 step  by clause 1 of lorder1/1
% +    1 step  by clause 1 of lorder1_/1
%   h+1
% + Sum(#nodes_i + 2)
%   i=2
%        steps to reverse all the levels except the first (the root)
%   = 2n + 2h
% +    h steps in calling clause 2 of lorder1_/1 (once per level)
% +  n+1 steps by clause 3 of lorder1_/1
% +  n   steps by clause 4 of lorder1_/1
%
% Therefore the total number is 4n + 3h + 3 steps.
%
% Therefore, the worst case happens when the height is maximum, i.e.,
% when the tree is a list. In this case, the number of steps is
% 4n+3n+3 = 7n+3.
%
lorder1(T) -> lorder1_([],[T]).

lorder1_([],                     []) -> [];
lorder1_(In,                     []) -> lorder1_([],hw:rev(In));
lorder1_(In,            [empty|Out]) -> lorder1_(In,Out);
lorder1_(In,[{Left,Root,Right}|Out]) ->
  [Root|lorder1_([Right,Left|In],Out)].

% 
lorder4(T) -> lorder4_([T],[]).

lorder4_(     [],[]) -> [];
lorder4_(     [], Q) -> lorder4_(hw:rev(Q),[]);
lorder4_([empty|P], Q) -> lorder4_(P,Q);
lorder4_([{Left,Root,Right}|P],Q) -> [Root|lorder4_(P,[Right,Left|Q])].

% level-order traversal (without optimisation)    

lo(T) -> lo__(enq(T,{[],[]})).

lo__({[],[]}) -> [];
lo__(Q) -> case deq(Q) of
             {ext,Q1} -> lo__(Q1);
             {{int,Root,Left,Right},Q1} -> [Root|lo__(enq(Right,enq(Left,Q1)))]
           end.

% !!
%
lord(empty) -> [];
lord({Left,Root,Right}) -> [Root|hw:shuffle(lord(Left),lord(Right))].

% Same as lorder1/1 but in tail form.
%
% The number of steps is thus 7n+3+(n+2) = 8n+5 in the worst case.
% In general: (4n+3h+3)+(n+2)= 5n+3h+5 steps.
%
lorder2(T) -> lorder2_([],[],[T]).

lorder2_(Nodes,[],[]) -> hw:rev(Nodes);
lorder2_(Nodes,In,[]) -> lorder2_(Nodes,[],hw:rev(In));
lorder2_(Nodes,In,[empty|Out]) -> lorder2_(Nodes,In,Out);
lorder2_(Nodes,In,[{Left,Root,Right}|Out]) ->
  lorder2_([Root|Nodes],[Right,Left|In],Out).

% Inspired from lorder2/1.
%
levels(T) -> levels_([[]],[],[T]).

levels_([[]|Levels],[],[]) -> 
  hw:rev(Levels);
levels_([L|Levels],In,[]) -> 
  levels_([[],hw:rev(L)|Levels],[],hw:rev(In));
levels_(Levels,In,[empty|Out]) ->
  levels_(Levels,In,Out);
levels_([L|Levels],In,[{Left,Root,Right}|Out]) ->
  levels_([[Root|L]|Levels],[Right,Left|In],Out).

% Optimisation of lorder2/1 by not visiting the external nodes, i.e.,
% they are neither enqueued nor dequeued. This saves n+1 dequeues and
% the number of reversal steps is reduced to (n-1)+2(h-1)=n+2h-3
% because the level h+1 is not visited anymore and all the internal
% nodes except the root are reversed. Accordingly, the number of calls
% to clause 2 of lorder_/3 is reduced to h-1. In total, it is
% (n+2h-3)+(h-1)=n+3h-4 instead of 2n+3h.  The difference of enqueuing
% steps w.r.t. to lorder2_/3 is therefore 2n+3h-(n+3h-4)=n+4.
%
% The number of steps is thus
%
% (5n+3h+5)-(n+1)-(n+4) = (4n+3h+4)-(n+4)
%                       = 3(n+h) steps.
%
% In the worst case, h = n, then we have 6n steps.
%
lorder(T) -> lorder_([],[],[T]).

lorder_(Nodes,[],[]) -> hw:rev(Nodes);
lorder_(Nodes,In,[]) -> lorder_(Nodes,[],hw:rev(In));
lorder_(Nodes,In,[{empty,Root,empty}|Out]) ->
  lorder_([Root|Nodes],In,Out);
lorder_(Nodes,In,[{empty,Root,Right}|Out]) ->
  lorder_([Root|Nodes],[Right|In],Out);
lorder_(Nodes,In,[{Left,Root,empty}|Out]) ->
  lorder_([Root|Nodes],[Left|In],Out);
lorder_(Nodes,In,[{Left,Root,Right}|Out]) ->
  lorder_([Root|Nodes],[Right,Left|In],Out).

% Another algorithm
%
% Let h be the height of the input tree. Then the number of steps is
%
%     1 step  by clause 1 of lorder3/1
% + h+2 steps by clauses 1 and 2 (all levels are cut, including the
%             external nodes and the empty forest "below" them)
%   h+1
% + Sum(#nodes_i + 1) 
%   i=1       by clauses 1, 2 and 3 of cut/1 (for each level,
%             including the external nodes, each node is processed
%             exactly once, and the case of no more nodes (empty list)
%             is handled exactly once too for each level)
%   = (2n+1)+(h+1)
%   = 2n+h+2
%   h+1
% + Sum(2*#inodes_i + 2)
%   i=1       steps to compute all the calls to hw:join/2 (each
%             level `i', including the empty forest below the leaves,
%             is appended, thus each time taking 2*#inodes_i+2 steps)
%   = 2n + 2(h+1)
%   = 2n + 2h + 2
%
% The total number is therefore 4n + 4h + 7.
%
% In the worst case, h = n (the tree is a list), and the programs
% takes then 8n+7 steps.
% 
lorder3(T) -> lorder3_([T]).

lorder3_(    []) -> [];  % Forgotten when evaluating the number of steps?
lorder3_(Forest) -> {Roots,Subforest} = cut(Forest),
                     hw:join(Roots,lorder3_(Subforest)).

cut(                        []) -> {[],[]};
cut(            [empty|Forest]) -> cut(Forest);
cut([{Left,Root,Right}|Forest]) ->
  {Roots,Subforest} = cut(Forest),
  {[Root|Roots],[Left,Right|Subforest]}.

%=====================================================================
% Printing a binary tree representing an arithmetic expression
% in prefix form

bin_op(Name,A,B) ->
  hw:join(Name,
    hw:join("(", hw:join(A, hw:join(",", hw:join(B,")"))))).
    
un_op(Name,A) ->
  hw:join(Name, hw:join("(", hw:join(A,")"))).

prefix({plus,Left,Right}) ->
  bin_op("add",prefix(Left),prefix(Right));
prefix({minus,Left,Right}) ->
  bin_op("sub",prefix(Left),prefix(Right));
prefix({power,Left,Right}) ->
  bin_op("power",prefix(Left),prefix(Right));
prefix({times,Left,Right}) ->
  bin_op("mult",prefix(Left),prefix(Right));
prefix({eq,Left,Right}) ->
  bin_op("eq",prefix(Left),prefix(Right));
prefix({uminus,Arg}) ->
  un_op("neg",prefix(Arg));
prefix({const,Const}) ->
  Const;
prefix({var,Var}) ->
  Var.

%=====================================================================
% Evaluating a tree representing an arithmetic expression

% This version fails if the lookup in the environment fails.
%
eval1(Env,{plus,Exp1,Exp2}) ->
  eval1(Env,Exp1) + eval1(Env,Exp2);
eval1(Env,{minus,Exp1,Exp2}) ->
  eval1(Env,Exp1) - eval1(Env,Exp2);
eval1(Env,{times,Exp1,Exp2}) ->
  eval1(Env,Exp1) * eval1(Env,Exp2);
eval1(Env,{power,Exp1,Exp2}) ->
  power(eval1(Env,Exp1),eval1(Env,Exp2));
eval1(Env,{eq,Exp1,Exp2}) ->
  eval1(Env,Exp1) == eval1(Env,Exp2);
eval1(Env,{uminus,Exp}) ->
  -eval1(Env,Exp);
eval1(_,{const,Const}) ->
  list_to_integer(Const);
eval1(Env,{var,Var}) ->
  lookup1(Env,Var).

power(N,P) -> power(N,P,1).

power(_,0,A) -> A;
power(N,P,A) -> power(N,P-1,N*A).  % Slow.
    
lookup1(         [],X) -> {not_found,X};
lookup1([{X,Val}|_],X) -> Val;
lookup1(    [_|Env],X) -> lookup1(Env,X).

% Without code duplication nor variable duplication in error reports.
% The unbound variables are kept in sorted lists.
%
eval2(Env,{plus,Exp1,Exp2}) ->
  app_bin2(plus,eval2(Env,Exp1),eval2(Env,Exp2));
eval2(Env,{minus,Exp1,Exp2}) ->
  app_bin2(minus,eval2(Env,Exp1),eval2(Env,Exp2));
eval2(Env,{times,Exp1,Exp2}) ->
  app_bin2(times,eval2(Env,Exp1),eval2(Env,Exp2));
eval2(Env,{power,Exp1,Exp2}) ->
  app_bin2(power,eval2(Env,Exp1),eval2(Env,Exp2));
eval2(Env,{eq,Exp1,Exp2}) ->
  app_bin2(eq,eval2(Env,Exp1),eval2(Env,Exp2));
eval2(Env,{uminus,Exp}) ->
  app_una2(uminus,eval2(Env,Exp));
eval2(_,{const,Const}) ->
  list_to_integer(Const);
eval2(Env,{var,Var}) ->
  lookup2(Env,Var).

lookup2(         [],X) -> {not_found,[X]};
lookup2([{X,Val}|_],X) -> Val;
lookup2(    [_|Env],X) -> lookup2(Env,X).

% Insertion sort without duplication (tail form)
%
isort([],    Su,   [])            -> Su;
isort(Sd,    [],[I|L])            -> rev_app_sort(Sd,[I],L);    
isort(Sd,[I|Su],[I|L])            -> rev_app_sort(Sd,[I|Su],L); % New
isort(Sd,[J|Su],[I|L]) when I > J -> isort([J|Sd],Su,[I|L]);
isort(Sd,    Su,[I|L])            -> rev_app_sort(Sd,[I|Su],L).

rev_app_sort(    [],Su,L) -> isort([],Su,L);
rev_app_sort([I|Sd],Su,L) -> rev_app_sort(Sd,[I|Su],L).

app_bin2(_,{not_found,X},{not_found,Y}) ->
  {not_found,isort([],X,Y)};
app_bin2(_,{not_found,X},_) ->
  {not_found,X};
app_bin2(_,_,{not_found,X}) ->
  {not_found,X};
app_bin2( plus,Val1,Val2) -> Val1 + Val2;
app_bin2(minus,Val1,Val2) -> Val1 - Val2;
app_bin2(times,Val1,Val2) -> Val1 * Val2;
app_bin2(power,Val1,Val2) -> power(Val1,Val2);
app_bin2(   eq,Val1,Val2) -> Val1 == Val2.

app_una2(     _,{not_found,X}) -> {not_found,X};
app_una2(uminus,          Val) -> -Val.

% Stop as soon as the first unbound variable is found in a preorder
% traversal.
%
% This version is not in tail form, so, once an error is found, the
% system stack containing all the calls waiting in the context, has to
% be popped up to the root.
%
eval3(Env,{BinOp,Exp1,Exp2}) ->
  eval3_1(Env,eval3(Env,Exp1),{BinOp,Exp2});
eval3(_,{const,N}) ->
  list_to_integer(N);
eval3(Env,{var,X}) ->
  lookup3(Env,X);
eval3(Env,{UnOp,Exp}) ->
  eval3_1(Env,eval3(Env,Exp),UnOp).

eval3_1(_,{not_found,X},_) -> {not_found,X};
eval3_1(Env,Arg1,{BinOp,Exp2}) ->
  eval3_2(eval3(Env,Exp2),{BinOp,Arg1});
eval3_1(_,Arg,uminus) -> -Arg.

eval3_2({not_found,X},_)   -> {not_found,X};
eval3_2(Arg2,{ plus,Arg1}) -> Arg1 + Arg2;
eval3_2(Arg2,{minus,Arg1}) -> Arg1 - Arg2;
eval3_2(Arg2,{times,Arg1}) -> Arg1 * Arg2;
eval3_2(Arg2,{power,Arg1}) -> power(Arg1,Arg2);
eval3_2(Arg2,{   eq,Arg1}) -> Arg1 == Arg2.

lookup3(         [],X) -> {not_found,X};
lookup3([{X,Val}|_],X) -> Val;
lookup3(    [_|Env],X) -> lookup3(Env,X).

% This version is in tail form (except for the call to power/2).
%
eval4(Env,Exp) -> eval4(Env,Exp,[]).
    
eval4(Env,{BinOp,Exp1,Exp2},A) -> eval4(Env,Exp1,[{BinOp,Exp2}|A]);
eval4(Env,        {const,N},A) -> up(Env,list_to_integer(N),A);
eval4(Env,          {var,X},A) -> lookup4(Env,Env,X,A);   % Two Env
eval4(Env,       {UnOp,Exp},A) -> eval4(Env,Exp,[UnOp|A]).

up(_,Arg,[]) ->
  Arg;
up(Env,Arg,[uminus|A]) ->
  up(Env,-Arg,A);
up(Env,Arg1,[{BinOp,Exp2}|A]) -> 
  eval4(Env,Exp2,[{apply,BinOp,Arg1}|A]);
up(Env,Arg2,[{apply,plus,Arg1}|A]) ->
  up(Env,Arg1+Arg2,A);
up(Env,Arg2,[{apply,minus,Arg1}|A]) ->
  up(Env,Arg1-Arg2,A);
up(Env,Arg2,[{apply,times,Arg1}|A]) ->
  up(Env,Arg1*Arg2,A);
up(Env,Arg2,[{apply,power,Arg1}|A]) ->
  up(Env,power(Arg1,Arg2),A);           % Not a tail call
up(Env,Arg2,[{apply,eq,Arg1}|A]) ->
  up(Env,Arg1==Arg2,A).

lookup4(  _,         [],X,_) -> {not_found,X};  % End of computation
lookup4(Env,[{X,Val}|_],X,A) -> up(Env,Val,A);
lookup4(Env,      [_|E],X,A) -> lookup4(Env,E,X,A).

% This version reports an error as soon as possible (if the lookup in
% the environment fails), because it is in tail form (more precisely
% in Continuation-Passing Style).
%
eval5(Env,Exp) -> eval5(Env,Exp,fun (X) -> X end).

eval5(Env,{plus,Exp1,Exp2},K) ->
  eval5(Env,Exp1,fun(L)->eval5(Env,Exp2,fun(R)->K(L+R) end) end);
eval5(Env,{minus,Exp1,Exp2},K) ->
  eval5(Env,Exp1,fun(L)->eval5(Env,Exp2,fun(R)->K(L-R) end) end);
eval5(Env,{times,Exp1,Exp2},K) ->
  eval5(Env,Exp1,fun(L)->eval5(Env,Exp2,fun(R)->K(L*R) end) end);
eval5(Env,{power,Exp1,Exp2},K) ->
  eval5(Env,Exp1,fun(L)->eval5(Env,Exp2,fun(R)->K(power(L,R))end)end);
eval5(Env,{eq,Exp1,Exp2},K) ->
  eval5(Env,Exp1,fun(L)->eval5(Env,Exp2,fun(R)->K(L==R)end)end);
eval5(Env,{uminus,Exp},K) ->
  eval5(Env,Exp,fun(V) -> K(-V) end);
eval5(_,{const,Const},K) ->
  K(list_to_integer(Const));
eval5(Env,{var,Var},K) ->
  lookup5(Env,Var,K).

lookup5(         [],X,_) -> {not_found,X}; % Continuation dropped
lookup5([{X,Val}|_],X,K) -> K(Val);
lookup5(    [_|Env],X,K) -> lookup5(Env,X,K).

% Same as eval5/2 but first-order form since it is the
% defunctionalisation of eval5/2.
%
eval6(Env,Exp) -> eval6(Env,Exp,[]).

eval6(Env,{plus,Exp1,Exp2},K) ->
  eval6(Env,Exp1,[{plus1,Env,Exp2}|K]);
eval6(Env,{minus,Exp1,Exp2},K) ->
  eval6(Env,Exp1,[{minus1,Env,Exp2}|K]);
eval6(Env,{times,Exp1,Exp2},K) ->
  eval6(Env,Exp1,[{times1,Env,Exp2}|K]);
eval6(Env,{power,Exp1,Exp2},K) ->
  eval6(Env,Exp1,[{power1,Env,Exp2}|K]);
eval6(Env,{eq,Exp1,Exp2},K) ->
  eval6(Env,Exp1,[{eq1,Env,Exp2}|K]);
eval6(Env,{uminus,Exp},K) ->
  eval6(Env,Exp,[uminus|K]);
eval6(_,{const,Const},K) ->
  apply6(K,list_to_integer(Const));
eval6(Env,{var,Var},K) ->
  lookup6(Env,Var,K).

lookup6(         [],X,_) -> {not_found,X}; % Continuation dropped
lookup6([{X,Val}|_],X,K) -> apply6(K,Val);
lookup6(    [_|Env],X,K) -> lookup6(Env,X,K).

apply6([],Val) -> Val;

apply6([{plus1,Env,Exp2}|K],Val1) ->
  eval6(Env,Exp2,[{plus2,Val1}|K]);
apply6([{minus1,Env,Exp2}|K],Val1) ->
  eval6(Env,Exp2,[{minus2,Val1}|K]);
apply6([{times1,Env,Exp2}|K],Val1) ->
  eval6(Env,Exp2,[{times2,Val1}|K]);
apply6([{power1,Env,Exp2}|K],Val1) ->
  eval6(Env,Exp2,[{power2,Val1}|K]);
apply6([{eq1,Env,Exp2}|K],Val1) ->
  eval6(Env,Exp2,[{eq2,Val1}|K]);

apply6([{ plus2,Val1}|K],Val2) -> apply6(K,Val1+Val2);
apply6([{minus2,Val1}|K],Val2) -> apply6(K,Val1-Val2);
apply6([{times2,Val1}|K],Val2) -> apply6(K,Val1*Val2);
apply6([{power2,Val1}|K],Val2) -> apply6(K,power(Val1,Val2));
apply6([   {eq2,Val1}|K],Val2) -> apply6(K,Val1==Val2);
apply6(       [uminus|K], Val) -> apply6(K,-Val).

%=====================================================================
% Breadth-first Numbering
%

% Okasaki's solution
%
enq(X,{In,Out}) -> {[X|In],Out}.

deq({[Y|In],     []}) -> deq({[],hw:rev([Y|In])});
deq({    In,[X|Out]}) -> {X,{In,Out}}.

f(T) -> {R,_} = deq(f(1,{[],[T]})),R.

f(_,{[],[]}) -> {[],[]};
f(I,Q) ->
  case deq(Q) of
    {empty,Q1} -> enq(empty,f(I,Q1));
    {{A,_,B},Q1} -> 
      {B1,Q2} = deq(f(I+1,enq(B,enq(A,Q1)))),
      {A1,Q3} = deq(Q2),
      enq({A1,I,B1},Q3)
  end.

% Variant
%
bfn(T) -> bfn__(0,[],[T]).

bfn__(_,[],           []) -> {[],[]};
bfn__(I, R,           []) -> bfn__(I,[],hw:rev(R));
bfn__(_,[],      [empty]) -> {[],[empty]};
bfn__(I, R    ,[empty|F]) -> {R1,F1}=bfn__(I,R,F), {[empty|R1],F1};
bfn__(I, R,[{T1,_,T2}|F]) -> {R1,[T21,T11|F1]}=bfn__(I+1,[T2,T1|R],F),
                             {[{T11,I,T21}|R1],F1}.    

% My solution
%
% Not in tail form.
%
bf_num(T) -> bf_num_([[]],[],[T],1).

bf_num_([[]|Levels],[],[],_) -> 
  rebuild(Levels);
bf_num_(Levels,In,[],I) -> 
  bf_num_([[]|Levels],[],hw:rev(In),I);
bf_num_([L|Levels],In,[empty|Out],I) ->
  bf_num_([[empty|L]|Levels],In,Out,I);
bf_num_([L|Levels],In,[{Left,_,Right}|Out],I) ->
  bf_num_([[I|L]|Levels],[Right,Left|In],Out,I+1).

rebuild([[T]]) -> T;
rebuild([Low,Up|Levels]) -> rebuild([graft([],Low,Up)|Levels]).

graft(F,[],[]) -> hw:rev(F);
graft(F,Low,[empty|Up]) -> graft([empty|F],Low,Up);
graft(F,[Right,Left|Low],[Root|Up]) ->
  graft([{Left,Root,Right}|F],Low,Up).


% Preorder numbering

npre(T) -> npre(1,T).

npre(I,ext) -> I;
npre(I,{int,Root,Left,Right}) ->
  {J,L}=npre(Left,I+1), {K,R}=npre(Right,J), {K,{int,I,L,R}}.
