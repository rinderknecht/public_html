% Red-Black Trees
%
% 9 June 2008
%

-module(rbt).
-compile(export_all). % I am lazy. Don't do this.

% Red-Black Trees (RBT) are Binary Search Trees (BST) with the
% following invariants:
%
% (1) Each node is either red or black
% (2) The root is black
% (3) No red node has a red child
% (4) All paths from root to leaf have the same number of black nodes
%
% These constraints ensure that the height of a RBT is at most
% 2*[log(n+1)], where [x] ("floor of x") is the greastest integer
% which is lower than log(n+1).
%
% As a consequence, the longest path from root to leaf is, at worst,
% twice as long as the shortest: this is the balancing property, which
% is not so tight as with Adelson-Velskii and Landis' trees (AVL
% trees), but still makes red-black trees a data structure efficiently
% accessed.
%
% This module is based on the definitions Chris Okasaki in his book
% __Purely Functional Data Structures__ (sec. 3.3, Cambridge, 1998) or
% his paper __Red-Black Trees in a Functional Setting__ (Functional
% Pearls, J. Functional Programming, 1(1), 1993).

%=====================================================================
% Inserting a node as a leaf, without duplicates
%
% The insertion is done in two phases: first, the classical insertion
% in a BST (the added node is red), then, on the way up to the root,
% it must be checked if some of the invariants are broken, and if so,
% they must be restored by means of rotations. Rotations are
% restructuring operations that preserve the nature of a BST, and
% which can be designed to do something more too.
%
% Balancing cases:
%
%             z(black)               y(red)
%             /   \                  /   \
%          y(red)  d            x(black) z(black)
%          /   \        ==>       / \     / \
%       x(red)  c                a   b   c   d 
%       /  \
%      a    b
%
%           z(black)                y(red)
%            /    \                  /   \
%         x(red)   d           x(black) y(black)
%         /  \          ==>       / \     / \
%        a  y(red)               a   b   c   d
%            /  \
%           b    c
%
%         x(black)                   y(red)
%         /  \                       /  \
%        a  y(red)             x(black) z(black)
%            /  \       ==>       / \    / \
%           b  z(red)            a   b  c   d
%               /  \
%              c    d
%
%          x(black)                y(red)
%           /  \                    /  \
%          a  z(red)          x(black) z(black)
%              /  \     ==>      / \    / \
%          y(red)  d            a   b  c   d
%           / \
%          b   c
%
% The first two cases are symmetric by flipping, as well as
% the third and the fourth.
%
% Notice that the inorder traversals of the input tree and the output
% tree are the same and that the height of the result is lower by
% one.
%
add(E,T) ->
  {Left,_,Root,Right} = add__(E,T),
  {Left,black,Root,Right}.
add__(E,empty) ->
  {empty,red,E,empty};
add__(E,S={_,_,E,_}) -> S;
add__(E,{Left,Colour,Root,Right}) when E < Root ->
  balance({add__(E,Left),Colour,Root,Right});
add__(E,{Left,Colour,Root,Right}) ->
  balance({Left,Colour,Root,add__(E,Right)}).

balance({{{A,red,X,B},red,Y,C},black,Z,D}) ->
  {{A,black,X,B},red,Y,{C,black,Z,D}};
balance({A,black,X,{B,red,Y,{C,red,Z,D}}}) ->
  {{A,black,X,B},red,Y,{C,black,Z,D}};
balance({{A,red,X,{B,red,Y,C}},black,Z,D}) ->
  {{A,black,X,B},red,Y,{C,black,Z,D}};
balance({A,black,X,{{B,red,Y,C},red,Z,D}}) ->
  {{A,black,X,B},red,Y,{C,black,Z,D}};
balance(T) -> T.

% Optimisation of add/2 by rebalancing the left subtree only if the
% insertion was done on it, and dually on the right. Practically, this
% simply means that balance/1 is now split into balance_l/1 and
% balance_r/1 and called respectively if E < Root or not.
%
add1(E,T) ->
  {Left,_,Root,Right} = add1__(E,T),
  {Left,black,Root,Right}.
add1__(E,empty) ->
  {empty,red,E,empty};
add1__(E,{Left,Colour,E,Right}) ->
  {Left,Colour,E,Right};
add1__(E,{Left,Colour,Root,Right}) when E < Root ->
  balance_l({add1__(E,Left),Colour,Root,Right});
add1__(E,{Left,Colour,Root,Right}) ->
  balance_r({Left,Colour,Root,add1__(E,Right)}).

balance_l({{{A,red,X,B},red,Y,C},black,Z,D}) ->
  {{A,black,X,B},red,Y,{C,black,Z,D}};
balance_l({{A,red,X,{B,red,Y,C}},black,Z,D}) ->
  {{A,black,X,B},red,Y,{C,black,Z,D}};
balance_l(T) -> T.

balance_r({A,black,X,{B,red,Y,{C,red,Z,D}}}) ->
  {{A,black,X,B},red,Y,{C,black,Z,D}};
balance_r({A,black,X,{{B,red,Y,C},red,Z,D}}) ->
  {{A,black,X,B},red,Y,{C,black,Z,D}};
balance_r(T) -> T.

% We have
%
% add(E,T) ->
%   {Left,_,Root,Right} = add__(E,T),
%   {Left,black,Root,Right}.
% =
% add(E,T) ->
%   (fun({Left,_,Root,Right}) -> {Left,black,Root,Right} end) (add__(E,T)).
%
% so we can proceed to using CPS:
%
add3(E,T) -> add3(E,T,fun(X) -> X end).

add3(E,T,K) ->
  add3__(E,T, fun({Left,_,Root,Right}) -> K({Left,black,Root,Right}) end).

add3__(E,empty,K) ->
  K({empty,red,E,empty});
add3__(E,{Left,Colour,E,Right},K) ->
  K({Left,Colour,E,Right});
add3__(E,{Left,Colour,Root,Right},K) when E < Root ->
  add3__(E,Left,fun(Tree) -> balance3({Tree,Colour,Root,Right},K) end);
add3__(E,{Left,Colour,Root,Right},K) ->
  add3__(E,Right,fun(Tree) -> balance3({Left,Colour,Root,Tree},K) end).

balance3({{{A,red,X,B},red,Y,C},black,Z,D},K) ->
  K({{A,black,X,B},red,Y,{C,black,Z,D}});
balance3({A,black,X,{B,red,Y,{C,red,Z,D}}},K) ->
  K({{A,black,X,B},red,Y,{C,black,Z,D}});
balance3({{A,red,X,{B,red,Y,C}},black,Z,D},K) ->
  K({{A,black,X,B},red,Y,{C,black,Z,D}});
balance3({A,black,X,{{B,red,Y,C},red,Z,D}},K) ->
  K({{A,black,X,B},red,Y,{C,black,Z,D}});
balance3(T,K) -> K(T).
