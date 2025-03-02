% Binary Trees
%
% Christian Rinderknecht
% 1 December 2008
%
% A and B denote accumulators
% C and I,J,K denote items
% M,N,P denote integers
% L,Q,R denote lists
%
% Note: The length of a list `L' is denoted by `|L|'.

%=====================================================================
% `height(T,H)' is provable iff `H' is the height of the binary tree
% `T'.  The height of the empty tree is undefined. The height of a
% tree reduced to a root is 0. The height of a non-empty tree is 1
% plus the maximum height of its subtrees.
%
height({Left,Root,Right},H) :- height__({Left,Root,Right},H).

height__(empty,-1).
height__({Left,_,Right},H) :-
  height__(Left,Hl), height__(Right,Hr), max(Hl,Hr,Hm), H is Hm + 1.

%=====================================================================
% `size(T,S)' is provable iff `S' is the number of nodes in the binary
% tree `T' (external nodes are not accounted for).
%
size(empty,0).
size({Left,_,Right},S) :-
  size(Left,L), size(Right,R), S is 1 + L + R.

%=====================================================================
% `preorder(T,L)' is provable iff `L' is the list of the nodes in
% binary tree `T' in preorder, i.e., for each subtree, the root comes
% first, then the nodes of the left subtree in preorder, followed by
% the nodes of the right subtree in preorder.

% Using conc/3 slows down.
%
% The maximum number of inferences occurs when conc/3 requires the
% maximum number of inferences, and since this number is `n', where
% `n' is the length of the first argument, it implies that `L' should
% be as long as possible, hence `preorder1(Left,L)' should use as many
% inferences as possible. The way to achieve that is to put all the
% nodes on the left side of the binary tree, so that it is isomorphic
% to a (left leaning) list, e.g., {{empty,a,empty},b,empty}.
%
% Let `n' be the size of this degenerate tree `T'; let S(n) be the
% maximum number of inferences to prove `preorder1(T,L)' then
%
%   S(0) = 0                 by clause 1 of preorder1/2
% S(n+1) = 1 + S(n) + 0 + n  by clause 2 of preorder1/2
%        = S(n) + n + 1
%
% By rewriting the equations for several cases and summing
% side-by-side, we have
%
%   S(1) =   S(0) + 1
%   S(2) =   S(1) + 2
%  ...................
%   S(n) = S(n-1) + n
%
% Thus S(n) = S(0) + n(n+1)/2 = n(n+1)/2 ~ n^2/2.
%
preorder1(empty,[]).
preorder1({Left,Root,Right},Nodes) :-
  preorder1(Left,L), preorder1(Right,R), conc([Root|L],R,Nodes).

% An efficient, direct approach.
%
% The number S(n) of inferences to prove `preorder(T,L)', where `n' is
% the size of `T' (or the length of `L') is
%
%   S(n) = 1 + T(n)
%   T(0) = 0
% T(n+1) = 1 + n
%
% That is, S(n) = n.
% 
preorder(Tree,Nodes) :- preorder__(Tree,[],Nodes).

preorder__(empty,Nodes,Nodes).
preorder__({Left,Root,Right},Acc,[Root|Nodes]) :-
  preorder__(Right,Acc,R), preorder__(Left,R,Nodes).

%=====================================================================
% `postorder(T,L)' is provable iff `L' is the list of the nodes in
% binary tree `T' in postorder, i.e., for each subtree, first come the
% nodes of the left subtree in postorder, then the nodes of the right
% subtree in postorder and, finally, the root.

% Using conc/3 is slow.
%
% Let S(n) be the number of inferences needed to prove
% postorder1(T,L), where `n' is the size of `T' (or, equivalently, the
% length of `L'). We have
%
%     S(0) = 0
% S(1+p+q) = 1 + S(p) + S(q) + q + p where `p' is the size of `Left' and
%                                    `q' the size of `Right'
% that is
%
%     S(n) = S(p) + S(q) + n since p+q+1=n by definition.
%
% The sum S(p) + S(q) is maximum when `n' is split the most unevenly,
% that is, when the tree is degenerated into a list. The minimum
% happens when the tree is divided into two parts of equal size, i.e.,
% when the binary tree is complete, leading to a logarithmic number of
% steps (since it is proportional to the height). This is true every
% time the toll (here exactly n) is linear in the size, not constant.
% Therefore, let us take for example p=n and q=0 (i.e., a left leaning
% tree) as the worst case. Then
%
% S(n) = S(n-1) + S(0) + n = S(n-1) + n
%
% By rewriting the equations for several cases and summing
% side-by-side, we have
%
%  S(1) =   S(0) + 1
%  S(2) =   S(1) + 2
% ...................
%  S(n) = S(n-1) + n
%       =   S(0) + n(n+1)/2 = n(n+1)/2 ~ n^2/2
%
postorder1(empty,[]).
postorder1({Left,Root,Right},Nodes) :-
  postorder1(Left,L), postorder1(Right,R),
  conc(R,[Root],A), conc(L,A,Nodes).

% A direct, faster approach.
%
%     S(n) = 1 + T(n)
%     T(0) = 0
% T(1+p+q) = 1 + T(q) + T(p)
%
% Thus S(n) = n + 1.
%
postorder(Tree,Nodes) :- postorder__(Tree,[],Nodes).

postorder__(empty,Nodes,Nodes).
postorder__({Left,Root,Right},Acc,Nodes) :-
  postorder__(Right,[Root|Acc],R), postorder__(Left,R,Nodes).

%=====================================================================
% `inorder(T,L)' is provable iff `L' is the list of the nodes in
% binary tree `T' in inorder, i.e., for each subtree, first come the
% nodes of the left subtree in inorder, next the root and finally the
% nodes of the right subtree in inorder.

% The maximum number of inferences occurs when conc/3 requires the
% maximum number of inferences, and since this number is `n', where
% `n' is the length of the first argument, it implies that `L' in
% `inorder1(Left,L)' should be as long as possible, hence `Left'. The
% way to achieve that is to put all the nodes on the left side of the
% binary tree, so that it is isomorphic to a (left leaning) list,
% e.g. {{empty,a,empty},b,empty}.
% 
% Let `n' be the size of this degenerate tree; let S(n) be the maximum
% number of inferences to prove `inorder1(T,L)' then 
%
%   S(0) = 0
% S(n+1) = 1 + S(n) + S(0) + n
%        = S(n) + (n+1)
%
% So
%
% S(1) =   S(0) + 1
% S(2) =   S(1) + 2
%...................
% S(n) = S(n-1) + n
%
% By summing these equations and eliminating S(1) + ... + S(n-1), we
% draw
%
% S(n) = S(0) + n(n+1)/2 = n(n+1)/2 ~ n^2/2
%
inorder1(empty,[]).
inorder1({Left,Root,Right},Nodes) :-
  inorder1(Left,L), inorder1(Right,R), conc(L,[Root|Right]).

% A direct, faster approach
%
%     S(n) = 1 + T(n)
%     T(0) = 0
% T(1+p+q) = 1 + T(p) + T(q)
%
% Thus S(n) = n + 1.
%
inorder(Tree,Nodes) :- inorder__(Tree,[],Nodes).

inorder__(empty,Nodes,Nodes).
inorder__({Left,Root,Right},Acc,Nodes) :-
  inorder__(Right,Acc,R), inorder__(Left,[Root|R],L).

%=====================================================================
% `flip(T,U)' is provable iff `U' is the image in a mirror of binary
% tree `T'.
%
flip(empty,empty).
flip({Left,Root,Right},{FL,Root,FR}) :-
  flip(Left,FL), flip(Right,FR).

%=====================================================================
% Some properties
%
%  preorder(flip(T),R) <=> postorder(T,U), rev(U,R)
%   inorder(flip(T),R) <=>   inorder(T,U), rev(U,R)
% postorder(flip(T),R) <=>  preorder(T,U), rev(U,R)

%=====================================================================
% `lorder(T,L)' is provable iff `L' is the list of the nodes in binary
% tree `T' in level order, that is, `L' contains all the nodes visited
% level by level, from the root to the leaves, from right to left.
%
% Let `h' be the height of the input tree and `n' the number of
% internal nodes. Then the number of inferences is
%
%          1 inference by clause 1 of lorder/2
%   h+1
% + Sum(#nodes_i + 1)
%   i=2
%            inferences to reverse all the levels, including the
%            lowest external nodes and except the first (the root)
%   = 2n + h
% +        h inferences by clause 2 of lorder__/3 (once per level)
% +  n+1     inferences by clause 3 of lorder__/3 (external nodes)
% +    n     inferences by clause 4 of lorder__/3 (internal nodes)
%
% Therefore the total number is 4n + 2h + 2.
%
lorder(Tree,Nodes) :- lorder__([],[Tree],Nodes).

lorder__([],         [],   []).
lorder__(In,         [],Nodes) :- rev(In,Ni), lorder__([],Ni,Nodes).
lorder__(In,[empty|Out],Nodes) :- lorder__(In,Out,Nodes).
lorder__(In,[{Left,Root,Right}|Out],[Root|Nodes]) :-
  lorder__([Right,Left|In],Out,Nodes).

% levels(Tree,Levels) is provable iff Levels is the list of all levels
% in Tree, including [] for the deepest external nodes (last item of
% Levels).
%
% Let `h' be the height of the input tree and `n' the number of
% internal nodes. Then the number of inferences is
%
%          1 inference  by clause 1 of levels/2
% +   h  + 1 inferences by clause 2 of levels__/2 (all levels are
%            processed, including the deepest external nodes)
% + n    + 1 inferences by clause 2 of cut/3 (all external nodes are empty)
% + n        inferences by clause 3 of cut/3 (all internal nodes are visited)
%
% The total number of inferences is therefore 2n + h + 3
%
levels(Tree,Levels) :- levels__([Tree],Levels).

levels__([],[]).
levels__(Forest,[Roots|Sublevels]) :-
  cut(Forest,Roots,Subforest),
  levels__(Subforest,Sublevels).

cut([],[],[]).
cut([empty|Forest],Roots,Subforest) :-
  cut(Forest,Roots,Subforest).
cut([{Left,Root,Right}|Forest],[Root|Roots],[Left,Right|Subforest]) :-
  cut(Forest,Roots,Subforest).
