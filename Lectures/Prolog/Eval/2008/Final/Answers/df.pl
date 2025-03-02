% General tree (the subtrees are not ordered)
%
%          1
%         /|\
%        2 6 3
%       / \  
%      4   5 
%
edge(1,3).
edge(2,4).
edge(1,2).
edge(2,5).
edge(1,6).

% Reflexive and transitive closure of `edge'
%
rt(A,A).
rt(A,B) :- edge(A,X), rt(X,B).

% Depth-first traversal (not unique: depends on the
% order of the facts defining `edge'.
%
depth_first(Root,Nodes) :- findall(Node,rt(Root,Node),Nodes).
