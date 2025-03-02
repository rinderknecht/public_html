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

% Depth-first traversal (not unique: depends on the order of the facts
% defining `edge'.
%
depth_first(Root,Nodes) :- findall(Node,rt(Root,Node),Nodes).

% `brt(A,B,N)' is provable iff composing `edge' N times from A leads to
% B.
%
brt(A,A,0).
brt(A,B,N) :- edge(A,X), M is N - 1, brt(X,B,M).

% Level
%
level(Root,Depth,Level) :- findall(Node,brt(Root,Node,Depth),Level).

% Successor
%
succ(Node,Children) :- findall(Child,edge(Node,Child),Children).

% Next level
%
next([],[]).
next([Node|Siblings],Next) :-
  succ(Node,Children),
  next(Siblings,Nephews),
  flatten([Children|Nephews],Next).

% Levels
%
levels(Root,[[Root]|Levels]) :- levels__([Root],Levels).

levels__([],[]).
levels__(Nodes,[NextLevel|Sublevels]) :-
  next(Nodes,NextLevel),
  levels__(NextLevel,Sublevels).
