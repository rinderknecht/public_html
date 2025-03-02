mirror(leaf(L), leaf(L)).
mirror(tree(Root,L,R), tree(Root,MR,ML)) :-
  mirror(L,ML), mirror(R,MR).

