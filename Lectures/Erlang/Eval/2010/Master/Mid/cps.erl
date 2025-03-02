-module(cps).
-compile(export_all).

preorder(ext) -> [];
preorder({Root,Left,Right}) ->
  [Root|join(preorder(Left),preorder(Right))].

postorder(ext) -> [];
postorder({Root,Left,Right}) ->
  join(postorder(Left),join(postorder(Right),[Root])).

join(   [],Q) -> Q;
join([I|P],Q) -> [I|join(P,Q)].
  
join_cps(   [],Q,K) -> K(Q);
join_cps([I|P],Q,K) -> join_cps(P,Q,fun(V) -> K([I|V]) end).
                                            
pre(T) -> pre__(T,fun(X) -> X end).

pre__(ext,K) -> K([]);
pre__({Root,Left,Right},K) ->
  pre__(Left,
        fun(L) -> pre__(Right,
                        fun(R) -> join_cps(L,R,
                                           fun(X) -> K([Root|X]) end) end) end).

post(T) -> post__(T,fun(X) -> X end).

post__(ext,K) -> K([]);
post__({Root,Left,Right},K) ->
  post__(Left,
         fun(L) -> post__(Right,
                          fun(R) -> join_cps(R,[Root],
                                             fun(X) -> join_cps(L,X,K) end) 
                          end)
         end).
