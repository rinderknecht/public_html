split([],[],[]).
split([X|T],[X|P],N) :- X >= 0, split(T,P,N).
split([X|T],P,[X|N]) :- split(T,P,N).
