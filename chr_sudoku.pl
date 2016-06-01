:- use_module(library(chr)).
:- use_module(library(lists)).
:- chr_constraint given/2, maybe/2.


given(P1,V) \ maybe(P2,L) <=> sees(P1,P2), select(V,L,L2) | maybe(P2,L2).
maybe(P,L) <=> member(V,L), given(P,V).


sees(X-_, X-_). % same row
sees(_-X, _-X). % same column
sees(X-Y, A-B) :- X//3 =:= A//3, Y//3 =:= B//3. % same box
