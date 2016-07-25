



noOverlap((c(TopX1, TopY1), s(W1, H1)), (c(TopX2, TopY2), s(W2, H2))) :-
	TopX1 + (W1-1) < TopX2 
	;
	TopX2 + (W2-1) < TopX1
	;
	TopY1 + (H1-1) < TopY2
	;
	TopY2 + (H2-1) < TopY1.



split(El, List, Result) :- split_(El, List, _, Result).

split_(_,[],R,R) :- 
	var(R) ->
		fail,!
		;
		true.

split_(El, [Curr|List], Result, Temp):- 
	(El == Curr ->
		Result = List,
		split_(_, [], Result, Temp)
		;
		split_(El,List,Result,Temp)).