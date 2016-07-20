:- [sudoku_problems].

asd(P) :- P = [[_,_,8,7,_,_,_,_,6], 
			[4,_,_,_,_,9,_,_,_],
			[_,_,_,5,4,6,9,_,_],
			[_,_,_,_,_,3,_,5,_],
			[_,_,3,_,_,7,6,_,_],
			[_,_,_,_,_,_,_,8,9],
			[_,7,_,4,_,2,_,_,5],
			[8,_,_,9,_,5,_,2,3], 
			[2,_,9,3,_,8,7,6,_]].

main :- puzzles(P,verydifficult), givens(P), maybes(P).


occurences(L, El, Res) :-  filter(L,El,Filtered), length(Filtered,Res).
filter(List, El, Res) :- filter_(List,El,[], Res).
filter_([], _, R, R).
filter_([H|T], El, Filtered, Temp):- 
	(
		H == El ->
		filter_(T, El, [H|Filtered],Temp)
		;
		filter_(T, El, Filtered,Temp)
	).