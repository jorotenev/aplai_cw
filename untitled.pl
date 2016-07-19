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

givens(P) :-  recurseRow(P,1).

maybes(P) :-  recurseRowM(P,1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Givens
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
recurseRow([],_).
recurseRow([X|T], Row):-
	recurseCol(X, Row, 1),
	RowNext is Row + 1,
	recurseRow(T, RowNext).

recurseCol([], _, _).
recurseCol([X|T],Row, Col):-
	( integer(X) ->
		write(Row-Col-X),
		nl
		;
		true
	),
	Col2 is Col + 1,
	recurseCol(T, Row, Col2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Maybes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recurseRowM([],_).
recurseRowM([X|T], Row):-
	recurseColM(X, Row, 1),
	RowNext is Row + 1,
	recurseRowM(T, RowNext).

recurseColM([], _, _).
recurseColM([X|T],Row, Col):-
	( integer(X) ->
		true
		;
		write(Row-Col-[1,2,3,4,5,6,7,8,9]),
		nl
		
	),
	Col2 is Col + 1,
	recurseColM(T, Row, Col2).