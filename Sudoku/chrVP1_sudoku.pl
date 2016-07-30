%:- module(chr_sudoku).
:- [sudoku_problems].

:- use_module(library(chr)).
:- use_module(library(lists)).
:- chr_option(optimize,full).

:- chr_constraint given(+pos,+val), maybe(+pos,+), can_start/0.
:- chr_type list(X) ---> [] ; [X | list(X)].
:- chr_type pos ---> row-col.
:- chr_type row == int.
:- chr_type col == int.
:- chr_type val == int.


maybe(P, [V]) <=>  given(P, V).
given(P1,V) \ maybe(P2,L) <=> sees(P1,P2), select(V,L,L2) | maybe(P2,L2).
maybe(P,L) <=> member(V,L), given(P,V). % failure in the body triggers Prolog choicepoints

/*
Utils
*/
sees(X-_, X-_):-!. % same row
sees(_-X, _-X):-!. % same column
sees(X1-Y1, A1-B1) :- 
	X is X1 - 1, 
	Y is Y1 - 1, 
	A is A1 - 1, 
	B is B1 - 1, 
	X//3 =:= A//3, 
	Y//3 =:= B//3,!.

%%%%%%%%%%% 
% Handle input
%%%%%%%%%%%

solve(ProblemName) :- 
	write("Starting "),write(ProblemName),nl,
	puzzles(P,ProblemName), 
	givens(P),
	maybes(P), !,
	write("Input generated"),nl,
	can_start,
	statistics,	 
	chr_show_store(chr_sudoku).

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
	(integer(X) ->
		given(Row-Col, X)
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
	(integer(X) ->
		true
		;
		maybe(Row-Col, [1,2,3,4,5,6,7,8,9])
	),
	Col2 is Col + 1,
	recurseColM(T, Row, Col2).