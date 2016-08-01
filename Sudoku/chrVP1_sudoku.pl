:- [sudoku_problems].

:- use_module(library(chr)).
:- use_module(library(lists)).
:- chr_option(optimize,full). % comment this out if you need to use the chr_trace.

:- chr_type list(X) ---> [] ; [X | list(X)].
:- chr_constraint given(+pos,+val), maybe(+pos,+list(int)), can_start/0.
:- chr_type pos ---> row-col.
:- chr_type row == int.
:- chr_type col == int.
:- chr_type val == int.

/*
The classical Sudoku Viewpoint. given/2 constraints represent cells on the grid
for which we are certain about the number that's on the cell.
maybe/2 constraints expresses the uncertainty about the number in a cell at a particular position.
The 
*/
absorb @ maybe(P, [V]) <=>  given(P, V).
% for all given/2 constraints, check if any of the maybes has values which will clash with given/2 cells.
% if there are any such clashing numbers in the maybe, remove them and re-add the maybe without the updated possiblities.
sudoku @ given(P1,V) \ maybe(P2,L) <=> memberchk(V,L), sees(P1,P2) | select(V,L,L2) maybe(P2,L2).

% wait for all of the gvenns and maybes to be added before starting the search
% (achieved with the can_start in the head). 
% the search works because of the backtracking behaviour of the member/2.
% if a branch fails, Prolog will backtrack to this member/2 call (the state of the CHR 
%% store will also be reverted). Then a new value will be picked from the list, and a new given/2
%% constraint will be added to the store
search @ can_start,maybe(P,L) <=> member(V,L), given(P,V),can_start.
sees(X-_, X-_):-!. % same row
sees(_-X, _-X):-!. % same column
sees(X1-Y1, A1-B1) :- % same box
	X is X1 - 1, 
	Y is Y1 - 1, 
	A is A1 - 1, 
	B is B1 - 1, 
	X//3 =:= A//3, 
	Y//3 =:= B//3,!.

/*
Utils
*/

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

% add all given/2 constraints
givens(P) :-  recurseRow(P,1).

% add all maybe/2 constraints
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