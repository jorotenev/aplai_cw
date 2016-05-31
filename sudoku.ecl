:- module(sudoku).
:- export solve/1.
:- lib(ic).

:- ensure_loaded(problems).
:- ensure_loaded(boardFunctions).

:- import puzzles/2 from problems.
:- import alldifferent/1 from ic_global.
:- import print_board/1 from boardFunctions.
:- import listoflists_to_matrix/2 from boardFunctions.

solve(ProblemName) :-
	%getting the board associated with the problem name 
	puzzles(Board, ProblemName),
	%converting the board to eclipse style
	listoflists_to_matrix(Board, NewBoard),
	%moduling the problem
	constrains(NewBoard),
	%searching solution
	labeling(NewBoard),
	%printing the solution
	print_board(NewBoard).




constrains(Board) :-
	%every board must be a square
	dim(Board, [N, N]),

	%must be checked first, to avoid unnecessary calculations for a bad board


	%every item in the board must be in domain 1..N
	Board[1..N,1..N] :: 1..N,

	%every row should have unique numbers
	uniqueRows(Board),
	%every col should have unique numbers
	uniqueCols(Board),
	%every square should have unique numbers
	uniqueSquares(Board).

uniqueRows(Board) :-
	dim(Board, [N,N]),
	( for(I, 1, N) , param(Board, N) do
		Row is Board[I,1..N],
		alldifferent(Row)
	).


uniqueSquares(Board) :-
	dim(Board, [N,N]),
	SquareLength is integer(sqrt(N)),
	%for every row (row is SquareLength thick)
	%increment I with SquareLength 
	( for(Row , 1, N , SquareLength), param(Board,N ,SquareLength) do		
		%for every col in a square ( col is SquareLength long)
		( for(Col, 1, N , SquareLength), param(Board, SquareLength,Row) do
			uniqueSquare(Board, Row , Col , SquareLength)
		)
	).
% row & column give starter position
uniqueSquare(Board, Row , Column , SquareLength) :-
	RowEnd #= Row + SquareLength - 1,
	ColumnEnd #= Column + SquareLength - 1,

	( for(I,Row,RowEnd), for(J,Column, ColumnEnd) , foreach(X, Square), param(Board) do
		X is Board[I,J]

	),
	alldifferent(Square).

uniqueCols(Board) :-
	dim(Board, [N,N]),
	( for(J,1,N), param(Board,N) do
		Col is Board[1..N,J],
		alldifferent(Col)
	).




