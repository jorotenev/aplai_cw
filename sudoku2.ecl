:-module(set).
:- lib(ic), lib(ic_sets).
:-export(run/2).
:- import (print_board/1 from boardFunctions).
:- import (puzzles/2 from problems).
:- import (listoflists_to_matrix/2 from boardFunctions).
:- import alldifferent/1 from ic_global.


run(ProblemName , Solution) :-
	%getting the problem board associated with the problem name 
	puzzles(Temp, ProblemName),
	%converting the board to eclipse style, and adding 0's for all unknows
	listoflists_to_matrix(Temp, ProblemBoard),
	dim(ProblemBoard,[Dim,Dim]),
	%create a list of sets(Buckets)
	MaxPositions is Dim*Dim,
	intsets(Buckets, Dim, 1, MaxPositions),
	(foreach(B, Buckets), param(Dim) do 
		( 
			#(B,Dim)
			%write(B) , nl
		)
	),
	
	(multifor([I,J], 1, Dim) , param(ProblemBoard,Buckets, Dim) do
		(
			X is ProblemBoard[I,J],
			integer(X),
			elementID(I,J,Dim, ID), 
			getBucket(Buckets, X, Bucket),
			write(ID), nl ,
			ID in Bucket
			
		)
		;
		true
	),
	all_disjoint(Buckets),
	
	%construct row sets
	%1..9 , 10..18, ...
	%elke rij moet 1..9 bevatten dus moet een rij versprijd zijn over alle buckets
	(for(Row, 1 , Dim), param(Dim,Buckets) do
		(
			elementID(Row,1,Dim, LowerBound),
			elementID(Row, Dim, Dim , UperBound),
			%write(LowerBound) , write(' ') , write(UperBound), nl,
			intset(RowSet, LowerBound, UperBound),
			#(RowSet, Dim),
			( foreach(B,Buckets), param(RowSet) do
				#(B /\ RowSet , C),
				C #=1
			)
		)
	),
	%construct column sets
	(for(Col, 1, Dim) , param(Dim,Buckets) do
		(
			MaxPositions is Dim*Dim,
			intset(ColSet, 1, MaxPositions),
			#(ColSet, Dim),
			( for(Row, 1, Dim) , param(Dim,ColSet,Col) do
				(
					elementID(Row, Col, Dim, ID),
					ID in ColSet
				)
			),
			( foreach(B,Buckets), param(ColSet) do
				#(B /\ ColSet , C),
				C #=1
			)
		)

	),
	N is integer(sqrt(Dim)),
	%constuct box sets
	( multifor([Row,Col], 1, [Dim,Dim], [N,N] ) , param(N,Dim,Buckets) do
		(
			
			X is N - 1,
			MaxPositions is Dim*Dim,
			intset(BoxSet, 1, MaxPositions),
			( multifor([I,J], 0 , X), param(Row,Col,Dim,BoxSet) do
				(
					SubSquareRow is Row + I,
					SubSquareCol is Col + J,
					elementID(SubSquareRow, SubSquareCol, Dim, ID),
					ID in BoxSet
					%write(ID) , write(' ')
				)
			),
			%write(' '),nl
			( foreach(B,Buckets), param(BoxSet) do
				#(B /\ BoxSet , C),
				C #=1
			)
		)
	),
	
	dim(Solution, [Dim,Dim]),

	(for(I, 1, Dim) , param(Buckets, Dim,Solution) do
		MaxPositions is Dim*Dim,
		getBucket(Buckets, I, B),
		write(B),
		(for(ID,1 , MaxPositions), param(B,Solution,Dim,I) do
			(
				ID in B,
				elementID(Row, Col , Dim, ID),
				write(Row), write(' '), write(Col),write(' '), write(ID), nl,
				Solution[Row,Col] #= I
			)
			;
			(
				ID notin B
			)
			
		)
	),

	label_sets(Buckets),
	print_board(Solution),
	(foreach(X,Buckets) do
		(
			write(X) , nl
		)
	).

getBucket([H|_], 1 , H).
getBucket([_|BucketsRest],Counter, Result):-
	getBucket(BucketsRest, C , Result ),
	Counter is C  + 1.

		
elementID(Row , Col , Length , ID) :-
	Row #>0,
	Col #>0,
	Row #=< Length,
	Col #=< Length,
	RowIndex #= (Row - 1),
	RowLength #= RowIndex * Length,
	ID #= RowLength + Col.
	

label_sets([]).
label_sets([S|Ss]) :-
        insetdomain(S,_,_,_),
        label_sets(Ss).
		


