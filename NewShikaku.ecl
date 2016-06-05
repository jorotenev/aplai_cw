:-module(newshikaku).
:- lib(ic).
:- export solve/2.
:- ensure_loaded(shikakuproblems).
:- ensure_loaded(shikakuprint).
:- import(show/4 from shikakuprint).
:- import (problem/4 from shikakuproblems).
:- import alldifferent/1 from ic_global.




solve(ProblemName,Solution) :-
	problem(ProblemName, Width, Height, Hints),
	%ic: (Board[1..Width,1..Height] :: 0..1),
	%create List with ID's
	( fromto([Hints,[]], [[(X,Y,Value)|Rest],NumberedHints], [Rest,[hint(X,Y,Value,c(IDx,IDy))|NumberedHints]], [[],NumberedHints]) do
		(
			IDx is X,
			IDy is Y
		)
	),

	
	( fromto( [NumberedHints, []], [[Hint|Rest], OldRec ], [Rest , NewRec ] , [[], List] ),
		param(NumberedHints, Width,Height) do
		(
			subtract(NumberedHints,[Hint], Others),			
			findall(NewRec,rectangle(Hint, Others, Width,Height,NewRec),List),
			append(OldRec,List,NewRec)
			
		)
	),
	length(List, N),
	length(NumberedHints,NumberOfHints),
	dim(SetX, [N]),
	ic:(SetX[1..N] :: 0..1),
	MaxN is N + 1,
	( fromto([List,[] , 1], [[rectangle(X1,Y1,W1,H1,N,c(I,J))|Rest],NumberedRec, Index], [Rest,[rectangle(X1,Y1,W1,H1,N,c(I,J),Index)|NumberedRec] , NewIndex], [[],NumberedRec,MaxN]) do
		(
			NewIndex is Index + 1
		)
	),

	
	
	%voor elke hint moeten we 1 rectangle vinden, dus de som is gelijk aan NumberOfHints
	(for(I,1,N), fromto(0,In,Out,Sum), param(SetX) do
		X #= SetX[I],
		Out #= In+X
	),
	Sum $= NumberOfHints,

	%write(Rectangles),
	%write(N),
	%write(List),
	%write(Rectangles),

	(fromto( NumberedRec , [rectangle(X1,Y1,W1,H1,_,_,Index1)|T1] , T1 , [] ), param(SetX) do
		(fromto(T1, [rectangle(X2,Y2,W2,H2,_,_,Index2)|T2], T2 , []) , param(X1,Y1,H1,W1,Index1 , SetX) do
			%write(Index1-Index2), nl,
			MaxX1 #= X1 + W1 -1,
			MaxY1 #= Y1 + H1 -1,
			MaxX2 #= X2 + W2 -1,
			MaxY2 #= Y2 + H2 -1 ,
			%write(SetX),
			%not overlap
			(
				X1 #> MaxX2 or X2 #> MaxX1 or Y1 #> MaxY2 or Y2 #> MaxY1
			) or
			%overlap
			(
				neg(X1 #> MaxX2 or X2 #> MaxX1 or Y1 #> MaxY2 or Y2 #> MaxY1) and ( neg(SetX[Index1]) or neg(SetX[Index2]))
			)	
		)
	),
	
	%write(NumberedRec),nl,
	(foreach(hint(_,_,_,c(IDx,IDy)),NumberedHints ), param(SetX,NumberedRec) do
	%	write(IDx-IDy),
		(foreach(rectangle(_,_,_,_,_,c(I,J),Index),NumberedRec), fromto(HintOverlaps,Out,In,[]), param(IDx,IDy) do
			(IDx is I, IDy is J ) -> Out=[Index|In] ; Out=In),
	%	write(HintOverlaps),nl,
		
		(foreach(X,HintOverlaps), fromto(0,In,Out,Sum), param(SetX) do
			Value #= SetX[X],
			Out #= In + Value
		),
		Sum $= 1
		
		
	),

	labeling(SetX),
	
	%write(SetX),
	( foreach(rectangle(X,Y,W,H,_,c(IDX,IDY),Index), NumberedRec) , fromto(Solution , Out ,In,[])  , param(SetX) do
		(
			%write(Index),
			subscript(SetX,[Index], Value),
			Value is 1
		) -> Out = [rect(c(IDX,IDY),c(X,Y),s(W,H))|In] ; Out=In),
	
	show(Width,Height,Hints,Solution).
	
	
	

no_overlap(rectangle(X1,Y1,W1,H1,_,_), rectangle(X2,Y2,W2,H2,_,_)) :-
	MaxX1 #= X1 + W1 -1,
	MaxY1 #= Y1 + H1 -1,
	MaxX2 #= X2 + W2 -1,
	MaxY2 #= Y2 + H2 -1 ,
	X1 #> MaxX2 or X2 #> MaxX1 or Y1 #> MaxY2 or Y2 #> MaxY1.




rectangle(hint(I,J,N,K),Others,Width,Height,rectangle(X,Y,W,H,N,K)):-
	ic:(X :: 1..Width),
	ic:(Y :: 1..Height),
	ic:(W :: 1..N),
	ic:(H :: 1..N),
	W*H #= N,
	X+W-1 #=< Width,
	Y+H-1 #=< Height,
	inside(X,Y,W,H,I,J),
	outsides(X,Y,W,H,Others),
	search([X,Y,W,H],0,smallest,indomain_reverse_max,complete,[]).

overlap(rectangle(X,Y,W,H,_,_), c(I,J)) :-
	%write(I), write(' '), write(J), nl,
	(
		I #>= X,
		J #>= Y,
		I #< X+W,
		J #< Y+H
	).
	
	
inside(X,Y,W,H,I,J):-
	I #>= X,
	J #>= Y,
	I #< X+W,
	J #< Y+H.
	
outsides(X,Y,W,H,L):-
	( foreach(hint(I,J,_,_),L), param(X,Y,W,H) do
		outside(X,Y,W,H,I,J)
	).
	
outside(X,Y,W,H,I,J):-
	I #< X or J #< Y or I #>=X+W or J #>= Y+H.
	
	