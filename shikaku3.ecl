:-module(shikaku3).
:- lib(ic).
:- export solve/2.
:- export value_inrange/2.
:- ensure_loaded(shikakuproblems).
:- ensure_loaded(shikakuprint).
:- import(show/4 from shikakuprint).
:- import (problem/4 from shikakuproblems).
:- import alldifferent/1 from ic_global.

solve(ProblemName, Solution) :-
	problem(ProblemName, Width, Height, Hints),
	%ic: (Board[1..Width,1..Height] :: 0..1),
	%create List with ID's
	( fromto([Hints,[]], [[(X,Y,Value)|Rest],NumberedHints], [Rest,[hint(X,Y,Value,c(IDx,IDy))|NumberedHints]], [[],NumberedHints]) do
		(
			IDx is X,
			IDy is Y
		)
	),

	%find rectanges
	%write(NumberedHints),nl,
	( fromto([NumberedHints, []], [[Hint|Rest], Rectangles], [Rest , NewRectangles ] , [[], Rectangles] ),
		param(NumberedHints, Width,Height) do
		(
			subtract(NumberedHints,[Hint], Others),
			%write(Hint),
			%write(Others),nl,
			rectangle(Hint, Others, Width,Height,NewRec),
			%there is overlap between the new found NewRec and the already chosen Rec's
			%write(NewRec-': ' -Rectangles), nl,
			
			(foreach(Rec,Rectangles), param(NewRec) do
				no_overlap(Rec,NewRec)
			),
			append2(Rectangles, [NewRec], NewRectangles)
			%write(NewRectangles),nl
		)
	),
	%write(Rectangles), nl,
	%create solution list from found Rec
	( fromto([Rectangles, []], [[rectangle(X,Y,W,H,_,c(A,B))| Rest], Solution], [Rest, [rect(c(IDX,IDY), c(PosX,PosY), s(SizeW,SizeH))|Solution]] , [[], Solution] ) do
		(
			IDX is A,
			IDY is B,
			SizeW is W,
			SizeH is H,
			PosX is X,
			PosY is Y
		)
	),

	show(Width,Height,Hints,Solution).



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

	

	

no_overlap(rectangle(X1,Y1,W1,H1,_,_), rectangle(X2,Y2,W2,H2,_,_)) :-
	MaxX1 is X1 + W1 -1,
	MaxY1 is Y1 + H1 -1,
	MaxX2 is X2 + W2 -1,
	MaxY2 is Y2 + H2 -1 ,
	X1 #> MaxX2 or X2 #> MaxX1 or Y1 #> MaxY2 or Y2 #> MaxY1.

	

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
	
write_tupple(I,J,Value,K) :-
	write(I) , write(' '), write(J), write(' '), write(Value) , write(' ') , write(K), nl.
	
write_hints(Hints) :-
	(foreach(hint(I,J,Value,K), Hints) do
		write_tupple(I,J,Value,K)
	).
	
append2([],L,L):-!.
append2([H|T],L,[H|TL]):-append2(T,L,TL).	
	