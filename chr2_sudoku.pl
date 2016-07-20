:- [sudoku_problems].

:- use_module(library(chr)).
:- use_module(library(lists)).
:- chr_option(optimize,full).

:- chr_type pos ---> row-col.
:- chr_type row == int.
:- chr_type col == int.

:- chr_constraint 	maybe(+pos, +),
					bucket(+pos, +).  % should be a list here.

add_to_bucket @  maybe(X-Y, Vals), bucket(C, Cells) <=> member(C, Vals), bucket(C, [X-Y | Cells]).

integrity @ bucket(_, Coords) ==> integrity(Coords).


integrity([]).
integrity([X-Y | T]) :-
	genAllRowPositions(Rows),
	genAllColPositions(Cols), % also add the Blocks
	checkCount(X-Y, Rows),
	checkCount(X-Y, Cols),
	integrity(T).



checkCount(_, []).
checkCount(X-Y, List):-
	occurences(List, X-Y, Occs),
	Occs =< 1.



row(X,Res) :- Res = [X-1, X-2, X-3, X-4, X-5, X-6, X-7, X-8, X-9].
col(X,Res) :- Res = [1-X, 2-X, 3-X, 4-X, 5-X, 6-X, 7-X, 8-X, 9-X].

t(AllRowCoords) :- genAllRowPositions(AllRowCoords).

genAllRowPositions(Result) :-  genAllPositions_([1,2,3,4,5,6,7,8,9], row, [], Result).
genAllColPositions(Result) :-  genAllPositions_([1,2,3,4,5,6,7,8,9], col, [], Result).

genAllPositions_([],_,R,R).
genAllPositions_([Row|Rest], Predicate, Res, Temp) :- 
	call(Predicate, Row, CurrentRow),
	genAllPositions_(Rest, Predicate, [CurrentRow|Res], Temp).


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