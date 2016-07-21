:- [sudoku_problems].

:- use_module(library(chr)).
:- use_module(library(lists)).
:- chr_option(optimize,full).

:- chr_type pos ---> row-col.
:- chr_type row == int.
:- chr_type col == int.

:- chr_constraint 	maybe(+pos, +),
					bucket(+pos, +),
					temp(+pos,+int).

/********
CHR Rules
********/ 

convert @ temp(X-Y, Num) , bucket(Bucket, Coords) <=> Num =:= Bucket | bucket(Bucket, [X-Y | Coords]).

absorb @ maybe(X-Y, [N]), bucket(N, Vals) <=> bucket(N, [X-Y|Vals]).

sudoku @ bucket(_, Coords) ==> integrity(Coords).

addToBucket @ maybe(X-Y, Vals) <=> member(Num, Vals), temp(X-Y, Num).
%% integrity @ bucket(_, Coords) ==> integrity(Coords).

%% unique @ bucket(X, Coords), bucket(Y, Coords2) ==> intersectionSize(Coords, Coords2, IntrSize), IntrSize > 0  | X == Y.


/******
Helpers
******/

/*
integrity(List)
The predicate is true when the coordinates in the List are seen at most once
in each row, col and block.
*/
integrity([]).
integrity(Cells) :-
	genAllRowPositions(Rows),
	genAllColPositions(Cols), % Block checks will also be added later.
	genAllBlockPositions(Blocks),
	intersectionSizeIsValid(Cells, Rows),
	intersectionSizeIsValid(Cells, Cols),
	intersectionSizeIsValid(Cells, Blocks).
	

/*
intersectionSizeIsValid(List, 2DList)
The predicate is true when for each of the sub-lists of @2DList,
the intersection of the sub-list with @List is less-or-equal to 1.
*/
intersectionSizeIsValid(_, []).
intersectionSizeIsValid(Cells, [CurrList| Rest]):-
	intersectionSize(Cells, CurrList, IntrSize),
	IntrSize =< 1,
	intersectionSizeIsValid(Cells, Rest).


/*
intersectionSize(List1, List2, IntersectionSize).
The predicate is true when the intersection(number of common elements)
between @List1 and @List2 is equal to IntersectionSize

@List2 *must* be non empty
*/
intersectionSize([],_,0).
intersectionSize([H|T], L, NewSize):-
	intersectionSize(T, L, Res),
	(
		member(H, L) ->
			NewSize is Res + 1
		;
			NewSize is Res
	).


/*
genAllRowPositions(2DList) / genAllColPositions(2DList)
The predicate is true when the @2DList is a 2d list with all row/col/block positions
e.g. for row: [[1-1,1-2,1-3, ...], [2-1,2-2,2-3, ...], ...]
e.g. for col: [[1-1,2-1,1-3, ...], [1-2,2-2,3-2, ...], ...]
e.g. for block [[1-1, 1-2, 1-3, 2-1, 2-2, 2-3, 3-1, 3-2, 3-3], ...]
*/
genAllRowPositions(Result) :-  genAllPositions_([1,2,3,4,5,6,7,8,9], row, [], Result).
genAllColPositions(Result) :-  genAllPositions_([1,2,3,4,5,6,7,8,9], col, [], Result).
genAllBlockPositions(Result) :- genAllPositions_([1,2,3,4,5,6,7,8,9], block, [], Result).

genAllPositions_([],_,R,R).
genAllPositions_([Row|Rest], Predicate, Res, Temp) :- 
	call(Predicate, Row, CurrentRow),
	genAllPositions_(Rest, Predicate, [CurrentRow|Res], Temp).

row(X,Res) :- Res = [X-1, X-2, X-3, X-4, X-5, X-6, X-7, X-8, X-9].
col(X,Res) :- Res = [1-X, 2-X, 3-X, 4-X, 5-X, 6-X, 7-X, 8-X, 9-X].



/**
ugh. ugly code.
block(number, List)
The predicate is true when coordinates in the @List, are the coordinates of 
cells in the @number-th block in the sudoku puzzle.
*/

block(X, Res) :- 
	magic(X, OffsetRatioRow, OffsetRatioCol),
	TR is OffsetRatioRow - 1,
	TC is OffsetRatioCol - 1,
	R is TR * 3,
	C is TC * 3 ,
	Temp = [(1+R)-(1+C),(1+R)-(2+C),(1+R)-(3+C),(2+R)-(1+C),(2+R)-(2+C),(2+R)-(3+C),(3+R)-(1+C),(3+R)-(2+C),(3+R)-(3+C)],
	fixList(Temp, Res).

fixList([],[]).
fixList([(N1+R)-(N2+C) | Rest], [F-S | Temp]):-
	fixList(Rest, Temp),
	F is N1 + R,
	S is N2 + C.


magic(Num, RowResult, ColResult) :- 
	TempNum is Num - 1,
	RowResult is (div(TempNum, 3)+1),
	ColResult is mod(TempNum,3) +1.
