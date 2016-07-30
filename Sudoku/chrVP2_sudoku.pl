% load the puzzles
:- [sudoku_problems].

:- use_module(library(chr)).
:- use_module(library(lists)).
%% :- chr_option(optimize,full).

:- chr_type pos ---> row-col.
:- chr_type row == int.
:- chr_type col == int.

:- chr_constraint 	maybe(+pos, +),
					bucket(+int, +),
					temp(+pos,+int),
					can_start/0.


/*
http://gecoder.org/examples/sudoku-set.html
Create one set (bucket) per assignable number (i.e. 1..9). Each bucket contains the 
positions of all squares that the number is located in. I.e. if the number 1
is located at cells 1-1 and 4-3, then bucket(1,[1-1,4-3]).
*/

/*
todo
if bucket is full it has all numbers
*/

/********
CHR Rules
********/ 
sudoku @ bucket(_, Coords) <=>  \+ integrity(Coords) | false.

absorb @ maybe(X-Y, [N]) <=> temp(X-Y, N).

convert @ temp(X-Y, Num), bucket(Bucket, Coords) # passive  <=> Num =:= Bucket | bucket(Num, [X-Y | Coords]).

% MUST be after the "convert" rule.
tempb4bucket @ temp(X-Y,Num) <=> bucket(Num, [X-Y]).

addToBucket @ can_start, maybe(X-Y, Vals) # passive <=> member(Num, Vals), temp(X-Y, Num),can_start.



/******
Helpers
******/
solve(ProblemName):-
	write('Starting '), write(ProblemName),nl,
	puzzles(P, ProblemName),
	buckets(P, Dict),
	maybes(P, Dict), !, 
	can_start,
	chr_show_store(chr2_sudoku),
	statistics.

all_diff(L) :- \+ (select(X,L,R), memberchk(X,R)).

	

/*
integrity(List)
The predicate is true when the coordinates in the List are seen at most once
in each row, col and block.
*/
integrity([]):-!.
integrity(Cells) :-
	all_diff(Cells),
	readyRows(Rows),
	intersectionSizeIsValid(Cells, Rows),
	readyCols(Cols), 
	intersectionSizeIsValid(Cells, Cols),
	readyBlocks(Blocks),
	intersectionSizeIsValid(Cells, Blocks).
	


/*
intersectionSizeIsValid(List, 2DList)
The predicate is true when, for each of the sub-lists of @2DList,
the intersection of the sub-list with @List is less-or-equal to 1.
*/
intersectionSizeIsValid(_, []):-!.
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
intersectionSize([],_,0):-!.
intersectionSize([H|T], L, NewSize):-
	intersectionSize(T, L, Res),
	(
		member(H, L) ->
			NewSize is Res + 1
		;
			NewSize is Res
	).


/*
Note, that we use ready[Rows,Cols,Blocks]/1 for efficiency reasons.

genAllRowPositions(2DList) / genAllColPositions(2DList)
The predicate is true when the @2DList is a 2d list with all row/col/block positions
e.g. for row: [[1-1,1-2,1-3, ...], [2-1,2-2,2-3, ...], ...]
e.g. for col: [[1-1,2-1,1-3, ...], [1-2,2-2,3-2, ...], ...]
e.g. for block [[1-1, 1-2, 1-3, 2-1, 2-2, 2-3, 3-1, 3-2, 3-3], ...]
*/
genAllRowPositions(Result) :-  genAllPositions_([1,2,3,4,5,6,7,8,9], row, [], Result),!.
genAllColPositions(Result) :-  genAllPositions_([1,2,3,4,5,6,7,8,9], col, [], Result),!.
genAllBlockPositions(Result) :- genAllPositions_([1,2,3,4,5,6,7,8,9], block, [], Result),!.

genAllPositions_([],_,R,R):-!.
genAllPositions_([Row|Rest], Predicate, Res, Temp) :- 
	call(Predicate, Row, CurrentRow),
	genAllPositions_(Rest, Predicate, [CurrentRow|Res], Temp).

row(X,Res) :- Res = [X-1, X-2, X-3, X-4, X-5, X-6, X-7, X-8, X-9].
col(X,Res) :- Res = [1-X, 2-X, 3-X, 4-X, 5-X, 6-X, 7-X, 8-X, 9-X].



/**
block(number, List)
The predicate is true when coordinates in the @List, are the coordinates of 
cells in the @number-th block in the sudoku puzzle.
*/

block(X, Res) :- 
	fromNumberToCoordinates(X, OffsetRatioRow, OffsetRatioCol),
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


fromNumberToCoordinates(Num, RowResult, ColResult) :- 
	TempNum is Num - 1,
	RowResult is (div(TempNum, 3) +1),
	ColResult is mod(TempNum, 3) +1.


% this is the output of using genAll[Row,Col,Block]Positions/1
% we use the predicates below for efficiency reasons.
readyRows(Rows):- Rows=
	[
		[9-1,9-2,9-3,9-4,9-5,9-6,9-7,9-8,9-9],
		[8-1,8-2,8-3,8-4,8-5,8-6,8-7,8-8,8-9],
		[7-1,7-2,7-3,7-4,7-5,7-6,7-7,7-8,7-9],
		[6-1,6-2,6-3,6-4,6-5,6-6,6-7,6-8,6-9],
		[5-1,5-2,5-3,5-4,5-5,5-6,5-7,5-8,5-9],
		[4-1,4-2,4-3,4-4,4-5,4-6,4-7,4-8,4-9],
		[3-1,3-2,3-3,3-4,3-5,3-6,3-7,3-8,3-9],
		[2-1,2-2,2-3,2-4,2-5,2-6,2-7,2-8,2-9],
		[1-1,1-2,1-3,1-4,1-5,1-6,1-7,1-8,1-9]
	].
readyCols(Cols):- Cols = [
		[1-9,2-9,3-9,4-9,5-9,6-9,7-9,8-9,9-9],
		[1-8,2-8,3-8,4-8,5-8,6-8,7-8,8-8,9-8],
		[1-7,2-7,3-7,4-7,5-7,6-7,7-7,8-7,9-7],
		[1-6,2-6,3-6,4-6,5-6,6-6,7-6,8-6,9-6],
		[1-5,2-5,3-5,4-5,5-5,6-5,7-5,8-5,9-5],
		[1-4,2-4,3-4,4-4,5-4,6-4,7-4,8-4,9-4],
		[1-3,2-3,3-3,4-3,5-3,6-3,7-3,8-3,9-3],
		[1-2,2-2,3-2,4-2,5-2,6-2,7-2,8-2,9-2],
		[1-1,2-1,3-1,4-1,5-1,6-1,7-1,8-1,9-1]
	].

readyBlocks(Blocks):- Blocks = [
		[7-7,7-8,7-9,8-7,8-8,8-9,9-7,9-8,9-9],
		[7-4,7-5,7-6,8-4,8-5,8-6,9-4,9-5,9-6],
		[7-1,7-2,7-3,8-1,8-2,8-3,9-1,9-2,9-3],
		[4-7,4-8,4-9,5-7,5-8,5-9,6-7,6-8,6-9],
		[4-4,4-5,4-6,5-4,5-5,5-6,6-4,6-5,6-6],
		[4-1,4-2,4-3,5-1,5-2,5-3,6-1,6-2,6-3],
		[1-7,1-8,1-9,2-7,2-8,2-9,3-7,3-8,3-9],
		[1-4,1-5,1-6,2-4,2-5,2-6,3-4,3-5,3-6],
		[1-1,1-2,1-3,2-1,2-2,2-3,3-1,3-2,3-3]
	].


/****
Input
****/


/**
@Result will hold a dict where the keys are numbers, and the values are arrays of X-Y coordinates
e.g. for the expert puzzle
buckets{
	1:[],

	2:[9-4,7-1,2-5],
	3:[7-2,6-4,3-6,2-1],
	4:[6-6,3-1],
	5:[9-8,1-9],
	6:[9-2,8-9,6-8,2-6],
	7:[5-1,2-9],
	8:[6-3,5-9,4-6,1-7],
	9:[8-8,7-4,6-1,5-5]}
*/
buckets(P, FullBuckets) :- 
	emptyDict(EmptyBuckets), 
	recurseRow(P,1, EmptyBuckets, FullBuckets), 
	fromDictToChrRules(FullBuckets).
	
maybes(P,Dict) :-  recurseRowM(P,1,Dict).



/*******
Buckets
*******/
emptyDict(Dict):-
	findall(X-[], between(1,9,X), Data), dict_create(Dict,buckets,Data).

recurseRow([],_, Res,Res).
recurseRow([X|T], Row, Dict, Temp):-
	recurseCol(X, Row, 1, Dict, NewDict),
	RowNext is Row + 1,
	recurseRow(T, RowNext, NewDict, Temp).

recurseCol([], _, _, Res, Res).
recurseCol([X|T], Row, Col, Dict, Temp):-

	(integer(X) ->
		put_dict([X = [Row-Col|Dict.X]], Dict, NewDict)
		;
		NewDict = Dict
	),
	Col2 is Col + 1,
	recurseCol(T, Row, Col2, NewDict, Temp).

fromDictToChrRules(Dict):-
	dict_pairs(Dict,_, Pairs),
	createRules(Pairs, bucket).

createRules([],_).
createRules([X-Coords|Rest], ChrConstraint):-
	call(ChrConstraint,X,Coords),
	createRules(Rest, ChrConstraint).
	
/*****
Maybes
*****/
recurseRowM([],_,_):-!.
recurseRowM([X|T], Row,Dict):-
	recurseColM(X, Row, 1,Dict),
	RowNext is Row + 1,
	recurseRowM(T, RowNext, Dict).

recurseColM([], _, _,_):-!.
recurseColM([X|T],Row, Col,Dict):-
	(integer(X) ->
		true
		;
		filterImposibleMaybes(Row-Col, Dict, Possibles),
		maybe(Row-Col, Possibles)
	),
	Col2 is Col + 1,
	recurseColM(T, Row, Col2,Dict).



filterImposibleMaybes(Row-Col, Dict, Possibles):-
	
	NaivePossibles = [1,2,3,4,5,6,7,8,9], 
	findall(
		Num, (
			member(Num,NaivePossibles),
			integrity([Row-Col|Dict.Num])
		), 
		Possibles).
