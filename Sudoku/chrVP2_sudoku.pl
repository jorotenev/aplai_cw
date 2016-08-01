:- [sudoku_problems].

:- use_module(library(chr)).
:- use_module(library(lists)).
:- chr_option(optimize,full).

:- chr_type pos ---> row-col.
:- chr_type row == int.
:- chr_type col == int.
:- chr_type list(T) ---> [] ; [T|list(T)].
:- chr_constraint 	% the maybe/2 constraint keeps the possible numbers which can occupy a given position on the grid
					maybe(+pos, +list(pos)), 
				    % bucket/2: the int is a number that can occupy a cell. the list holds the coordinates on which the number occurs
					bucket(+int, +list(pos)),
					% temp/2 is used when a number from a maybe was picked, just before it is added to a bucket.
					temp(+pos,+int), 
					% added only after all the buckets and maybes are added to the store, so that the search can commence
					can_start/0.
					



/*
http://gecoder.org/examples/sudoku-set.html
Create one set (bucket) per assignable number (i.e. 1..9). Each bucket contains the 
positions of all squares that the number is located in. I.e. if the number 1
is located at cells 1-1 and 4-3, then bucket(1,[1-1,4-3]).
*/

/*
 solve/1 is the top level predicate, which should be used to solve puzzless.
*/
/********
CHR Rules
********/ 

% if there's a maybe constraint with just one possible number - then prepare the number to be added to a bucket.
absorb @ maybe(X-Y, [N]) <=> temp(X-Y,N).

% check if merging the temp with the bucket will make the bucket not correct as per the game rules
correct @ bucket(N,Coords) # passive \ temp(X-Y, N) <=> \+ integrity([X-Y|Coords]) | false.

% if the temp constraint hasn't been removed/no backtrack has occurred  in the previous 
% rules, then merge the temp with the bucket. Note that for efficiency, the Num variable
% is used in both heads. equivalently an equality check could have been made in the guard.
convert @ temp(X-Y, Num), bucket(Num, Coords) # passive <=> bucket(Num, [X-Y | Coords]).

% when we add a new bucket constrain, check the maybes. if a number from the list of "possible" values in a maybe constraint
% will make a bucket "wrong", then remove this value from the maybe constraint straight away, instead of waiting for the "search" rule to select the number
% and find it false later.
active @ bucket(N, Coords) \ maybe(X-Y, Possibles) # passive <=> memberchk(N, Possibles), \+ integrity([X-Y | Coords]) | delete(Possibles, N,NewPosib), maybe(X-Y, NewPosib).


% due to can_start, this rule will trigger only when all maybes were added to the constraint store.
% the body of the rule relies heavily on the backtracking behaviour of member/2.
search @ can_start, maybe(X-Y, Vals) # passive <=> member(Num, Vals), temp(X-Y, Num),can_start.



/******
Helpers
******/
/*
solve(+) is the top level predicate, which should be used to solve puzzless.
ProblemName should be one of the problems defined in sudoku_problems.pl
*/
solve(ProblemName):-
	write('Starting '), write(ProblemName), nl,
	puzzles(P, ProblemName), % get the puzzle
 	% generate the bucket constraints, based on the already populated hints of the puzzle
	buckets(P, Dict),
	% same for maybes, but via the empty cells of the puzzle
	maybes(P, Dict), !, 
	% the search rule will now trigger.
	can_start,
	% show the constraints of the store after the search has completed.
	% it will containt 9 buckets, with the correct coordinates in them
	chr_show_store(chr2_sudoku),
	statistics.



/*
integrity(List)
The predicate is true when the coordinates in the List are seen at most once
in each row, col and block.
%% */
integrity([]):-!.
integrity([_]):-!.
integrity(Cells) :-
	all_diff(Cells),
	readyRows(Rows),
	intersectionSizeIsValid(Cells, Rows),!,
	readyCols(Cols), 
	intersectionSizeIsValid(Cells, Cols),!,
	readyBlocks(Blocks),
	intersectionSizeIsValid(Cells, Blocks),!.
	


/*
intersectionSizeIsValid(List, 2DList)
The predicate is true when, for each of the sub-lists of @2DList,
the intersection of the sub-list with @List is less-or-equal to 1.
*/
intersectionSizeIsValid(_, []):-!.
intersectionSizeIsValid(Cells, [CurrList| Rest]):-
	intersection(Cells, CurrList, Intersection),!,
	length(Intersection, IntrSize),
	IntrSize =< 1,
	intersectionSizeIsValid(Cells, Rest).


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
% the predicates below are now used for efficiency reasons.
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
	
maybes(P,Dict) :-  	
	recurseRowM(P,1,Dict,[], Mullas),
	flatten(Mullas,Flatten),
	sortedOnLength(Flatten,Sorted),
	fromListToRule(Flatten).

fromListToRule([]).
fromListToRule([(Row-Col,Possibles)|Rest]):-
	maybe(Row-Col,Possibles),
	fromListToRule(Rest)
	.

sortedOnLength(Unsrt, Srtd):-
	predsort(criteria, Unsrt, Srtd)
	.


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
recurseRowM([],_,_,R,R):-!.
recurseRowM([X|T], Row,Dict, R, Temp):-
	recurseColM(X, Row, 1,Dict,[], Res),
	RowNext is Row + 1,
	recurseRowM(T, RowNext, Dict, [Res|R], Temp).

recurseColM([], _, _,_, R,R):-!.
recurseColM([X|T],Row, Col,Dict, R, Temp):-
	(integer(X) ->
		true,
		Add = R
		;
		filterImposibleMaybes(Row-Col, Dict, Possibles),
		Add = [(Row-Col,Possibles)|R]
	),
	Col2 is Col + 1,
	recurseColM(T, Row, Col2,Dict, Add, Temp).



filterImposibleMaybes(Row-Col, Dict, Possibles):-
	NaivePossibles = [1,2,3,4,5,6,7,8,9], 
	findall(
		Num, (
			member(Num,NaivePossibles),
			integrity([Row-Col|Dict.Num])
		), 
		Possibles).

% criteria/3 is used to do predsort. it will sort a list of tuples, where the second element
% in the tuple is a list. The ordering is determined by the length of the list.
% currenly it is in ascending order, so that after adding all the maybe constraints,
% the constraints with the shorted lists, will be called first.
criteria(R, (_-_,L1),(_-_,L2)) :- length(L1,Len1),length(L2,Len2), Len2 =\=Len1, compare(R, Len2, Len1).
criteria(R,E1,E2) :- compare(R,E1,E2).
all_diff(L) :- \+ (select(X,L,R), memberchk(X,R)).