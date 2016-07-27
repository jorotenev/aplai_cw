:- use_module(library(chr)).
%% :- chr_option(optimize, full).
:- use_module(library(lists)).

%% :- ensure_loaded(shikakuprint).
%% :- ensure_loaded(shikakuproblems).
:- use_module(shikakuprint).
:- use_module(shikakuproblems).

:- chr_constraint 	hint(+int, +int, +int).
:- chr_constraint	maybe(+, +).  %% hint_coords [(top_coords, size),..]
:- chr_constraint rect(+, +, +). %% hint_coords, c(TopX, TopY), s(W,H)
:- chr_constraint can_start/0. %% the first time this is added to the store, is when all maybes have been added.

/**
CHR Rules
**/

%% empty @ maybe(_, []) ==> false.

%% absorb_maybe @ maybe(c(X,Y), [(c(TopX,TopY), s(Width,Height))]) <=> 
%% 	rect(c(X,Y), c(TopX, TopY), s(Width,Height)).


search @  maybe(c(X,Y), Possibilities) <=> 
	member((c(TopX, TopY), s(W, H)), Possibilities), 
	rect(c(X, Y), c(TopX, TopY), s(W, H)).


% if the rects overlap, then fail. This will backtrack to the last member/2 in the search rule. 
% Note that if member/2 has Possibilities left, the passive rect will *exist* in that new branch of member/2.
% the passive pragma [1] here means that no mirror checks will be made because the rule will trigger only when
% the active constraint matches the fisrt head.

% [1] https://sicstus.sics.se/sicstus/docs/3.12.7/html/sicstus/CHR-Pragmas.html
integrity @ rect(c(_, _), c(TopX1, TopY1), s(W1, H1)) , rect(c(_, _), c(TopX2, TopY2), s(W2, H2))  <=>
	rectsOverlap((c(TopX1, TopY1), s(W1, H1)), (c(TopX2, TopY2), s(W2, H2))) | false. 


/****
Utils
****/
solve(ProblemName):-
	problem(ProblemName, GridW, GridH, Hints),
	makeMaybes(GridW, GridH, Hints, Hints),
	can_start,
	show(GridW, GridH, Hints, chr),!,
	statistics.

makeMaybes(_,_,[],_):-!.
makeMaybes(GridW, GridH, [(X, Y, Val) | Rest], AllHints):-
	check(GridW, GridH, X, Y, Val, AllHints, Possibilities),!,
	maybe(c(X,Y), Possibilities),
	makeMaybes(GridW, GridH, Rest, AllHints).



check(GridW, GridH, X, Y, Val, AllHints, Result):- 
	findall(
		Temp, 
		check_(GridW, GridH, X, Y, Val, AllHints, Temp), 
		TempResult),
	list_to_set(TempResult,Result).


check_(GridW, GridH, X, Y, Val, AllHints, (c(TopX, TopY), s(W, H))) :-
	
	TopYLow is max(1,Y-(Val-1)),
	TopXLow is max(1,X-(Val-1)), 

 	between(TopYLow, Y, TopY), between(TopXLow, X, TopX), 
	
	between(1,Val,W), between(1,Val,H), 
	
	W * H =:= Val, 
	
	TopX + (W-1) =< GridW, 
	TopY + (H-1) =< GridH, 
	
	TopX + (W-1) >= X,
	TopY + (H-1) >= Y,
	
	delete(AllHints, (X,Y,Val), AllOtherHints),
	does_not_contain((c(TopX, TopY), s(W, H)), AllOtherHints).





splitOn(El, List, Result) :- split_(El, List, _, Result).

split_(_,[],R,R) :- 
	var(R) ->
		R = []
		;
		true.

split_(El, [Curr|List], Result, Temp):- 
	(El == Curr ->
		Result = List,
		split_(_, [], Result, Temp)
		;
		split_(El,List,Result,Temp)).

rectsOverlap((C1, S1),(C2,S2)):-
	\+ noOverlap((C1,S1),(C2,S2)).

noOverlap((c(TopX1, TopY1), s(W1, H1)), (c(TopX2, TopY2), s(W2, H2))) :-
	(TopX1 + (W1-1) < TopX2 , !)
	;
	(TopX2 + (W2-1) < TopX1 , !)
	;
	(TopY1 + (H1-1) < TopY2, !)
	;
	(TopY2 + (H2-1) < TopY1,!).

 
does_not_contain(_, []):-!.
does_not_contain((c(TopX, TopY), s(W, H)), [(OtherX,OtherY,_)|OtherHints]):-
	(
		(TopX + W - 1 < OtherX, !)
		;
		(OtherX < TopX , !)
		;
		(TopY + H - 1 < OtherY, !)
		;
		(OtherY < TopY, !)
	),
	does_not_contain((c(TopX, TopY), s(W, H)), OtherHints).