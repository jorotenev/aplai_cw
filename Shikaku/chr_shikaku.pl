:- use_module(library(chr)).
:- chr_option(optimize, full).
%% :- use_module(library(lists)).

%% :- ensure_loaded(shikakuprint).
%% :- ensure_loaded(shikakuproblems).
:- use_module(shikakuprint).
:- use_module(shikakuproblems).

:- chr_constraint 	hint(+int, +int, +int).
:- chr_constraint	maybe(+, +).  %% hint_coords [(top_coords, size),..]
:- chr_constraint rect(+, +, +). %% hint_coords, c(TopX, TopY), s(W,H)
:- chr_constraint can_start/0. %% the first time this is added to the store, is when all maybes have been added.
:- chr_constraint boo/0.
:- chr_constraint specialMaybe(+,+).


/**
CHR Rules
**/


empty @ maybe(_, []) ==>  false.

absorb_maybe @ maybe(c(X,Y), [(c(TopX,TopY), s(Width,Height))]) <=>  
	rect(c(X,Y), c(TopX, TopY), s(Width,Height)).

% if the rects overlap, then fail. This will backtrack to the last member/2 in the search rule. 
% Note that if member/2 has Possibilities left, the passive rect will *exist* in that new branch of member/2.
% the passive pragma [1] here means that no mirror checks will be made because the rule will trigger only when
% the active constraint matches the fisrt head.
% [1] https://sicstus.sics.se/sicstus/docs/3.12.7/html/sicstus/CHR-Pragmas.html
integrity @ rect(c(_, _), c(TopX1, TopY1), s(W1, H1)) , rect(c(_, _), c(TopX2, TopY2), s(W2, H2)) # passive <=>
	\+ doNotOverlap((c(TopX1, TopY1), s(W1, H1)), (c(TopX2, TopY2), s(W2, H2))) |  false. 

/* 
	it's ok for the maybe constraint to be passive. 
	even if we are in the pre_"can_start" phase, 
	the rule triggers only when we add a new rect. This way, we use the 
	new knowledge we have (i.e. the rect itself) to prune the obviously wrong maybes.
*/
active_constraint @ rect(_, TopCoords, TopSize)  \ maybe(MaybeHintCoords, Possibilities) # passive<=>
	overlaps((TopCoords, TopSize), Possibilities,[], Overlaps),
	Overlaps \= []
	|
	subtract(Possibilities, Overlaps, NonConflictingMaybe),
	maybe(MaybeHintCoords, NonConflictingMaybe).


/* 
the biggest inefficiency is using simpagation here.
it increases the search space a lot because 
every time we add a new rect from the rule, when Applying the rect, the rule will be fired again.
Instead, if we use simplification(the current state), the rule will start only when the 
created rect rule applications are done.
*/
search @  can_start , maybe(c(X,Y), Possibilities) <=> 
	member((c(TopX, TopY), s(W, H)), Possibilities), 
	rect(c(X, Y), c(TopX, TopY), s(W, H)),
	can_start.
/****
Utils
****/
solve(ProblemName):-
	write("Starting "),write(ProblemName),
	problem(ProblemName, GridW, GridH, Hints),
	time(makeMaybes(GridW, GridH, Hints, Hints)),!,
	write("Maybes generated. CHR takes over now."),nl,
	can_start,
	statistics,
	show(GridW, GridH, Hints, chr),!,
	nl,write("Finished "),write(ProblemName),nl
	.

makeMaybes(_,_,[],_):-!.
makeMaybes(GridW, GridH, [(X, Y, Val) | Rest], AllHints):-
	check(GridW, GridH, X, Y, Val, AllHints, Possibilities),!,
	maybe(c(X,Y), Possibilities),
	makeMaybes(GridW, GridH, Rest, AllHints).

check(GridW, GridH, X, Y, Val, AllHints, Result):- 
	findall(
		Temp, 
		check_(GridW, GridH, X, Y, Val, AllHints, Temp), 
		TempResult),!,
	list_to_set(TempResult,Result),!.


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
	%% subtract(AllHints, [(X,Y,Val)], AllOtherHints),
	does_not_contain((c(TopX, TopY), s(W, H)), AllOtherHints).


/*not used anymore. it adds for readibility, but at an extra performance cost*/
rectsOverlap((C1, S1),(C2,S2)):-
	\+ doNotOverlap((C1,S1),(C2,S2)).

doNotOverlap((c(TopX1, TopY1), s(W1, H1)), (c(TopX2, TopY2), s(W2, H2))) :-
	(TopX1 + W1 =< TopX2 ; TopX2 + W2 =< TopX1)
	;
	(TopY1 + H1 =< TopY2 ; TopY2 + H2 =< TopY1).

 
does_not_contain(_, []):- !.
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


/**
overlaps(Rect, Possibilities, Overlapping)
given a rect, and a list of rects,
Overlapping contains the rects in the list which 
overlap with Rect.
**/
overlaps(_,[],[],_):-false,!.
overlaps(_,[],R,R) :-!.
overlaps(Rect, [Maybe|Possibilities], Result,Temp ) :-
	(
	rectsOverlap(Rect, Maybe) ->
		overlaps(Rect,Possibilities, [Maybe|Result], Temp)
		;
		overlaps(Rect,Possibilities, Result, Temp)
	).