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


absorb_maybe @ maybe(c(X,Y), [(c(TopX,TopY), s(Width,Height))]) <=> 
	rect(c(X,Y), c(TopX, TopY), s(Width,Height)).


search @ maybe(c(X,Y), Possibilities) <=> 
	member((c(TopX, TopY), s(W, H)), Possibilities), 
	rect(c(X, Y), c(TopX, TopY), s(W, H)),
	split((c(TopX, TopY), s(W, H)), Possibilities, Rest),
	length(Rest,LenRest),
	( LenRest > 0 ->
		maybe(c(X,Y), Rest)
		;
		true
	).


integrity @ rect(c(_, _), c(TopX1, TopY1), s(W1, H1)) , rect(c(_, _), c(TopX2, TopY2), s(W2, H2)) ==>
	noOverlap((c(TopX1, TopY1), s(W1, H1)), (c(TopX2, TopY2), s(W2, H2))).


solve(ProblemName):-
	problem(ProblemName, GridW, GridH, Hints),
	%% write(Hints),
	makeMaybes(GridW, GridH, Hints),
	show(GridW, GridH, Hints, chr).

/****
Utils
****/
noOverlap((c(TopX1, TopY1), s(W1, H1)), (c(TopX2, TopY2), s(W2, H2))) :-
	TopX1 + (W1-1) < TopX2 
	;
	TopX2 + (W2-1) < TopX1
	;
	TopY1 + (H1-1) < TopY2
	;
	TopY2 + (H2-1) < TopY1.


makeMaybes(_,_,[]).
makeMaybes(GridW, GridH, [(X, Y, Val) | Rest]):-
	check(GridW, GridH, X, Y, Val, Possibilities),
	maybe(c(X,Y), Possibilities),
	makeMaybes(GridW, GridH, Rest).

%% noOverlap/2 + +
 

check(GridW, GridH, X, Y, Val, Result):- 
	findall(Temp, check_(GridW, GridH, X, Y, Val, Temp), Result).

check_(GridW, GridH, X, Y, Val, (c(TopX, TopY), s(W, H))) :-
	%% write('asd'),
	TopYLow is max(1,Y-(Val-1)),
	TopXLow is max(1,X-(Val-1)), 

 	between(TopYLow, Y, TopY), between(TopXLow, X, TopX), 
	
	between(1,Val,W), between(1,Val,H), 
	
	W * H =:= Val, 
	
	TopX + (W-1) =< GridW, 
	TopY + (H-1) =< GridH, 
	
	TopX + (W-1) >= X,
	TopY + (H-1) >= Y.

	%% write('TopX is '), 
	%% write(TopX), nl, write('TopY is '),
	%% write(TopY),nl, write('W is '), 
	%% write(W),nl, write('H is '), 
	%% write(H),nl,write('============'),nl.

split(El, List, Result) :- split_(El, List, _, Result).

split_(_,[],R,R) :- 
	var(R) ->
		fail,!
		;
		true.

split_(El, [Curr|List], Result, Temp):- 
	(El == Curr ->
		Result = List,
		split_(_, [], Result, Temp)
		;
		split_(El,List,Result,Temp)).