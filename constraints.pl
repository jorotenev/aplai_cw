check(GridW, GridH, X, Y, Val, Result):- 
	findall(Temp, check_(GridW, GridH, X, Y, Val, Temp), Result).

check_(GridW, GridH, X, Y, Val, (c(TopX, TopY), s(W, H))) :-
	write('asd'),
	TopYLow is max(1,Y-(Val-1)), between(TopYLow, Y, TopY),
	TopXLow is max(1,X-(Val-1)), between(TopXLow, X, TopX), 
	
	between(1,Val,W), between(1,Val,H), 
	
	W * H =:= Val, 
	
	TopX + (W-1) =< GridW, 
	TopY + (H-1) =< GridH, 
	
	TopX + (W-1) >= X,
	TopY + (H-1) >= Y,

	write('TopX is '), 
	write(TopX), nl, write('TopY is '),
	write(TopY),nl, write('W is '), 
	write(W),nl, write('H is '), 
	write(H),nl,write('============'),nl.

