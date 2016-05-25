:-module(boardFunctions).
:-export print_board/1.
:-export listoflists_to_matrix/2.

print_board(Board) :-
	dim(Board, [N,N]),
	( for(I,1,N), param(Board,N) do
	    ( for(J,1,N), param(Board,I) do
		X is Board[I,J],
		( var(X) -> write("  _") ; printf(" %2d", [X]) )
	    ), nl
	), nl.

%must be looked at !!!!!!!!
listoflists_to_matrix(Xss, Xzz) :-
    % list of lists to list of arrays
    ( foreach(Xs,Xss), foreach(Xz,Xzs) do
        array_list(Xz, Xs)
    ),
    % list of arrays to array of arrays
    array_list(Xzz, Xzs).
