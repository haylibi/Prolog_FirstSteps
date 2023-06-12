minimax(Pos, BestNextPos, Val) :-                     
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    best(NextPosList, BestNextPos, Val), !.

minimax(Pos, _, Val) :-                     % Pos has no successors
    utility(Pos, Val).

best([Pos], Pos, Val) :-                                % There is no more position to compare
    minimax(Pos, _, Val), !.

best([Pos1 | PosList], BestPos, BestVal) :-             % There are other positions to compare
    minimax(Pos1, _, Val1),
    best(PosList, Pos2, Val2),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).
    
betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    min_to_move(Pos0),                         % MIN to move in Pos0
    Val0 > Val1, !.                            % MAX prefers the greater value

betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    max_to_move(Pos0),                         % MAX to move in Pos0
    Val0 < Val1, !.                            % MIN prefers the lesser value

betterOf(_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0

min_to_move([o, _, _]).

max_to_move([x, _, _]).	

utility([o, win, _], 1).       % Previous player (MAX) has win.
utility([x, win, _], -1).      % Previous player (MIN) has win.
utility([_, draw, _], 0).


drawPos(_,Board) :-
    not( member(0, Board) ).

winPos(P, [X1, X2, X3, X4, X5, X6, X7, X8, X9]) :-
    equal(X1, X2, X3, P) ;    % 1st line
    equal(X4, X5, X6, P) ;    % 2nd line
    equal(X7, X8, X9, P) ;    % 3rd line
    equal(X1, X4, X7, P) ;    % 1st col
    equal(X2, X5, X8, P) ;    % 2nd col
    equal(X3, X6, X9, P) ;    % 3rd col
    equal(X1, X5, X9, P) ;    % 1st diag
    equal(X3, X5, X7, P).     % 2nd diag

equal(X, X, X, X).

move([X1, play, Board], [X2, win, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1, Board, NextBoard),
    winPos(X1, NextBoard), !.

move([X1, play, Board], [X2, draw, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1, Board, NextBoard),
    drawPos(X1,NextBoard), !.

move([X1, play, Board], [X2, play, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1, Board, NextBoard).

move_aux(P, [0|Bs], [P|Bs]).

move_aux(P, [B|Bs], [B|B2s]) :-
    move_aux(P, Bs, B2s).

nextPlayer(o,x).
nextPlayer(x,o).

render([X1, X2, X3, X4, X5, X6, X7, X8, X9]):-
	writerow([X1,X2,X3]),
	writeln("-----"),
	writerow([X4,X5,X6]),
	writeln("-----"),
	writerow([X7,X8,X9]).

writerow([0]):-writeln(" "),!.
writerow([A]):-writeln(A),!.
writerow([0|L]):-write(" "),write("|"),writerow(L),!.
writerow([A|L]):-write(A),write("|"),writerow(L).

% human plays with o and plays first

play:-
	freshBoard(B),
	aux_play([o,play,B]). % o starts

aux_play([_,draw,_]):-writeln("It's a draw").
aux_play([x,win,_]):-writeln("o wins").
aux_play([o,win,_]):-writeln("x wins").
aux_play([x,play,B]):-
	writeln("Computer plays: "),
	minimax([x,play,B],[P,S,B2],_),render(B2),
	aux_play([P,S,B2]).
aux_play([o,play,B]):-
	write("Human plays: "),flush_output,
	read(Pos),
	newBoard(Pos,B,o,B2),
	render(B2),
	(drawPos(_,B2),S=draw;winPos(o,B2),S=win;S=play),
	aux_play([x,S,B2]).


freshBoard([0,0,0,0,0,0,0,0,0]).

newBoard(1,[0|L],X,[X|L]):-!.
newBoard(N,[S|L],X,[S|L2]):-N2 is N-1,newBoard(N2,L,X,L2),!.
	


