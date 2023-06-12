% Possible solution for the three cards game (exercise)

minimax(Pos, BestNextPos, Val,D) :-
    D>0,D2 is D-1,                     
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    player_thinking(Pos,P), % not assuming alternating moves
    best(NextPosList, P, BestNextPos, Val,D2), !.

% Pos has no successors or depth bound found
minimax(Pos, _, Val,_) :-               
    utility(Pos, Val).

% There is no more position to compare
best([Pos], _, Pos, Val,D) :-           
    minimax(Pos, _, Val,D), !.
% There are other positions to compare
best([Pos1 | PosList], P, BestPos, BestVal,D) :-   
    minimax(Pos1, _, Val1,D),
    best(PosList, P, Pos2, Val2,D),
    betterOf(P, Pos1, Val1, Pos2, Val2, BestPos, BestVal).
    
betterOf(P, Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    thinker(P),                         % MIN to move in Pos0
    Val0 > Val1, !.                            % MAX prefers the greater value

betterOf(P, Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    opponent(P),                         % MAX to move in Pos0
    Val0 < Val1, !.                            % MIN prefers the lesser value

betterOf(_,_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0

player_thinking([P,_,_,_],P).

thinker(x).
opponent(o).

utility([o, win, _, _], 1).       % Previous player (MAX) has win.
utility([x, win, _, _], -1).      % Previous player (MIN) has win.
utility([_,play,_, _],0).

winPos(P,[h(_,[]),h(_,[])],T):-del(T,p(P,C),[p(P2,C2)]),outranks(C,C2).

% In the winning move P is the player who lost
move([X1, play, Hands,Table], [P,win, NextHands,[]]) :-
    move_aux(X1,Hands,Table,NextHands,Table2),
    otherPlayer(X1,X2),
    (winPos(X1,NextHands,Table2),P=X2;winPos(X2,NextHands,Table2),P=X1), !.
move([X1, play, Hands,Table], [X2,play, NextHands,NextTable]) :-
    move_aux(X1,Hands,Table,NextHands,Table2),
    nextPlayer(Table2,X2,NextTable).

%move_aux(Player,Hands,Table,NextHands,NextTable).
move_aux(P,Hands,[],[h(P,H2)|Hands2],[p(P,C)]):-
    del(Hands,h(P,H),Hands2),del(H,C,H2).
move_aux(P,Hands,[p(P2,C2)],[h(P,H2)|Hands2],[p(P,C),p(P2,C2)]):-
    del(Hands,h(P,H),Hands2),del(H,C,H2),innext2(C,C2).
move_aux(P,Hands,[p(P2,C2)],[h(P,H2)|Hands2],[p(P,C),p(P2,C2)]):-
    del(Hands,h(P,H),Hands2),del(H,C,H2),not((member(X,H),innext2(X,C2))).

del([X|L],X,L).
del([Y|L],X,[Y|L2]):-X\==Y,del(L,X,L2).

innext2(A,B):-rank(A,RA),rank(B,RB),(RB+1=:=RA;RB+2=:=RA).

nextPlayer([p(P1,C)],P2,[p(P1,C)]):-otherPlayer(P1,P2).
nextPlayer([p(P1,C1),p(P2,C2)],P1,[]):-outranks(C1,C2).
nextPlayer([p(P1,C1),p(P2,C2)],P2,[]):-outranks(C2,C1).

otherPlayer(x,o).
otherPlayer(o,x).

outranks(A,B):-rank(A,RA),rank(B,RB),RA>RB.

rank(C,N):-number(C),N is C-1,!.
rank(j,7). rank(q,8). rank(k,9). rank(a,10).

render(g(H,T)):-
	write("Hands: "),writeln(H),
	write("Table: "),writeln(T),
	writeln("----").

% human plays with o and plays first

play(D):-
	deal(H),
	render(g(H,[])),
	nocards(x,H,NC),
	aux_play([o,play,H,[]],NC,D). % o starts

aux_play([x,win,_,_],_,_):-writeln("o wins").
aux_play([o,win,_,_],_,_):-writeln("x wins").
aux_play([x,play,H,T],NC,D):-
	writeln("Computer plays: "),
%	imagine_game(x,H,NC,Hi),
%	minimax([x,play,Hi,T],[P,S,H2i,T2],Val,D),
	minimax([x,play,H,T],[P,S,H2,T2],Val,D),
%	unimagine_game(x,H2i,H,H2),
	write("Val: "),writeln(Val),
	render(g(H2,T2)),
	aux_play([P,S,H2,T2],NC,D).
aux_play([o,play,H,T],NC,D):-
	write("Human plays: "),flush_output,
	read(C),
	newPos(C,H,T,o,H2,T2),
	nextPlayer(T2,P2,T3),
	otherPlayer(P2,P1),
	render(g(H2,T3)),
	% next player is P2 unless game ended with a win of either P2 or P1.
	(winPos(P2,H2,T2),P3=P1,S=win;winPos(P1,H2,T2),P3=P2,S=win;P3=P2,S=play),
	aux_play([P3,S,H2,T3],[C|NC],D).


%deal([h(x,[4,7,q]),h(o,[a,k,3])]).
%deal([h(x,[k,3,7]),h(o,[a,2,4])]). % 2,4,a computer should win
%deal([h(x,[4,k,q]),h(o,[5,7,2])]). % 2,5,7 computer should be declared winner

deal([h(x,H1),h(o,H2)]):-
    ranks(D),
	delrand(3,D,H1,D2),delrand(3,D2,H2,_).

ranks([2,3,4,5,6,7,j,q,k,a]).

delrand(0,L,[],L):-!.
delrand(N,L,[X|RD],L3):-length(L,NL), P is random(NL), delpos(P,L,X,L2),N2 is N-1, 
    delrand(N2,L2,RD,L3).

delpos(0,[X|L],X,L).
delpos(N,[Y|L],X,[Y|L2]):-N2 is N-1, delpos(N2,L,X,L2).

nocards(P,H,NC):-member(h(P,NC),H).

newPos(C,H,T,P,[h(P,HP2)|H2],[p(P,C)|T]):-del(H,h(P,HP),H2),del(HP,C,HP2).

imagine_game(P,Hands,NC,[h(P2,H2)|Hands2]):-
    otherPlayer(P,P2),del(Hands,h(P2,H),Hands2),imagine_hand(H,NC,H2).

imagine_hand(H,NC,H2):-length(H,N),length(H2,N),filltop(NC,H2).

filltop(NC,H):-ranks(R),reverse(R,RR),deleteall(NC,RR,RR2),aux_filltop(H,RR).

deleteall([],L,L).
deleteall([X|R],L,L3):-delete(L,X,L2),deleteall(R,L2,L3).

aux_filltop([],_).
aux_filltop([X|H],[X|R]):-aux_filltop(H,R).

unimagine_game(P,Handsi,Hands,[h(P2,H)|Handsr]):-
    member(h(P2,H),Hands),
    otherPlayer(P,P2),del(Handsi,h(P2,_),Handsr).


