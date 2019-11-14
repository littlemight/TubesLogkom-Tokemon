:- include('map.pl').
:- include('player.pl').


save(_) :-
 \+status(_),
 write('Kamu belum main nih, mau save apaan??'), nl, !.

% save(_) :-
%     saved(_),
%     write('File sudah disave hehe'), nl, !.

save(FileName) :-
    % \+ saved(_),
        tell(FileName),
            writeMap,
            writeInventory,
        told, !. 
    % !, saved(1).


writeMap:-
 forall(height(X),(write('height('), write(X))),
 forall(width(X),(write('width('),write(X))),
 forall(gym(X,Y),(write('gym)('),write(X,Y))),
 forall(fence(X,Y),(write('fence)('),write(X,Y))),
 forall(posPlayer(X,Y),(write('posPlayer)('),write(X,Y))), !.

writeInventory:-
 forall(inventory(X),(write('inventory('),write(X))),
 forall(status(X),(write('status('),write(X))), !.

loadGame(_) :-
 status(_),
 write('You are still playing a game, you cannot load another game now, better save it first then load another later !'), nl, !.

loadGame(FileName):-
 \+file_exists(FileName),
 write('File not found'), nl, !.
    
loadGame(FileName):-
 open(FileName, read, Stream),
        readFileLines(Stream,Lines),
    close(Stream),
    assertaLine(Lines), 
    asserta(status(1)), !.

assertaLine([]) :- !.

assertaLine([X|L]):-
 asserta(X),
 assertaLine(L), !.


readFileLines(Stream,[]) :-
    at_end_of_stream(Stream).

readFileLines(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    readFileLines(Stream,L).