
saveGame(_) :-
 \+status(_),
 write('You must play the game to save'), nl, !.

saveGame(_) :- status(battle), write('You cannot save the game while you are in a fight!'), nl, !.

saveGame(_) :- battle(_), write('You cannot save the game while you are in a fight!'), nl, !.

saveGame(FileName) :-
        tell(FileName),
            writeMap,nl,
            writeInventory,
            writeLvl,
        told, !. 


writeMap:-
 forall(height(X),(write('height('), write(X),write(').'),nl)),
 forall(width(X),(write('width('),write(X),write(').'),nl)),
 forall(gym(X,Y),(write('gym('),write(X),write(','),write(Y),write(').'),nl)),
 forall(fence(X,Y),(write('fence('),write(X),write(','),write(Y),write(').'),nl)),
 forall(status(X),(write('status('),write(X),write(').'),nl)),
 forall(posPlayer(X,Y),(write('posPlayer('),write(X),write(','),write(Y),write(').'))), !.

writeInventory:-
 forall(tokemon(A,B,C,D,E),(write('tokemon('),write(A),write(','),write(B),write(','),write(C),write(','),write(D),write(','),write(E),write(').'),nl)),
 forall(inventory(X),(write('inventory('),write(X),write(').'),nl)),
 forall(hasHealed(X,Y),(write('hasHealed('),write(X),write(','),write(Y),write(').'),nl)),!.

writeLvl :-
    forall(level(Tokemon, Exp), (write('level('), write(Tokemon), write(','), write(Exp), write(').'), nl)), !.

loadGame(_) :- status(_), write('You cannot load another game now!'),nl,!.

loadGame(FileName):-
 \+file_exists(FileName),
 write('File not found :) .'), nl, !.
    
loadGame(FileName):-
 reset, 
 open(FileName, read, Stream),
        readFileLines(Stream,Lines),
    close(Stream), assertaLine(Lines), asserta(init(1)), !.

assertaLine([]) :- !.

assertaLine([X|L]):-
 asserta(X), assertaLine(L), !.

readFileLines(Stream,[]) :-
    at_end_of_stream(Stream).

readFileLines(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream), read(Stream,X), readFileLines(Stream,L).