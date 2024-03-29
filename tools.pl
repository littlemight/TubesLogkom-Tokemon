take([], 0,'') :- !.
take([C|_], 0, C) :- !.
take([_|T], Pos, C) :- NextPos is (Pos-1), take(T, NextPos, C), !.

reset :-
    retractall(inventory(_)),
    retractall(encounter(_)),
    retractall(status(_)),
    retractall(battle(_)),
    retractall(tokemon(_, _, _, _, _)),
    retractall(height(_)),
    retractall(width(_)),
    retractall(gym(_, _)),
    retractall(fence(_, _)),
    retractall(posPlayer(_, _)),
    retractall(status(_)),
    retractall(level(_, _)),
    retractall(special(_)).
