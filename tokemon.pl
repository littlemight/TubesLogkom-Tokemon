
/* tokemon(Id, NameTokemon, XPos, YPos, Health, Ownership) */
/* Ownership = 0 -- Roaming
   Ownership = 1 -- Sama player
   Kalo mati, retract aja
*/

/* maxHealth(NameTokemon, MaxHP) */
/* legendary(NameTokemon) */
/* normal(NamaTokemon) */
:- dynamic(tokemon/6).

/* DATABASE KENTANG */
legendary(fritz). 
legendary(gerald).
normal(tjie).
normal(yo).
normal(gay).

maxHealth(fritz, 666). 
maxHealth(gerald, 420). 
maxHealth(tjie, 69).
maxHealth(yo, 96). 
maxHealth(gay, 969).

type(fritz, fire).
type(gerald, leaves).
type(tjie, water).
type(yo, water).
type(gay, fire).
/* END OF DATABASE KENTANG */

/* TOKEMON SPAWNS */
initNormal(0) :- !.
initNormal(N) :-
    height(H),
    width(W),
    random(1, W, X),
    random(1, H, Y),
    findall(Tokemon, normal(Tokemon), ListTokemon),
    length(ListTokemon, LenListTokemon),
    random(0, LenListTokemon, Pick),
    take(ListTokemon, Pick, NameTokemon),
    maxHealth(NameTokemon, HealthTokemon),
    asserta(tokemon(N, NameTokemon, X, Y, HealthTokemon, 0)),
    NextN is N - 1,
    initNormal(NextN), !.
/* END OF TOKEMON SPAWNS */

/* TOKEMON ROAMS */
roamAllTokemon :-
    findall(Id, tokemon(Id, _, _, _, _, 0), ListTokemon),
    write(ListTokemon), nl, 
    updateListTokemon(ListTokemon).

updateListTokemon([]) :- !.
updateListTokemon([Id | Tail]) :-
    random(1, 5, RNG),
    decide(Id, RNG),
    updateListTokemon(Tail), !.

decide(Id, RNG) :-
    tokemon(Id, NameTokemon, _, _, _, _),
    (normal(NameTokemon) ->
        decideNormal(Id, RNG)
    ;   decideLegendary(Id, RNG)
    ).

decideNormal(Id, RNG) :-
    write(Id), write(' '), write(RNG), nl, 
    RNG =< 2, !, randomRoam(Id).

decideNormal(Id, _) :-
    moveTowardsPlayer(Id).

decideLegendary(Id, RNG) :-
    RNG =< 3, !, randomRoam(Id).

decideLegendary(Id, _) :-
    moveTowardsPlayer(Id).

randomRoam(Id) :-
    random(1, 5, RNG);
    (RNG =:= 1 ->
        wTokemon(Id)
    ; RNG =:= 2 ->
        aTokemon(Id)
    ; RNG =:= 3 ->
        sTokemon(Id)
    ;   dTokemon(Id)
    ).

moveTowardsPlayer(Id) :- 
    tokemon(Id, _, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon =:= XPlayer, YTokemon =:= YPlayer, !.

moveTowardsPlayer(Id) :- 
    tokemon(Id, _, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon =:= XPlayer, YTokemon < YPlayer, sTokemon(Id), !.

moveTowardsPlayer(Id) :- 
    tokemon(Id, _, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon =:= XPlayer, YTokemon > YPlayer, wTokemon(Id), !.

moveTowardsPlayer(Id) :- 
    tokemon(Id, _, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon < XPlayer, YTokemon =:= YPlayer, dTokemon(Id), !.

moveTowardsPlayer(Id) :- 
    tokemon(Id, _, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon > XPlayer, YTokemon =:= YPlayer, aTokemon(Id), !.

moveTowardsPlayer(Id) :- 
    tokemon(Id, _, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon > XPlayer, YTokemon > YPlayer,
    random(1, 3, RNG),
    (RNG =:= 1 ->
        aTokemon(Id)
    ;   wTokemon(Id)
    ),
    !.

moveTowardsPlayer(Id) :- 
    tokemon(Id, _, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon > XPlayer, YTokemon < YPlayer,
    random(1, 3, RNG),
    (RNG =:= 1 ->
        aTokemon(Id)
    ;   sTokemon(Id)
    ),
    !.

moveTowardsPlayer(Id) :- 
    tokemon(Id, _, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon < XPlayer, YTokemon > YPlayer,
    random(1, 3, RNG),
    (RNG =:= 1 ->
        dTokemon(Id)
    ;   wTokemon(Id)
    ),
    !.

moveTowardsPlayer(Id) :- 
    tokemon(Id, _, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon < XPlayer, YTokemon < YPlayer,
    random(1, 3, RNG),
    (RNG =:= 1 ->
        dTokemon(Id)
    ;   sTokemon(Id)
    ),
    !.
/* END OF TOKEMON ROAMS */

/* TOKEMON MOVEMENTS */
wTokemon(Id) :-
    retract(tokemon(Id, NameTokemon, X, Y, Health, Ownership)),
    YNew is Y - 1,
    (Y > 1, \+(fence(X, YNew)) ->
        assertz(tokemon(Id, NameTokemon, X, YNew, Health, Ownership))
    ;   assertz(tokemon(Id, NameTokemon, X, Y, Health, Ownership))
    ),
    !.

aTokemon(Id) :-
    retract(tokemon(Id, NameTokemon, X, Y, Health, Ownership)),
    XNew is X - 1,
    (X > 1, \+(fence(XNew, Y)) ->
        assertz(tokemon(Id, NameTokemon, XNew, Y, Health, Ownership))
    ;   assertz(tokemon(Id, NameTokemon, X, Y, Health, Ownership))
    ),
    !.

sTokemon(Id) :-
    retract(tokemon(Id, NameTokemon, X, Y, Health, Ownership)),
    height(YMax), YNew is Y + 1,
    (Y < YMax, \+(fence(X, YNew)) ->
        assertz(tokemon(Id, NameTokemon, X, YNew, Health, Ownership))
    ;   assertz(tokemon(Id, NameTokemon, X, Y, Health, Ownership))
    ),
    !.

dTokemon(Id) :-
    retract(tokemon(Id, NameTokemon, X, Y, Health, Ownership)),
    width(XMax), XNew is X + 1,
    (X < XMax, \+(fence(XNew, Y)) ->
        assertz(tokemon(Id, NameTokemon, XNew, Y, Health, Ownership))
    ;   assertz(tokemon(Id, NameTokemon, X, Y, Health, Ownership))
    ),
    !.
/* END OF TOKEMON MOVEMENTS */