:- dynamic(inventory/1). /* inventory(Tokemon), Tokemon ada di inventory player */
:- dynamic(encounter/1). /* encounter(Tokemon), sekarang lagi ketemu Tokemon apa */
:- dynamic(battle/1). /* battle(Tokemon), sekarang Tokemon apa yang kita pilih buat battle */
:- dynamic(hasHealed/0).

starterNotSpawned(Tokemon) :- starter(Tokemon), \+(tokemon(Tokemon, _, _, _, _)).

initPlayer :-
    /* Get Player Position */
    height(H),
    width(W),
    random(1, H, YPlayer),
    random(1, W, XPlayer),
    asserta(posPlayer(XPlayer, YPlayer)),
    
    /* Pick Unspawned Tokemon */
    findall(Tokemon, starterNotSpawned(Tokemon), ListTokemon),
    length(ListTokemon, LenListTokemon),
    random(0, LenListTokemon, Pick),
    take(ListTokemon, Pick, NameTokemon),
    maxHealth(NameTokemon, HealthTokemon),
    asserta(inventory(NameTokemon)),
    asserta(tokemon(NameTokemon, 0, 0, HealthTokemon, 1)). /* 1 = dimiliki player, (0, 0) soalnya posisinya ga penting) */
    

writeNabrak :- write('Ouch, you hit a fence.'), nl.
writeNorth :- write('You went North.'), nl.
writeSouth :- write('You went South.'), nl.
writeEast :- write('You went East.'), nl.
writeWest :- write('You went West.'), nl.

w :- \+status(roam), write('Sorry! You cannot do that for now. '), nl, !, fail.
w :-
  retract(posPlayer(X, Y)),
  YNew is Y - 1,
  (
    Y > 1, \+(fence(X, YNew)) ->
    asserta(posPlayer(X, YNew)), writeNorth, write('...'), nl, sleep(0.5), roamAllTokemon,
    tokemonTingle, nl, sleep(0.5), checkEncounter
    ;asserta(posPlayer(X, Y)), writeNabrak
  ), !.

a :- \+status(roam),  write('Sorry! You cannot do that for now. '),!, fail.
a :-
  retract(posPlayer(X, Y)),
  XNew is X - 1,
  (
    X > 1, \+(fence(XNew, Y)) ->
    asserta(posPlayer(XNew, Y)), writeWest, write('...'), nl, sleep(0.5), roamAllTokemon,
    tokemonTingle, nl, sleep(0.5), checkEncounter
    ;asserta(posPlayer(X, Y)), writeNabrak
  ),
  !.

s :- \+status(roam), write('Sorry! You cannot do that for now. '),!, fail.
s :-
  retract(posPlayer(X, Y)),
  height(YMax),
  YNew is Y + 1,
  (
    Y < YMax, \+ (fence(X, YNew)) ->
    asserta(posPlayer(X, YNew)), writeSouth, write('...'), nl, sleep(0.5), roamAllTokemon,
    tokemonTingle, nl, sleep(0.5), checkEncounter
    ;asserta(posPlayer(X, Y)), writeNabrak
  ),
  !.

d :- \+status(roam), write('Sorry! You cannot do that for now. '),!, fail.
d :-
  retract(posPlayer(X, Y)),
  width(XMax),
  XNew is X + 1,
  (
    X < XMax, \+(fence(XNew, Y)) ->
      asserta(posPlayer(XNew, Y)), writeEast, write('...'), nl, sleep(0.5), roamAllTokemon,
      tokemonTingle, nl, sleep(0.5), checkEncounter
    ; asserta(posPlayer(X, Y)), writeNabrak
  ),
  !.

sizeInventory(Size) :-
    findall(Tokemon, inventory(Tokemon), ListTokemon),
    length(ListTokemon, Size).

pick(_) :- \+(encounter(_)), write('You\'re not in battle!'), nl, !.
pick(Tokemon) :- battle(Tokemon), write('You already picked that Tokemon!'), nl, !.
pick(Tokemon) :- 
    (inventory(Tokemon) ->
        retractall(battle(_)),
        format('You : \"~w I choose you!\"', [Tokemon]), nl, nl,
        encounter(Enemy),
        printBattleStatus(Tokemon, Enemy),
        asserta(battle(Tokemon))
    ;   write('You don\'t have that Tokemon!'), nl
    ).


isInventoryFull :-
    sizeInventory(Size),
    NewSize is Size + 1,
    (NewSize > 6), !.

addTokemon(Tokemon) :-
    asserta(inventory(Tokemon)),
    retract(tokemon(Tokemon, X, Y, Health, _)),
    asserta(tokemon(Tokemon, X, Y, Health, 1)).


drop(Tokemon) :-
    (inventory(Tokemon) ->
        retract(inventory(Tokemon)),
        retract(tokemon(Tokemon, _, _, Health, _)),
        posPlayer(XPlayer, YPlayer),
        asserta(tokemon(Tokemon, XPlayer, YPlayer, Health, 0)),
        (
            \+inventory(_) ->
            kalah
        )
    ; write('You cannot drop a Tokemon you do not have! '),nl
    ).

checkEncounter :-
    posPlayer(XPlayer, YPlayer),
    findall(Tokemon, tokemon(Tokemon, XPlayer, YPlayer, _, 0), ListTokemon),
    length(ListTokemon, LenListTokemon),
    LenListTokemon =:= 0, !. 

checkEncounter :-
    posPlayer(XPlayer, YPlayer),
    findall(Tokemon, tokemon(Tokemon, XPlayer, YPlayer, _, 0), ListTokemon),
    length(ListTokemon, LenListTokemon),
    LenListTokemon > 0, 
    random(0, LenListTokemon, Pick),
    take(ListTokemon, Pick, NameTokemon),
    write('A wild Tokemon appears!'), nl,
    write('It is '),
    write(NameTokemon),
    write('!'),nl,
    write('Fight or Run?'),nl,
    retract(status(roam)),
    asserta(status(battle)),
    asserta(encounter(NameTokemon)), !.

run :- \+(encounter(_)), write('You\'re not facing a Tokemon!'), nl, !.
run :-
    encounter(Tokemon),
    \+(battle(_)),
    random(1, 101, RNG),
    (RNG =< 20 ->
        write('You successfully escaped the Tokemon!'), nl, retract(encounter(Tokemon)),
        retract(status(battle)),
        asserta(status(roam)),
        (
            special(Tokemon) ->
            retract(special(Tokemon))
        )
    ;   write('You failed to run!'), nl, decideEnemyBattle
    ), !.

run :-
    encounter(Tokemon),
    \+(battle(_)),
    random(1, 101, RNG),
    (RNG =< 20 ->
        write('You successfully escaped the Tokemon!'), nl, retract(encounter(Tokemon)),
        retract(status(battle)),
        asserta(status(roam)),
        (
            special(Tokemon) ->
            retract(special(Tokemon))
        )
    ;   write('You failed to run!'), nl, decideEnemyBattle
    ).

run :-
    encounter(Tokemon),
    battle(TokemonPlayer),
    random(1, 101, RNG),
    (RNG =< 10 ->
        write('You successfully escaped the Tokemon!'), nl, retract(encounter(Tokemon)),
        retract(battle(TokemonPlayer)),
        retract(status(battle)),
        asserta(status(roam)),
        (
            special(Tokemon) ->
            retract(special(Tokemon))
        ),
        (
            special(TokemonPlayer) ->
            retract(special(TokemonPlayer))
        )
    ;   write('You failed to run!'), nl, fight
    ), !.

fight :- \+(encounter(_)), write('You\'re not in battle!'), nl, !.
fight :- battle(_), write('Proceed with the battle!'), nl, !.
fight :-
    write('Choose your Tokemon!'), nl,
    write('Available Tokemons: '), printInventory, nl.

printInventory :-
    findall(Tokemon, inventory(Tokemon), ListTokemon),
    write(ListTokemon).

heal :- 
    posPlayer(XPlayer, YPlayer), 
    \+(gym(XPlayer, YPlayer)),
    write('You are not in a gym! Go to a gym if you want to heal your tokemon :] '),
    nl,
    !.

heal :-
    posPlayer(XPlayer, YPlayer),
    gym(XPlayer, YPlayer),
    (hasHealed(XPlayer, YPlayer) ->
        write('You already used this gym!')
    ;   asserta(hasHealed(XPlayer, YPlayer)),
        findall(Tokemon, inventory(Tokemon), ListTokemon),
        healList(ListTokemon),
        write('Your Tokemons have been healed!'),
        nl,
        status,
        !
    ), !.
    
healList([]):- !.
healList([Tokemon|Tail]) :-
    maxHealth(Tokemon, MaxHP),
    multiplier(Tokemon, Multiplier),
    MaxHPByLvl is Multiplier*MaxHP,
    retract(tokemon(Tokemon, X, Y, _, _)),
    asserta(tokemon(Tokemon, X, Y, MaxHPByLvl, 1)),
    healList(Tail).
