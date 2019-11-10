:- dynamic(inventory/1). /* inventory(Tokemon), Tokemon ada di inventory player */
:- dynamic(encounter/1). /* encounter(Tokemon), sekarang lagi ketemu Tokemon apa */
:- dynamic(battle/1). /* battle(Tokemon), sekarang Tokemon apa yang kita pilih buat battle */

initPlayer :-
    /* Get Player Position */
    height(H),
    width(W),
    random(1, H, YPlayer),
    random(1, W, XPlayer),
    asserta(posPlayer(XPlayer, YPlayer)),
    
    /* Pick Unspawned Tokemon */
    findall(Tokemon, normalNotSpawned(Tokemon), ListTokemon),
    length(ListTokemon, LenListTokemon),
    random(0, LenListTokemon, Pick),
    take(ListTokemon, Pick, NameTokemon),
    maxHealth(NameTokemon, HealthTokemon),
    asserta(inventory(NameTokemon)),
    asserta(tokemon(NameTokemon, 0, 0, HealthTokemon, 1)). /* 1 = dimiliki player, (0, 0) soalnya posisinya ga penting) */

sizeInventory(Size) :-
    findall(Tokemon, inventory(Tokemon), ListTokemon),
    length(ListTokemon, Size).

pick(Tokemon) :- 
    (inventory(Tokemon) ->
        write('yay bisa'),
        asserta(battle(Tokemon))
    ;   write('You don\'t have that Tokemon!')
    ).


isInventoryFull :-
    sizeInventory(Size),
    NewSize is Size + 1,
    (NewSize > 6), !.

addTokemon(Tokemon) :-
    asserta(inventory(Tokemon)),
    retract(tokemon(Tokemon, X, Y, Health, _)),
    asserta(tokemon(Tokemon, X, Y, Health, 1)).

dropTokemon(Tokemon) :-
    \+(inventory(Tokemon)), !, fail.

dropTokemon(Tokemon) :-
    inventory(Tokemon),
    retract(tokemon(Tokemon, _, _, Health, _)),
    posPlayer(XPlayer, YPlayer),
    asserta(tokemon(Tokemon, XPlayer, YPlayer, Health, 0)).

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
    write('Fight or Run?'), nl,
    retract(status(roam)),
    asserta(status(battle)),
    asserta(encounter(NameTokemon)), !.

run :-
    encounter(Tokemon),
    battle(TokemonPlayer),
    random(1, 101, RNG),
    (RNG =< 40 ->
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
    ).

fight :-
    write('Choose your Tokemon!'), nl, nl,
    write('Available Tokemons: '), printInventory, nl.

printInventory :-
    findall(Tokemon, inventory(Tokemon), ListTokemon),
    write(ListTokemon).

heal :- 
    posPlayer(XPlayer, YPlayer), 
    \+(gym(XPlayer, YPlayer)),
    write("You are not in a gym"),
    nl,
    !.
heal :-
    findall(Tokemon, inventory(Tokemon), ListTokemon),
    healList(ListTokemon), !.
healList([]):- !.
healList([Tokemon|Tail]) :-
    maxHealth(Tokemon, MaxHP),
    retract(tokemon(Tokemon, X, Y, _, _)),
    asserta(tokemon(Tokemon, X, Y, MaxHP, 1)),
    healList(Tail).