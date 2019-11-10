
/* tokemon(NameTokemon, XPos, YPos, Health, Ownership) */
/* Ownership = 0 -- Roaming
   Ownership = 1 -- Sama player
   Kalo mati, retract aja
*/

/* maxHealth(NameTokemon, MaxHP) */
/* legendary(NameTokemon) */
/* normal(NamaTokemon) */
/* damage(NamaTokemon, JumlahDamage) */
/* skill(NamaTokemon,NamaSkill,JumlahDamage) */
:- dynamic(tokemon/5).

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

damage(fritz,100).
damage(gerald,120).
damage(tjie,30).
damage(yo,20).
damage(gay,25).

skill(fritz,a,200).
skill(gerald,b,250).
skill(tjie,c,100).
skill(yo,d,80).
skill(gay,e,90).
/* END OF DATABASE KENTANG */

/* TOKEMON SPAWNS */
normalNotSpawned(Tokemon) :- normal(Tokemon), \+(tokemon(Tokemon, _, _, _, _)).
legendaryNotSpawned(Tokemon) :- legendary(Tokemon), \+(tokemon(Tokemon, _, _, _, _)).

initNormal(0) :- !.
initNormal(N) :-
    height(H),
    width(W),
    random(1, W, X),
    random(1, H, Y),
    findall(Tokemon, normalNotSpawned(Tokemon), ListTokemon),
    length(ListTokemon, LenListTokemon),
    random(0, LenListTokemon, Pick),
    take(ListTokemon, Pick, NameTokemon),
    maxHealth(NameTokemon, HealthTokemon),
    asserta(tokemon(NameTokemon, X, Y, HealthTokemon, 0)),
    NextN is N - 1,
    initNormal(NextN), !.
/* END OF TOKEMON SPAWNS */

/* TOKEMON ROAMS */
roamAllTokemon :-
    findall(Tokemon, tokemon(Tokemon, _, _, _, 0), ListTokemon),
    write(ListTokemon), nl, 
    updateListTokemon(ListTokemon).

updateListTokemon([]) :- !.
updateListTokemon([Tokemon | Tail]) :-
    random(1, 5, RNG),
    decide(Tokemon, RNG),
    updateListTokemon(Tail), !.

decide(Tokemon, RNG) :-
    (normal(Tokemon) ->
        decideNormal(Tokemon, RNG)
    ;   decideLegendary(Tokemon, RNG)
    ).

decideNormal(Tokemon, RNG) :-
    RNG =< 2, !, randomRoam(Tokemon).

decideNormal(Tokemon, _) :-
    moveTowardsPlayer(Tokemon).

decideLegendary(Tokemon, RNG) :-
    RNG =< 3, !, randomRoam(Tokemon).

decideLegendary(Tokemon, _) :-
    moveTowardsPlayer(Tokemon).

randomRoam(Tokemon) :-
    random(1, 5, RNG);
    (RNG =:= 1 ->
        wTokemon(Tokemon)
    ; RNG =:= 2 ->
        aTokemon(Tokemon)
    ; RNG =:= 3 ->
        sTokemon(Tokemon)
    ;   dTokemon(Tokemon)
    ).

moveTowardsPlayer(Tokemon) :- 
    tokemon(Tokemon, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon =:= XPlayer, YTokemon =:= YPlayer, !.

moveTowardsPlayer(Tokemon) :- 
    tokemon(Tokemon, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon =:= XPlayer, YTokemon < YPlayer, sTokemon(Tokemon), !.

moveTowardsPlayer(Tokemon) :- 
    tokemon(Tokemon, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon =:= XPlayer, YTokemon > YPlayer, wTokemon(Tokemon), !.

moveTowardsPlayer(Tokemon) :- 
    tokemon(Tokemon, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon < XPlayer, YTokemon =:= YPlayer, dTokemon(Tokemon), !.

moveTowardsPlayer(Tokemon) :- 
    tokemon(Tokemon, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon > XPlayer, YTokemon =:= YPlayer, aTokemon(Tokemon), !.

moveTowardsPlayer(Tokemon) :- 
    tokemon(Tokemon, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon > XPlayer, YTokemon > YPlayer,
    random(1, 3, RNG),
    (RNG =:= 1 ->
        aTokemon(Tokemon)
    ;   wTokemon(Tokemon)
    ),
    !.

moveTowardsPlayer(Tokemon) :- 
    tokemon(Tokemon, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon > XPlayer, YTokemon < YPlayer,
    random(1, 3, RNG),
    (RNG =:= 1 ->
        aTokemon(Tokemon)
    ;   sTokemon(Tokemon)
    ),
    !.

moveTowardsPlayer(Tokemon) :- 
    tokemon(Tokemon, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon < XPlayer, YTokemon > YPlayer,
    random(1, 3, RNG),
    (RNG =:= 1 ->
        dTokemon(Tokemon)
    ;   wTokemon(Tokemon)
    ),
    !.

moveTowardsPlayer(Tokemon) :- 
    tokemon(Tokemon, XTokemon, YTokemon, _, _), posPlayer(XPlayer, YPlayer),
    XTokemon < XPlayer, YTokemon < YPlayer,
    random(1, 3, RNG),
    (RNG =:= 1 ->
        dTokemon(Tokemon)
    ;   sTokemon(Tokemon)
    ),
    !.
/* END OF TOKEMON ROAMS */

/* TOKEMON MOVEMENTS */
wTokemon(Tokemon) :-
    retract(tokemon(Tokemon, X, Y, Health, Ownership)),
    YNew is Y - 1,
    (Y > 1, \+(fence(X, YNew)) ->
        assertz(tokemon(Tokemon, X, YNew, Health, Ownership))
    ;   assertz(tokemon(Tokemon, X, Y, Health, Ownership))
    ),
    !.

aTokemon(Tokemon) :-
    retract(tokemon(Tokemon, X, Y, Health, Ownership)),
    XNew is X - 1,
    (X > 1, \+(fence(XNew, Y)) ->
        assertz(tokemon(Tokemon, XNew, Y, Health, Ownership))
    ;   assertz(tokemon(Tokemon, X, Y, Health, Ownership))
    ),
    !.

sTokemon(Tokemon) :-
    retract(tokemon(Tokemon, X, Y, Health, Ownership)),
    height(YMax), YNew is Y + 1,
    (Y < YMax, \+(fence(X, YNew)) ->
        assertz(tokemon(Tokemon, X, YNew, Health, Ownership))
    ;   assertz(tokemon(Tokemon, X, Y, Health, Ownership))
    ),
    !.

dTokemon(Tokemon) :-
    retract(tokemon(Tokemon, X, Y, Health, Ownership)),
    width(XMax), XNew is X + 1,
    (X < XMax, \+(fence(XNew, Y)) ->
        assertz(tokemon(Tokemon, XNew, Y, Health, Ownership))
    ;   assertz(tokemon(Tokemon, X, Y, Health, Ownership))
    ),
    !.
/* END OF TOKEMON MOVEMENTS */

status :-
    findall(Tokemon, tokemon(Tokemon,_,_,_,1), ListTokemon),
    write('Tokemon kamu : '),
    nl,
    printStatus(ListTokemon).
    
printStatus([]) :- !.

printStatus([Tokemon|Tail]) :-
    tokemon(Tokemon, _, _, HP, _),
    type(Tokemon, Type),
    write(Tokemon),
    nl,
    write('Health : '),
    write(HP),
    nl,
    write('Type : '),
    write(Type),
    nl,
    nl,
    printStatus(Tail).

attack :-
    battle(TokemonP),
    encounter(Enemy),
    (
        type(TokemonP,fire),type(Enemy,leaves) ->
            damage(TokemonP,Atk),
            AtkAtribut is Atk + Atk/2
        ; type(TokemonP,leaves),type(Enemy,water) ->
            damage(TokemonP,Atk),
            AtkAtribut is Atk + Atk/2
        ; type(TokemonP,water),type(Enemy,water) ->
            damage(TokemonP,Atk),
            AtkAtribut is Atk + Atk/2
        ; damage(TokemonP,Atk),
        AtkAtribut is Atk
    ),
    tokemon(Enemy,_,_,HP,_),
    HPnew is HP - AtkAtribut,
    (
        HPnew =< 0 ->
        write('Musuh kalah.'),
        nl,
        write('Tangkep ga?'),
        nl,
        retract(tokemon(Enemy,_,_,_,_))
        ; retract(tokemon(Enemy,X,Y,_,Owner)),
        assertz(tokemon(Enemy,X,Y,HPnew,Owner)),
        write('Musuh kena damage')
    ).

specialAttack :-
    battle(TokemonP),
    encounter(Enemy),
    (
        type(TokemonP,fire),type(Enemy,leaves) ->
            skill(TokemonP,Jurus,Atk),
            AtkAtribut is Atk + Atk/2
        ; type(TokemonP,leaves),type(Enemy,water) ->
            skill(TokemonP,Jurus,Atk),
            AtkAtribut is Atk + Atk/2
        ; type(TokemonP,water),type(Enemy,water) ->
            skill(TokemonP,Jurus,Atk),
            AtkAtribut is Atk + Atk/2
        ; damage(TokemonP, Atk), 
        AtkAtribut is Atk
    ),
    tokemon(Enemy,_,_,HP,_),
    HPnew is HP - AtkAtribut,
    (
        HPnew =< 0 ->
        write('Musuh kalah.'),
        nl,
        write('Tangkep ga?'),
        nl,
        retract(tokemon(Enemy,_,_,_,_))
        ; retract(tokemon(Enemy,X,Y,_,Owner)),
        assertz(tokemon(Enemy,X,Y,HPnew,Owner)),
    
        write('Musuh kena Special Attack')
    ).


enemyAttack :-
    battle(TokemonP),
    encounter(Enemy),
    (
        type(Enemy,fire),type(TokemonP,leaves) ->
            damage(Enemy,Atk),
            AtkAtribut is Atk + Atk/2
        ; type(Enemy,leaves),type(TokemonP,water) ->
            damage(Enemy,Atk),
            AtkAtribut is Atk + Atk/2
        ; type(Enemy,water),type(TokemonP,water) ->
            damage(Enemy,Atk),
            AtkAtribut is Atk + Atk/2
        ; damage(Enemy,Atk),
        AtkAtribut is Atk
    ),
    tokemon(TokemonP,_,_,HP,_),
    HPnew is HP - AtkAtribut,
    (
        HPnew =< 0 ->
            write('Tokemon kita kalah.'),
            nl,
            write('Pilih Tokemon lagi'),
            nl,
            retract(tokemon(TokemonP,_,_,_,_)),
            retract(inventory(TokemonP)),
            printInventory
        ; retract(tokemon(TokemonP,X,Y,_,Owner)),
        assertz(tokemon(TokemonP,X,Y,HPnew,Owner)),
        write('Kita kena damage')
    ).