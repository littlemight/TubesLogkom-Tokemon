:- include('tools.pl').

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

/* special(NamaTokemon) */
:- dynamic(tokemon/5). 
:- dynamic(special/1). /* special(Tokemon), Tokemon udah pake special atau belum */

/* DATABASE KENTANG */
legendary(bangkumon).
legendary(mejamon).
legendary(zhafransyah).
legendary(vegan).
normal(fabian).
normal(jones).
normal(mitel).
normal(yoga).
normal(arip).
normal(laron).
normal(azong).
normal(tudecu).
normal(pilbet).
normal(jopan).

maxHealth(bangkumon, 200).
maxHealth(mejamon, 500).
maxHealth(zhafransyah, 1000).
maxHealth(vegan, 90).
maxHealth(fabian, 150).
maxHealth(jones, 75).
maxHealth(mitel, 120).
maxHealth(yoga, 150).
maxHealth(arip, 100).
maxHealth(laron, 1).
maxHealth(azong, 80).
maxHealth(tudecu, 50).
maxHealth(pilbet, 90).
maxHealth(jopan, 130).

type(bangkumon, fire).
type(mejamon, water).
type(zhafransyah, fire).
type(vegan, leaves).
type(fabian, water).
type(jones, leaves).
type(mitel, fire).
type(yoga, leaves).
type(arip, leaves).
type(laron, fire).
type(azong, water).
type(tudecu, leaves).
type(pilbet, fire).
type(jopan, fire).

damage(bangkumon, 35).
damage(mejamon, 10).
damage(zhafransyah, 50).
damage(vegan, 10).
damage(fabian, 15).
damage(jones, 20).
damage(mitel, 10).
damage(yoga, 25).
damage(arip, 30).
damage(laron, 1).
damage(azong, 20).
damage(tudecu, 50).
damage(pilbet, 15).
damage(jopan, 15).

skill(bangkumon, patah, 100).
skill(mejamon, kebalik, 15).
skill(zhafransyah, ruqyah, 150).
skill(vegan, capcay, 1000).
skill(fabian, berdoa, 50).
skill(jones, breakdance, 35).
skill(mitel, danusan, 75).
skill(yoga, muntah, 50).
skill(arip, par, 35).
skill(laron, sampah, 2).
skill(azong, renang, 25).
skill(tudecu, nyasar, 60).
skill(pilbet, flamethrower, 20).
skill(jopan, kentut, 55).

/* END OF DATABASE KENTANG */

/* TOKEMON SPAWNS */
normalNotSpawned(Tokemon) :- normal(Tokemon), \+(tokemon(Tokemon, _, _, _, _)).
legendaryNotSpawned(Tokemon) :- legendary(Tokemon), \+(tokemon(Tokemon, _, _, _, _)).
legendaryRoaming(Tokemon) :- legendary(Tokemon), tokemon(Tokemon, _, _, _, 0).

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
   
initLegendary(0) :- !.
initLegendary(N) :-
    height(H),
    width(W),
    random(1, W, X),
    random(1, H, Y),
    findall(Tokemon, legendaryNotSpawned(Tokemon), ListTokemon),
    length(ListTokemon, LenListTokemon),
    random(0, LenListTokemon, Pick),
    take(ListTokemon, Pick, NameTokemon),
    maxHealth(NameTokemon, HealthTokemon),
    asserta(tokemon(NameTokemon, X, Y, HealthTokemon, 0)),
    NextN is N - 1,
    initLegendary(NextN), !.
/* END OF TOKEMON SPAWNS */

/* TOKEMON ROAMS */
roamAllTokemon :-
    findall(Tokemon, tokemon(Tokemon, _, _, _, 0), ListTokemon),
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
    random(1, 5, RNG),
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

/* TOKEMON OUTPUTS  */
status :-
    findall(Tokemon, tokemon(Tokemon,_,_,_,1), ListTokemon),
    write('Tokemon kamu : '),
    nl,
    printStatus(ListTokemon),
    
    findall(LegendaryEnemy, legendaryRoaming(LegendaryEnemy), ListEnemy),
    write('Your Enemy:'),
    nl,
    printStatus(ListEnemy)
    .
    
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

printSpecialAttackMessage(TokemonP, Jurus) :- format('~w used ~w!', [TokemonP, Jurus]), nl.
printPlayerDamage(AtkAtribut, Enemy) :- format('You dealt ~w damage to ~w', [AtkAtribut, Enemy]), nl.
printEnemyDamage(Enemy, AtkAtribut, CurrentPicked) :- format('~w dealt ~w damage to ~w', [Enemy, AtkAtribut, CurrentPicked]), nl.
printBattleStatus(TokemonP, Enemy) :-
	tokemon(TokemonP, _, _, HP, _),
    type(TokemonP, Type),
    write(TokemonP),
    nl,
    write('Health : '),
    write(HP),
    nl,
    write('Type : '),
    write(Type),
    nl,
    nl,
    
    tokemon(Enemy, _, _, EnemyHP, _),
    type(Enemy, EnemyType),
    write(Enemy),
    nl,
    write('Health : '),
    write(EnemyHP),
    nl,
    write('Type : '),
    write(EnemyType),
    nl,
    nl.
/* END OF TOKEMON OUTPUTS */

/* TOKEMON BATTLE BEHAVIOUR */
capture :-
    isInventoryFull,
    write('You cannot capture another Tokemon! You have to drop one first.'), nl, !.
capture :-
    retract(encounter(Enemy)),
    format('~w is captured!', [Enemy]),
    retract(tokemon(Enemy,X,Y,_,_)),
    maxHealth(Enemy, MaxHP),
    asserta(tokemon(Enemy,X,Y,MaxHP,1)),
    addTokemon(Enemy),
    retract(battle(_)).
ignore :- /* ignore == mati */
    retract(encounter(Enemy)),
    retract(tokemon(Enemy,_,_,_,_)),
    retract(battle(_)).

% PLAYER
attack :- \+status(battle), write('waduh sori ga bisa nih gan'),!, fail.
attack :- \+(battle(_)), write('Pilih Tokemon terlebih dahulu!'), !.
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
        ; type(TokemonP,water),type(Enemy,fire) ->
            damage(TokemonP,Atk),
            AtkAtribut is Atk + Atk/2
        ; damage(TokemonP,Atk),
        AtkAtribut is Atk
    ),
    tokemon(Enemy,X,Y,HP,Ownership),
    printPlayerDamage(AtkAtribut, Enemy), nl,
    retract(tokemon(Enemy, X, Y, HP, Ownership)),
    HPnew is HP - AtkAtribut,
    (HPnew < 0 ->
        HPadd is 0
    ; HPadd is HPnew
    ),
    asserta(tokemon(Enemy, X, Y, HPadd, Ownership)),
    printBattleStatus(TokemonP, Enemy),
    (
        HPadd =:= 0 ->
        write('Musuh kalah.'),
        nl,
        write('Tangkep ga?'),
        nl,
        (
            special(Enemy) ->
            retract(special(Enemy));
            !
        ),
        (
            special(TokemonP) ->
            retract(special(TokemonP));
            !
        )
        ; retract(tokemon(Enemy,X,Y,_,Owner)),
        assertz(tokemon(Enemy,X,Y,HPnew,Owner))
    ),
    (HPadd > 0 ->
        decideEnemyBattle, !
    ; !
    ).
    
specialAttack :- \+status(battle), write('waduh sori ga bisa nih gan'),!, fail.
specialAttack :- \+(battle(_)), write('Pilih Tokemon terlebih dahulu!'), !.
specialAttack :- battle(TokemonP), special(TokemonP), write('Special attacks can only be used once per battle!'), !, fail.
specialAttack :-
    battle(TokemonP),
    encounter(Enemy),
    asserta(special(TokemonP)),
    (
        type(TokemonP,fire),type(Enemy,leaves) ->
            skill(TokemonP,Jurus,Atk),
            AtkAtribut is Atk + Atk/2
        ; type(TokemonP,leaves),type(Enemy,water) ->
            skill(TokemonP,Jurus,Atk),
            AtkAtribut is Atk + Atk/2
        ; type(TokemonP,water),type(Enemy,fire) ->
            skill(TokemonP,Jurus,Atk),
            AtkAtribut is Atk + Atk/2
        ; damage(TokemonP, Atk), 
        AtkAtribut is Atk
    ),
    tokemon(Enemy,X,Y,HP,Ownership),
    printSpecialAttackMessage(TokemonP, Jurus), nl,
    printPlayerDamage(AtkAtribut,Enemy),nl,
    retract(tokemon(Enemy, X, Y, HP, Ownership)),
    HPnew is HP - AtkAtribut,
    (HPnew < 0 ->
        HPadd is 0
    ; HPadd is HPnew
    ),
    asserta(tokemon(Enemy, X, Y, HPadd, Ownership)),
    printBattleStatus(TokemonP, Enemy),
    (HPnew =< 0 ->
        write('Musuh kalah.'),
        nl,
        write('Tangkep ga?'),
        nl,
        retract(tokemon(Enemy,_,_,_,_)),
        retract(encounter(Enemy)),
        (
            special(Enemy) ->
            retract(special(Enemy))
        ),
        retract(special(TokemonP))
        ; retract(tokemon(Enemy,X,Y,_,Owner)),
        assertz(tokemon(Enemy,X,Y,HPnew,Owner))
    ),
    (HPadd > 0 ->
        decideEnemyBattle, !
    ; !
    ).
% END OF PLAYER

% ENEMY
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
        ; type(Enemy,water),type(TokemonP,fire) ->
            damage(Enemy,Atk),
            AtkAtribut is Atk + Atk/2
        ; damage(Enemy,Atk),
        AtkAtribut is Atk
    ),
    tokemon(TokemonP,X,Y,HP,Ownership),
    format('~w attacks!', [Enemy]), nl,
    printEnemyDamage(Enemy, AtkAtribut, TokemonP), nl,

    HPnew is HP - AtkAtribut,
    (HPnew < 0 ->
        HPadd is 0
    ; HPadd is HPnew
    ),
    retract(tokemon(TokemonP,X,Y,HP,Ownership)),
    asserta(tokemon(TokemonP, X, Y, HPadd, Ownership)),
    printBattleStatus(TokemonP, Enemy),
    (HPadd =:= 0 ->
        write('Tokemon kita kalah.'),
        nl,
        (
            \+inventory(_) ->
            kalah
        ),
        write('Pilih Tokemon lagi'),
        nl,
        retract(tokemon(TokemonP,_,_,_,_)),
        retract(inventory(TokemonP)),
        retract(battle(TokemonP)),
        printInventory,
        (
            special(TokemonP) ->
            retract(special(TokemonP))
        )
    ; retract(tokemon(TokemonP,X,Y,_,Owner)), assertz(tokemon(TokemonP,X,Y,HPnew,Owner))
    ).

enemySpecialAttack :- encounter(Enemy), special(Enemy), format('~w cannot use is special attack anymore!',[Enemy]), !, fail.
enemySpecialAttack :-
    battle(TokemonP),
    encounter(Enemy),
    asserta(special(Enemy)),
    (
        type(Enemy,fire),type(TokemonP,leaves) ->
            skill(Enemy,Jurus,Atk),
            AtkAtribut is Atk + Atk/2
        ; type(Enemy,leaves),type(TokemonP,water) ->
            skill(Enemy,Jurus,Atk),
            AtkAtribut is Atk + Atk/2
        ; type(Enemy,water),type(TokemonP,fire) ->
            skill(Enemy,Jurus,Atk),
            AtkAtribut is Atk + Atk/2
        ; skill(Enemy,Jurus,Atk),
        AtkAtribut is Atk
    ),
    tokemon(TokemonP,X,Y,HP,Ownership),
    printSpecialAttackMessage(Enemy, Jurus), nl, 
    printEnemyDamage(Enemy, AtkAtribut, TokemonP), nl,
    HPnew is HP - AtkAtribut,
    (HPnew < 0 ->
        HPadd is 0
    ; HPadd is HPnew
    ),
    retract(tokemon(TokemonP, X, Y, HP, Ownership)),
    asserta(tokemon(TokemonP, X, Y, HPadd, Ownership)),
    printBattleStatus(TokemonP, Enemy),
    (HPadd =:= 0 ->
            write('Tokemon kita kalah.'),
            nl,
            (
                \+inventory(_) ->
                kalah
            ),
            write('Pilih Tokemon lagi'),
            nl,
            retract(tokemon(TokemonP,_,_,_,_)),
            retract(inventory(TokemonP)),
            retract(battle(TokemonP)),
            printInventory,
            (
                special(TokemonP) ->
                retract(special(TokemonP))
            )
        ; retract(tokemon(TokemonP,X,Y,_,Owner)),
        assertz(tokemon(TokemonP,X,Y,HPnew,Owner))
    ).
% END OF ENEMY

% RNG BEHAVIOUR 
decideEnemyBattle :-
    random(1, 101, RNG),
    (RNG =< 25 ->
        enemySpecialAttack
    ;   enemyAttack
    ).
% END OF RNG BEHAVIOUR
/* END OF TOKEMON BATTLE BEHAVIOUR */

% LOSING CONDITION
kalah :-
    reset,
    write('Wah kamu kalah, cupu sih, ayo coba lagi!').