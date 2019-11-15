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
:- dynamic(level/2). /* level(Tokemon, lvl), Level dari tokemon */

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

evolveto(jones, napoleon).
evolveto(arip, pira).
evolveto(laron, tawon).

starter(jones).
starter(mitel).
starter(yoga).
starter(arip).

level(bangkumon,1).
level(mejamon,1).
level(zhafransyah,1).
level(vegan,1).
level(fabian,1).
level(jones,1).
level(mitel,1).
level(yoga,1).
level(arip,1).
level(laron,1).
level(azong,1).
level(tudecu,1).
level(pilbet,1).
level(jopan,1).

% tokemon evolve jadi apa
evolveto(bangkumon, evolveBangkumon).
    
% base health untuk level 1
maxHealth(bangkumon, 200). maxHealth(evolveBangkumon, 500).
maxHealth(mejamon, 500).
maxHealth(zhafransyah, 500).
maxHealth(vegan, 9).
maxHealth(fabian, 150).
maxHealth(jones, 750).
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
type(zhafransyah, electric).
type(vegan, ground).
type(fabian, water).
type(jones, flying).
type(mitel, fire).
type(yoga, leaves).
type(arip, flying).
type(laron, ground).
type(azong, water).
type(tudecu, electric).
type(pilbet, fire).
type(jopan, flying).

damage(bangkumon, 35).
damage(mejamon, 10).
damage(zhafransyah, 50).
damage(vegan, 10).
damage(fabian, 15).
damage(jones, 2000).
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
skill(jones, breakdance, 3500).
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

expGain(Tokemon, Enemy, ExpGained) :-
    level(Tokemon, Lvl),
    (legendary(Enemy) ->
        Mult is 2
    ;   Mult is 1
    ),
    ExpGained is Mult*(1/(1 + 0.5* (Lvl - 1))).

multiplier(Tokemon, Multiplier) :-
    level(Tokemon, Lvl),
    FLvl is floor(Lvl),
    Multiplier is (1.2)**(FLvl - 1).

evolve(Tokemon) :- \+evolveto(_), write('waduh sorry gabisa gan !'), nl, !.
evolve(Tokemon) :- level(Tokemon, X), X<4 , write('waduh sorry gabisa gan!'), nl, !.
evolve(Tokemon) :- 
    retract(level(Tokemon, X)),
    X >= 4,
    evolveto(Tokemon,Evolved),
    asserta(level(Evolved,X)),
    retract(inventory(Tokemon)),
    retract(tokemon(Tokemon,XPos,YPos,HP,Owner)),
    asserta(inventory(Evolved)),
    HPnew is 2 * HP,
    asserta(tokemon(Evolved,XPos,YPos,HPnew,Owner)),
    format('~w has evolved to ~w !', [Tokemon, Evolved]),
    nl.



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
    write('Your Tokemons: '),
    nl,
    printStatus(ListTokemon),
    
    findall(LegendaryEnemy, legendaryRoaming(LegendaryEnemy), ListEnemy),
    write('Legendary Tokemons Left:'),
    nl,
    printStatus(ListEnemy)
    .
    
printStatus([]) :- !.

printStatus([Tokemon|Tail]) :-
    tokemon(Tokemon, _, _, HP, _),
    type(Tokemon, Type),
    format('> ~w', [Tokemon]),
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
	\+tokemon(TokemonP, _, _, _, _),
    write('Quick, pick a Tokemon!'),
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
    nl,
    !.
printBattleStatus(TokemonP, Enemy) :-
	tokemon(TokemonP, _, _, HP, _),
    type(TokemonP, Type),
    format('> ~w', [TokemonP]),
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
    format('> ~w', [Enemy]),
    nl,
    write('Health : '),
    write(EnemyHP),
    nl,
    write('Type : '),
    write(EnemyType),
    nl,
    nl,
    !.
/* END OF TOKEMON OUTPUTS */

/* TOKEMON BATTLE BEHAVIOUR */
capture :- \+(encounter(_)), write('You\'re not in battle!'), nl, !.
capture :- encounter(Enemy), tokemon(Enemy, _, _, HP, _), HP > 0, write('You have to defeat the Tokemon first!'), nl, !.
capture :-
    isInventoryFull,
    write('You cannot capture another Tokemon! You have to drop one first.'), nl, !.
capture :-
    retract(encounter(Enemy)),
    format('~w is captured!', [Enemy]), nl,
    retract(tokemon(Enemy,X,Y,_,_)),
    maxHealth(Enemy, MaxHP),
    asserta(tokemon(Enemy,X,Y,MaxHP,1)),
    addTokemon(Enemy),
    retract(battle(_)),
    (
        \+legendaryRoaming(_) ->
        menang
    ),!.
ignore :- \+(encounter(_)), write('You\'re not in battle!'), nl, !.
ignore :- encounter(Enemy), tokemon(Enemy, _, _, HP, _), HP > 0, write('You have to defeat the Tokemon first!'), nl, !.
ignore :- /* ignore == mati */
    retract(encounter(Enemy)),
    format('~w ran away.', [Enemy]), nl, 
    retract(tokemon(Enemy,_,_,_,_)),
    retract(battle(_)),
    (
        \+legendaryRoaming(_) ->
        menang
    ),!.

% PLAYER
attack :- \+status(battle), write('Sorry! You cannot do that for now. '),!, fail.
attack :- \+(battle(_)), write('Pick a Tokemon!'), !.
attack :- encounter(Tokemon), tokemon(Tokemon, _, _, HP, _), HP =:= 0, write('Have some mercy.'), nl, !.
attack :-
    battle(TokemonP),
    encounter(Enemy),
    multiplier(TokemonP, Mult),
    (
        type(TokemonP,fire),type(Enemy,leaves) ->
            damage(TokemonP,Atk),
            AtkAtribut is Mult * (Atk + Atk/2)
        ; type(TokemonP,leaves),type(Enemy,water) ->
            damage(TokemonP,Atk),
            AtkAtribut is Mult * (Atk + Atk/2)
        ; type(TokemonP,water),type(Enemy,fire) ->
            damage(TokemonP,Atk),
            AtkAtribut is Mult * (Atk + Atk/2)
        ; type(TokemonP, flying),type(Enemy,leaves) ->
            damage(TokemonP,Atk),
            AtkAtribut is Mult * (Atk + Atk/2)
        ; type(TokemonP, electric),type(Enemy, water) ->
            damage(TokemonP,Atk),
            AtkAtribut is Mult * (Atk + Atk/2)
        ; type(TokemonP, ground),type(Enemy,electric) ->
            damage(TokemonP,Atk),
            AtkAtribut is Mult * (Atk + Atk/2)    
        ; type(TokemonP,leaves),type(Enemy,fire) ->
            damage(TokemonP,Atk),
            AtkAtribut is Mult * (Atk - Atk/2)
        ; type(TokemonP,water),type(Enemy,leaves) ->
            damage(TokemonP,Atk),
            AtkAtribut is Mult * (Atk - Atk/2)
        ; type(TokemonP,fire),type(Enemy,water) ->
            damage(TokemonP,Atk),
            AtkAtribut is Mult * (Atk - Atk/2)
        ; type(TokemonP, leaves),type(Enemy,flying) ->
            damage(TokemonP,Atk),
            AtkAtribut is Mult * (Atk - Atk/2)
        ; type(TokemonP, water),type(Enemy, electric) ->
            damage(TokemonP,Atk),
            AtkAtribut is Mult * (Atk - Atk/2)
        ; type(TokemonP, electric),type(Enemy,ground) ->
            damage(TokemonP,Atk),
            AtkAtribut is Mult * (Atk - Atk/2)    
        ; damage(TokemonP,Atk),
        AtkAtribut is Mult * Atk
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
        retract(level(TokemonP, Lvl)),
        expGain(TokemonP, Enemy, Exp),
        asserta(level(TokemonP, Exp)),
        format('~w fainted.', [Enemy]),
        nl,
        write('Capture?'),
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
        ), retract(status(_)),
        asserta(status(roam))
        ; retract(tokemon(Enemy,X,Y,_,Owner)),
        assertz(tokemon(Enemy,X,Y,HPnew,Owner))
        
    ),
    (HPadd > 0 ->
        sleep(2), decideEnemyBattle, !
    ; !
    ).
    
specialAttack :- \+(status(battle)), write('Sorry! You cannot do that for now.'), nl, !, fail.
specialAttack :- \+(battle(_)), write('Pick a Tokemon!'), nl, !.
specialAttack :- encounter(Tokemon), tokemon(Tokemon, _, _, HP, _), HP =:= 0, write('Have some mercy.'), nl, !.
specialAttack :- battle(TokemonP), special(TokemonP), write('Special attacks can only be used once per battle!'), nl, !, fail.
specialAttack :-
    battle(TokemonP),
    encounter(Enemy),
    skill(TokemonP, Jurus, Atk),
    asserta(special(TokemonP)),
    level(TokemonP, Lvl),
    (   
        type(TokemonP,fire),type(Enemy,leaves) ->
            AtkAtribut is Mult * (Atk + Atk/2)
        ; type(TokemonP,leaves),type(Enemy,water) ->
            AtkAtribut is Mult * (Atk + Atk/2)
        ; type(TokemonP,water),type(Enemy,fire) ->
            AtkAtribut is Mult * (Atk + Atk/2)
        ; type(TokemonP, flying),type(Enemy,leaves) ->
            AtkAtribut is Mult * (Atk + Atk/2)
        ; type(TokemonP, electric),type(Enemy, water) ->
            AtkAtribut is Mult * (Atk + Atk/2)
        ; type(TokemonP, ground),type(Enemy,electric) ->
            AtkAtribut is Mult * (Atk + Atk/2)  
        ; type(TokemonP,leaves),type(Enemy,fire) ->
            AtkAtribut is Mult * (Atk - Atk/2)
        ; type(TokemonP,water),type(Enemy,leaves) ->
            AtkAtribut is Mult * (Atk - Atk/2)
        ; type(TokemonP,fire),type(Enemy,water) ->
            AtkAtribut is Mult * (Atk - Atk/2)
        ; type(TokemonP, leaves),type(Enemy,flying) ->
            AtkAtribut is Mult * (Atk - Atk/2)
        ; type(TokemonP, water),type(Enemy, electric) ->
            AtkAtribut is Mult * (Atk - Atk/2)
        ; type(TokemonP, electric),type(Enemy,ground) ->
            AtkAtribut is Mult * (Atk - Atk/2)  
        ; AtkAtribut is Mult * Atk
    ),
    tokemon(Enemy,X,Y,HP,Ownership),
    printSpecialAttackMessage(TokemonP, Jurus), nl,
    write('.....'), nl,
    sleep(1),
    printPlayerDamage(AtkAtribut,Enemy),nl,
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
        retract(level(TokemonP, Lvl)),
        expGain(TokemonP, Enemy, Exp),
        asserta(level(TokemonP, Exp)),
        format('~w fainted', [Enemy]),
        nl,
        write('Capture?'),
        nl,
        (
            special(Enemy) ->
            retract(special(Enemy))
        ), retract(status(_)), asserta(status(roam)), retract(special(TokemonP))
        ; retract(tokemon(Enemy,X,Y,_,Owner)),
        assertz(tokemon(Enemy,X,Y,HPnew,Owner))
        
    ),
    (HPadd > 0 ->
        sleep(2), decideEnemyBattle, !
    ; !
    ).
% END OF PLAYER

% ENEMY
enemyAttack :- \+(battle(_)), encounter(Enemy), format("A wild ~w appears!", [Enemy]), nl, !.
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
        ; type(Enemy, flying),type(TokemonP,leaves) ->
            damage(Enemy,Atk),
            AtkAtribut is Atk + Atk/2
        ; type(Enemy, electric),type(TokemonP, water) ->
            damage(Enemy,Atk),
            AtkAtribut is Atk + Atk/2
        ; type(Enemy, ground),type(TokemonP,electric) ->
            damage(Enemy,Atk),
            AtkAtribut is Atk + Atk/2  
        ;type(Enemy,leaves),type(TokemonP,fire) ->
            damage(Enemy,Atk),
            AtkAtribut is Atk - Atk/2
        ; type(Enemy,water),type(TokemonP,leaves) ->
            damage(Enemy,Atk),
            AtkAtribut is Atk - Atk/2
        ; type(Enemy,fire),type(TokemonP,water) ->
            damage(Enemy,Atk),
            AtkAtribut is Atk - Atk/2
        ; type(Enemy, leaves),type(TokemonP,flying) ->
            damage(Enemy,Atk),
            AtkAtribut is Atk - Atk/2
        ; type(Enemy, water),type(TokemonP, electric) ->
            damage(Enemy,Atk),
            AtkAtribut is Atk - Atk/2
        ; type(Enemy, electric),type(TokemonP,ground) ->
            damage(Enemy,Atk),
            AtkAtribut is Atk - Atk/2  
        ;  damage(Enemy,Atk),
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
        write('Your Tokemon loses!'),
        nl,
        retract(tokemon(TokemonP,_,_,_,_)),
        retract(inventory(TokemonP)),
        retract(battle(TokemonP)),
        (
            \+inventory(_) ->
            kalah
            ;write('Choose another tokemon! '),
        nl,
        printInventory,
            (
                special(TokemonP) ->
                retract(special(TokemonP))
            )
        )
    ; retract(tokemon(TokemonP,X,Y,_,Owner)), assertz(tokemon(TokemonP,X,Y,HPnew,Owner))
    ).

% enemySpecialAttack :- encounter(Enemy), special(Enemy), format('~w cannot use its special attack anymore!',[Enemy]), !, fail. 
% Kalo AI nya milih special attack, kasih attack biasa aja
enemySpecialAttack :- encounter(Enemy), special(Enemy), enemyAttack, !.
enemySpecialAttack :- \+(battle(_)), encounter(Enemy), format("A wild ~w appears!", [Enemy]), nl, !.
enemySpecialAttack :-
    battle(TokemonP),
    encounter(Enemy),
    asserta(special(Enemy)),
    skill(Enemy, Jurus, Atk),
    (
        type(TokemonP,fire),type(Enemy,leaves) ->
            AtkAtribut is Lvl * (Atk - Atk/2)
        ; type(TokemonP,leaves),type(Enemy,water) ->
            AtkAtribut is Lvl * (Atk - Atk/2)
        ; type(TokemonP,water),type(Enemy,fire) ->
            AtkAtribut is Lvl * (Atk - Atk/2)
        ; type(TokemonP, flying),type(Enemy,leaves) ->
            AtkAtribut is Lvl * (Atk - Atk/2)
        ; type(TokemonP, electric),type(Enemy, water) ->
            AtkAtribut is Lvl * (Atk - Atk/2)
        ; type(TokemonP, ground),type(Enemy,electric) ->
            AtkAtribut is Lvl * (Atk - Atk/2)  
        ; type(TokemonP,leaves),type(Enemy,fire) ->
            AtkAtribut is Lvl * (Atk + Atk/2)
        ; type(TokemonP,water),type(Enemy,leaves) ->
            AtkAtribut is Lvl * (Atk + Atk/2)
        ; type(TokemonP,fire),type(Enemy,water) ->
            AtkAtribut is Lvl * (Atk + Atk/2)
        ; type(TokemonP, leaves),type(Enemy,flying) ->
            AtkAtribut is Lvl * (Atk + Atk/2)
        ; type(TokemonP, water),type(Enemy, electric) ->
            AtkAtribut is Lvl * (Atk + Atk/2)
        ; type(TokemonP, electric),type(Enemy,ground) ->
            AtkAtribut is Lvl * (Atk + Atk/2)  
        ; AtkAtribut is Lvl * Atk
    ),
    tokemon(TokemonP,X,Y,HP,Ownership),
    printSpecialAttackMessage(Enemy, Jurus), nl, 
    write('.....'), nl,
    sleep(1),
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
            write('Your Tokemon loses! '),
            nl,
            
            retract(tokemon(TokemonP,_,_,_,_)),
            retract(inventory(TokemonP)),
            retract(battle(TokemonP)),
            (
            \+inventory(_) ->
            kalah
            ; write('Choose another tokemon! '),
            nl,
            printInventory,
                (
                    special(TokemonP) ->
                    retract(special(TokemonP))
                )
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
    write('You lose! Get gud!!!'),nl,
    write('Type start to play again!'), nl, !.

% WINNING CONDITION
menang :- 
    reset,
    write('JENG JENG JENG JENG '),nl,
    sleep(1),
    write('  __ __   ___   __ __      __    __  ____  ____       ______  __ __    ___       ____   ____  ___ ___    ___      __   '),nl,
    write(' |  |  | /   \\ |  |  |    |  |__|  ||    ||    \\     |      ||  |  |  /  _]     /    | /    ||   |   |  /  _]    |  | '),nl,
    write(' |  |  ||     ||  |  |    |  |  |  | |  | |  _  |    |      ||  |  | /  [_     |   __||  o  || _   _ | /  [_     |  | '),nl,
    write(' |  ~  ||  O  ||  |  |    |  |  |  | |  | |  |  |    |_|  |_||  _  ||    _]    |  |  ||     ||  \\_/  ||    _]    |__| '),nl,
    write(' |___, ||     ||  :  |    |  `     | |  | |  |  |      |  |  |  |  ||   [_     |  |_ ||  _  ||   |   ||   [_      __  '),nl,
    write(' |     ||     ||     |     \\      /  |  | |  |  |      |  |  |  |  ||     |    |     ||  |  ||   |   ||     |    |  | '),nl,
    write(' |____/  \\___/  \\__,_|      \\_/\\_/  |____||__|__|      |__|  |__|__||_____|    |___,_||__|__||___|___||_____|    |__| '),nl,
                                                                                                                    

    write('Congratulations !! Write start if you want to play again!! '),!.


