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
legendary(mamon).           legendary(kumon).           evolveto(mamon, kumon).
legendary(tokek).           legendary(tokai).           evolveto(tokek, tokai).
legendary(zhafransyah).     legendary(sultan).          evolveto(zhafransyah, sultan).
legendary(vegan).           legendary(cogan).           evolveto(vegan, cogan).
normal(fabian).             normal(fabiun).             evolveto(fabian, fabiun).
normal(jones).              normal(jones2).             evolveto(jones, jones2).
normal(mitel).              normal(miteru).             evolveto(mitel, miteru).
normal(yogay).              normal(magay).              evolveto(yogay, magay).
normal(arip).               normal(pari).               evolveto(arip, pari).
normal(laron).              normal(mawut).              evolveto(laron, mawut).
normal(azong).              normal(azab).               evolveto(azong, azab).
normal(tudecu).             normal(paranicu).           evolveto(tudecu, paranicu).
normal(pilbet).             normal(pilbat).             evolveto(pilbet, pilbat).
normal(jopan).              normal(jepun).              evolveto(jopan, jepun).

starter(jones).
starter(mitel).
starter(yogay).
starter(arip).

level(mamon,1).             level(kumon, 4).
level(tokek,1).             level(tokai, 4).
level(zhafransyah,1).       level(sultan, 4).
level(vegan,1).             level(cogan, 4).
level(fabian,1).            level(fabiun, 4).
level(jones,1).             level(jones2, 4).
level(mitel,1).             level(miteru, 4).
level(yogay,1).             level(magay, 4).
level(arip,1).              level(pari, 4).
level(laron,1).             level(mawut, 4).
level(azong,1).             level(azab, 4).
level(tudecu,1).            level(paranicu, 4).
level(pilbet,1).            level(pilbat, 4).
level(jopan,1).             level(jepun, 4).
    
maxHealth(mamon, 200).          maxHealth(kumon, 400).
maxHealth(tokek, 200).          maxHealth(tokai, 400).
maxHealth(zhafransyah, 300).    maxHealth(sultan, 600).
maxHealth(vegan, 10).           maxHealth(cogan, 50).
maxHealth(fabian, 150).         maxHealth(fabiun, 300).
maxHealth(jones, 150).          maxHealth(jones2, 300).
maxHealth(mitel, 100).          maxHealth(miteru, 200).
maxHealth(yogay, 250).          maxHealth(magay, 500).
maxHealth(arip, 120).           maxHealth(pari, 240).
maxHealth(laron, 10).           maxHealth(mawut, 20).
maxHealth(azong, 130).          maxHealth(azab, 300).
maxHealth(tudecu, 90).          maxHealth(paranicu, 200).
maxHealth(pilbet, 110).         maxHealth(pilbat, 250).
maxHealth(jopan, 140).          maxHealth(jepun, 300)

type(mamon, fire).              type(kumon, fire).
type(tokek, water).             type(tokai, water).
type(zhafransyah, electric).    type(sultan, electric).
type(vegan, ground).            type(cogan, ground).
type(fabian, water).            type(fabiun, water).
type(jones, ground).            type(jones2, ground).
type(mitel, fire).              type(miteru, fire).
type(yogay, leaves).            type(magay, leaves),
type(arip, flying).             type(pari, flying).
type(laron, ground).            type(mawut, ground).
type(azong, water).             type(azab, water).
type(tudecu, electric).         type(paranicu, electric).
type(pilbet, fire).             type(pilbat, water).
type(jopan, leaves).            type(jepun, leaves).

damage(mamon, 30).              damage(kumon, 60).
damage(tokek, 25).              damage(tokai, 50).
damage(zhafransyah, 30).        damage(sultan, 60).
damage(vegan, 50).              damage(cogan, 100).
damage(fabian, 30).             damage(fabiun, 60).
damage(jones, 40).              damage(jones2, 80).
damage(mitel, 20).              damage(miteru, 40).
damage(yogay, 25).              damage(magay, 50).
damage(arip, 30).               damage(pari, 60).
damage(laron, 1).               damage(mawut, 5).
damage(azong, 35).              damage(azab, 70).
damage(tudecu, 30).             damage(paranicu, 60).
damage(pilbet, 30).             damage(pilbat, 60).
damage(jopan, 50).              damage(jepun, 100).

skill(mamon, sesajen, 60).          skill(kumon, aljabar, 150).
skill(tokek, mengerang, 60).        skill(tokai, menjerit, 150).
skill(zhafransyah, ruqyah, 75).     skill(sultan, santet, 150).
skill(vegan, jamur, 150).            skill(cogan, capcay, 300).
skill(fabian, berdoa, 70).          skill(fabiun, sembahyang, 140).
skill(jones, breakdance, 100).       skill(jones2, menggeliat, 200).
skill(mitel, danusan, 50).          skill(miteru, ntakntul, 100).
skill(yogay, muntah, 40).           skill(magay, ngambang, 100).
skill(arip, par, 60).               skill(pari, rap, 120).
skill(laron, nyampah, 10).          skill(mawut, mati, 20).
skill(azong, renang, 30).           skill(azab, polo, 70).
skill(tudecu, nyasar, 60).          skill(paranicu, panik, 120).
skill(pilbet, flamethrower, 50).    skill(pilbat, waterthrower, 100).
skill(jopan, sembelit, 20).         skill(jepun, kentut, 70).
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
    (FLvl >= 4 ->
        RLvl is FLvl - 3 
    ;   RLvl is FLvl
    ),
    Multiplier is (1.2)**(RLvl - 1).

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
    (inventory(Tokemon) ->
        level(Tokemon, Exp),
        FLvl is floor(Exp),
        write('Level : '),
        write(FLvl),
        nl
    ; !
    ),
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
    (\+legendaryRoaming(_) ->
        menang
    ;   !
    ),!.
ignore :- \+(encounter(_)), write('You\'re not in battle!'), nl, !.
ignore :- encounter(Enemy), tokemon(Enemy, _, _, HP, _), HP > 0, write('You have to defeat the Tokemon first!'), nl, !.
ignore :- /* ignore == mati */
    retract(encounter(Enemy)),
    format('~w ran away.', [Enemy]), nl, 
    retract(tokemon(Enemy,_,_,_,_)),
    retract(battle(_)),
    (\+legendaryRoaming(_) ->
        menang
    ;   !
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
        type(TokemonP, fire),type(Enemy, leaves) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk + Atk/2)
        ; type(TokemonP, leaves),type(Enemy, water) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk + Atk/2)
        ; type(TokemonP, water),type(Enemy, fire) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk + Atk/2)
        ; type(TokemonP, flying),type(Enemy, ground) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk + Atk/2)
        ; type(TokemonP, ground),type(Enemy, electric) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk + Atk/2)
        ; type(TokemonP, electric),type(Enemy, flying) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk + Atk/2)
        ; type(TokemonP, fire),type(Enemy, flying) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk + Atk/2)
        ; type(TokemonP, flying),type(Enemy, leaves) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk + Atk/2)
        ; type(TokemonP, leaves),type(Enemy, electric) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk + Atk/2)
        ; type(TokemonP, electric),type(Enemy, water) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk + Atk/2)
        ; type(TokemonP, water),type(Enemy, ground) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk + Atk/2)
        ; type(TokemonP, ground),type(Enemy, fire) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk + Atk/2)
        ; type(TokemonP, leaves),type(Enemy, fire) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk - Atk/2)
        ; type(TokemonP, water),type(Enemy, leaves) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk - Atk/2)
        ; type(TokemonP, fire),type(Enemy, water) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk - Atk/2)
        ; type(TokemonP, ground),type(Enemy, flying) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk - Atk/2)
        ; type(TokemonP, electric),type(Enemy, ground) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk - Atk/2)
        ; type(TokemonP, flying),type(Enemy, electric) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk - Atk/2)
        ; type(TokemonP, flying),type(Enemy, fire) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk - Atk/2)
        ; type(TokemonP, leaves),type(Enemy, flying) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk - Atk/2)
        ; type(TokemonP, electric),type(Enemy, leaves) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk - Atk/2)
        ; type(TokemonP, water),type(Enemy, electric) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk - Atk/2)
        ; type(TokemonP, ground),type(Enemy, water) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk - Atk/2)
        ; type(TokemonP, fire),type(Enemy, ground) ->
            damage(TokemonP, Atk),
            AtkAtribut is Lvl * (Atk - Atk/2)    
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
    (HPadd =:= 0 ->
        retract(level(TokemonP, Lvl)),
        expGain(TokemonP, Enemy, Exp),
        asserta(level(TokemonP, Exp)),
        format('~w fainted.', [Enemy]),
        nl,
        (
            floor(Exp) >= 4 ->
            evolveto(TokemonP,Evolved),
            retract(level(TokemonP,Exp)),
            asserta(level(Evolved,Exp)),
            retract(inventory(TokemonP)),
            retract(tokemon(TokemonP,XPos,YPos,HP,Owner)),
            asserta(inventory(Evolved)),
            multiplier(TokemonP, Mult),
            HPnew is Mult * HP,
            asserta(tokemon(Evolved,XPos,YPos,HPnew,Owner)),
            format('~w has evolved to ~w !', [TokemonP, Evolved]),
            nl
            ; !
        ),
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
        (
            floor(Exp) >= 4 ->
            evolveto(TokemonP,Evolved),
            retract(level(TokemonP,Exp)),
            asserta(level(Evolved,Exp)),
            retract(inventory(TokemonP)),
            retract(tokemon(TokemonP,XPos,YPos,HP,Owner)),
            asserta(inventory(Evolved)),
            multiplier(TokemonP, Mult),
            HPnew is Mult * HP,
            asserta(tokemon(Evolved,XPos,YPos,HPnew,Owner)),
            format('~w has evolved to ~w !', [TokemonP, Evolved]),
            nl
            ; !
        ),
        write('Capture?'),
        nl,
        (special(Enemy) ->
            retract(special(Enemy))
        ;   !
        ),
        retract(status(_)),
        asserta(status(roam)),
        retract(special(TokemonP))
    ;   retract(tokemon(Enemy,X,Y,_,Owner)), assertz(tokemon(Enemy,X,Y,HPnew,Owner))
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


