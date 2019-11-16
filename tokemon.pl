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
/* basic tokemon */
legendary(mamon).                      
legendary(tokekmon).                      
legendary(zhafransyah).               
legendary(vegan).                      
normal(fabian).                          
normal(jones).                           
normal(mitel).                           
normal(yogay).                            
normal(arip).                              
normal(laron).                            
normal(azong).                             
normal(tudecu).                        
normal(pilbet).                          
normal(jopan).                            

/* evolved tokemon */
legendary(kumon).
legendary(tokekmon).
legendary(sultan).
legendary(cogan).
normal(fabiun).
normal(jones2).
normal(miteru).
normal(magay).
normal(pari).
normal(mawut).
normal(azab).
normal(paranicu).
normal(pilbat).
normal(jepun).

/* evolveto */
evolveto(mamon, kumon).
evolveto(tokekmon, tokaimon).
evolveto(zhafransyah, sultan).
evolveto(vegan, cogan).
evolveto(fabian, fabiun).
evolveto(jones, jones2).
evolveto(mitel, miteru).
evolveto(yogay, magay).
evolveto(arip, pari).
evolveto(laron, mawut).
evolveto(azong, azab).
evolveto(tudecu, paranicu).
evolveto(pilbet, pilbat).
evolveto(jopan, jepun).

/* starter tokemon */
starter(jones).
%starter(mitel).
%starter(yoga).
%starter(arip).

/* level basic tokemon */
level(mamon,1.0).             
level(tokekmon,1.0).          
level(zhafransyah,1.0).       
level(vegan,1.0).             
level(fabian,1.0).            
level(jones,1.0).             
level(mitel,1.0).             
level(yogay,1.0).             
level(arip,1.0).              
level(laron,1.0).             
level(azong,1.0).             
level(tudecu,1.0).            
level(pilbet,1.0).            
level(jopan,1.0).         

/* level evolved tokemon */
level(kumon, 4.0).
level(tokaimon, 4.0).
level(sultan, 4.0).
level(cogan, 4.0).
level(fabiun, 4.0).
level(jones2, 4.0).
level(miteru, 4.0).
level(magay, 4.0).
level(pari, 4.0).
level(mawut, 4.0).
level(azab, 4.0).
level(paranicu, 4.0).
level(pilbat, 4.0).
level(jepun, 4.0).
    
/* maxHealth basic tokemon */
maxHealth(mamon, 200).          
maxHealth(tokekmon, 200).       
maxHealth(zhafransyah, 300).    
maxHealth(vegan, 10).           
maxHealth(fabian, 150).         
maxHealth(jones, 150).          
maxHealth(mitel, 100).          
maxHealth(yogay, 250).          
maxHealth(arip, 120).           
maxHealth(laron, 10).           
maxHealth(azong, 130).          
maxHealth(tudecu, 90).          
maxHealth(pilbet, 110).         
maxHealth(jopan, 140).          

/* maxhealth evolved tokemon */
maxHealth(kumon, 400).
maxHealth(tokaimon, 400).
maxHealth(sultan, 600).
maxHealth(cogan, 50).
maxHealth(fabiun, 300).
maxHealth(jones2, 300).
maxHealth(miteru, 200).
maxHealth(magay, 500).
maxHealth(pari, 240).
maxHealth(mawut, 20).
maxHealth(azab, 300).
maxHealth(paranicu, 200).
maxHealth(pilbat, 250).
maxHealth(jepun, 300).

/* type basic tokemon */
type(mamon, fire).              
type(tokekmon, water).          
type(zhafransyah, electric).    
type(vegan, ground).            
type(fabian, water).            
type(jones, ground).            
type(mitel, fire).              
type(yogay, leaves).            
type(arip, flying).             
type(laron, ground).            
type(azong, water).             
type(tudecu, electric).         
type(pilbet, fire).             
type(jopan, leaves).     

/* type evolved tokemon */
type(kumon, fire).
type(tokaimon, water).
type(sultan, electric).
type(cogan, ground).
type(fabiun, water).
type(jones2, ground).
type(miteru, fire).
type(magay, leaves),
type(pari, flying).
type(mawut, ground).
type(azab, water).
type(paranicu, electric).
type(pilbat, water).
type(jepun, leaves).

/* attack basic tokemon */
damage(mamon, 30).              
damage(tokekmon, 25).           
damage(zhafransyah, 30).        
damage(vegan, 50).              
damage(fabian, 30).             
damage(jones, 40).              
damage(mitel, 20).              
damage(yogay, 25).              
damage(arip, 30).               
damage(laron, 1.0).               
damage(azong, 35).              
damage(tudecu, 30).             
damage(pilbet, 30).             
damage(jopan, 50).      

/* attack evolved tokemon */
damage(kumon, 60).
damage(tokaimon, 50).
damage(sultan, 60).
damage(cogan, 100).
damage(fabiun, 60).
damage(jones2, 80).
damage(miteru, 40).
damage(magay, 50).
damage(pari, 60).
damage(mawut, 5).
damage(azab, 70).
damage(paranicu, 60).
damage(pilbat, 60).
damage(jepun, 100).

/* specialAttack basic tokemon */ 
skill(mamon, sesajen, 60).          
skill(tokekmon, mengerang, 60).     
skill(zhafransyah, ruqyah, 75).     
skill(vegan, jamur, 150).           
skill(fabian, berdoa, 70).          
skill(jones, breakdance, 100).      
skill(mitel, danusan, 50).          
skill(yogay, muntah, 40).           
skill(arip, par, 60).               
skill(laron, nyampah, 10).          
skill(azong, renang, 30).           
skill(tudecu, nyasar, 60).          
skill(pilbet, flamethrower, 50).    
skill(jopan, sembelit, 20).         

/* specialAttack evolved tokemon */
skill(kumon, aljabar, 150).
skill(tokaimon, menjerit, 150).
skill(sultan, santet, 150).
skill(cogan, capcay, 300).
skill(fabiun, sembahyang, 140).
skill(jones2, menggeliat, 200).
skill(miteru, ntakntul, 100).
skill(magay, ngambang, 100).
skill(pari, rap, 120).
skill(mawut, mati, 20).
skill(azab, polo, 70).
skill(paranicu, panik, 120).
skill(pilbat, waterthrower, 100).
skill(jepun, kentut, 70).
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
    ExpGained is Mult*(1/(1 + 0.5* (Lvl - 1.0))).

multiplier(Tokemon, Multiplier) :-
    level(Tokemon, Lvl),
    FLvl is floor(Lvl),
    (FLvl >= 4 ->
        RLvl is FLvl - 3 
    ;   RLvl is FLvl
    ),
    Multiplier is (1.2)**(RLvl - 1.0).

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

resetLvl :-
    findall(Tokemon, normalNotSpawned(Tokemon), ListTokemon),
    resetLvlList(ListTokemon).
resetLvlList([]) :- !.
resetLvlList([H | T]) :-
    asserta(level(H, 1.0)),
    resetLvlList(T).

initNormal(0) :- !.
initNormal(N) :-
    height(H),
    width(W),
    repeat,
        random(1, H, Y), random(1, W, X),
        (fence(X, Y) -> fail
        ; gym(X, Y) -> fail
        ; !
        ),
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
    findall(Tokemon, tokemon(Tokemon,_,_,_,1.0), ListTokemon),
    write('Your Tokemons: '),
    nl,
    printStatus(ListTokemon),
    
    findall(LegendaryEnemy, legendaryRoaming(LegendaryEnemy), ListEnemy),
    write('Legendary Tokemons Left:'),
    nl,
    printStatus(ListEnemy), !
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
        Persen is floor((Exp - floor(Exp))*100),
        write('('), write(Persen), write('% to the next level)'),
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
    asserta(tokemon(Enemy,X,Y,MaxHP,1.0)),
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
        level(TokemonP, BefExp),
        expGain(TokemonP, Enemy, ExpGain),
        retract(level(Tokemon, BefExp)),
        NewExp is BefExp + ExpGain,
        asserta(level(TokemonP, NewExp)),
        format('~w fainted.', [Enemy]),
        nl,
        (floor(NewExp) >= 4 ->
            evolveto(TokemonP,Evolved),
            retract(level(TokemonP,NewExp)),
            asserta(level(Evolved,NewExp)),
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
    multiplier(TokemonP, Mult),
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
    sleep(1.0),
    printPlayerDamage(AtkAtribut,Enemy),nl,
    retract(tokemon(Enemy, X, Y, HP, Ownership)),
    HPnew is HP - AtkAtribut,
    (HPnew < 0 ->
        HPadd is 0
    ; HPadd is HPnew
    ),
    asserta(tokemon(Enemy, X, Y, HPadd, Ownership)),
    printBattleStatus(TokemonP, Enemy),
    (HPadd =:= 0 ->
        level(TokemonP, BefExp),
        expGain(TokemonP, Enemy, ExpGain),
        retract(level(Tokemon, BefExp)),
        NewExp is BefExp + ExpGain,
        asserta(level(TokemonP, NewExp)),
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
    sleep(1.0),
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
    sleep(1.0),
    write('  __ __   ___   __ __      __    __  ____  ____       ______  __ __    ___       ____   ____  ___ ___    ___      __   '),nl,
    write(' |  |  | /   \\ |  |  |    |  |__|  ||    ||    \\     |      ||  |  |  /  _]     /    | /    ||   |   |  /  _]    |  | '),nl,
    write(' |  |  ||     ||  |  |    |  |  |  | |  | |  _  |    |      ||  |  | /  [_     |   __||  o  || _   _ | /  [_     |  | '),nl,
    write(' |  ~  ||  O  ||  |  |    |  |  |  | |  | |  |  |    |_|  |_||  _  ||    _]    |  |  ||     ||  \\_/  ||    _]    |__| '),nl,
    write(' |___, ||     ||  :  |    |  `     | |  | |  |  |      |  |  |  |  ||   [_     |  |_ ||  _  ||   |   ||   [_      __  '),nl,
    write(' |     ||     ||     |     \\      /  |  | |  |  |      |  |  |  |  ||     |    |     ||  |  ||   |   ||     |    |  | '),nl,
    write(' |____/  \\___/  \\__,_|      \\_/\\_/  |____||__|__|      |__|  |__|__||_____|    |___,_||__|__||___|___||_____|    |__| '),nl,
                                                                                                                    

    write('Congratulations !! Write start if you want to play again!! '),!.


