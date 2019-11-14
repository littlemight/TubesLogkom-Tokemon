tokemonTingle :- random(1, 101, RNG), RNG >= 90, write('What the frick? Tokemon Tingle is not working.'), nl, !.
tokemonTingle :- random(1, 101, RNG), RNG >= 80, write('Your Tokemon Tingle is pretty wack right now.'), nl, !.
tokemonTingle :- random(1, 101, RNG), RNG > 70, write('Come on Tokemon Tingle...'), nl, !.
tokemonTingle :-
    posPlayer(XPlayer, YPlayer), 
    findall(Tokemon, tokemon(Tokemon, _, _, _, 0), ListTokemon),
    findNearestTokemon(ListTokemon, XPlayer, YPlayer, NearestTokemon),
    tokemon(NearestTokemon, XNearest, YNearest, _, _),
    getDirection(XPlayer, YPlayer, XNearest, YNearest, Direction),
    narrateSense(Direction),
    (visible -> % kalo god mode
        format('You: (~w, ~w)', [XPlayer, YPlayer]), nl,
        format('Nearest: ~w (~w, ~w)', [NearestTokemon, XNearest, YNearest]), nl
    ;   !
    ),
    !.

calcDistance(X1, Y1, X2, Y2, D) :-
    D is sqrt((X1 - X2)**2 + (Y1 - Y2)**2).

findNearestTokemon([Tokemon], _, _, Tokemon) :- !.
findNearestTokemon([Tokemon|Tail], XPlayer, YPlayer, MTokemon) :-
    findNearestTokemon(Tail, XPlayer, YPlayer, MTokemon),
    tokemon(MTokemon, XCur, YCur, _, 0),
    tokemon(Tokemon, XCur1, YCur1, _, 0),
    calcDistance(XPlayer, YPlayer, XCur, YCur, DisM),
    calcDistance(XPlayer, YPlayer, XCur1, YCur1, Dis),
    DisM =< Dis, !.

findNearestTokemon([Tokemon | Tail], XPlayer, YPlayer, Tokemon) :-
    findNearestTokemon(Tail, XPlayer, YPlayer, MTokemon),
    tokemon(MTokemon, XCur, YCur, _, 0),
    tokemon(Tokemon, XCur1, YCur1, _, 0),
    calcDistance(XPlayer, YPlayer, XCur, YCur, DisM),
    calcDistance(XPlayer, YPlayer, XCur1, YCur1, Dis),
    DisM > Dis, !.
    
getDirection(XPlayer, YPlayer, XNearest, YNearest, Dir) :-
    (YNearest < YPlayer ->
        Dir is 1
    ; XNearest > XPlayer ->
        Dir is 2
    ; YNearest > YPlayer ->
        Dir is 3
    ; Dir is 4
    ), 
    !.

narrateSense(Direction) :-
    random(1, 101, RNG),
    (RNG =< 25 ->
        write('You sense something '), writeDirection(Direction), write('!'), nl
    ; RNG =< 50 ->
        write('Something\'s moving '), writeDirection(Direction), write('!'), nl
    ; RNG =< 75 ->
        write('Maybe it\'s '), writeDirection(Direction), write('???'), nl
    ;   writeDirection(Direction), write(', definitely.'), nl
    ),
    !.

writeDirection(Direction) :-
    (Direction =:= 1 ->
        write('North')
    ; Direction =:= 2 ->
        write('East')
    ; Direction =:= 3 ->
        write('South')
    ;   write('West')
    ),
    !.