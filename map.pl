:- dynamic(height/1).
:- dynamic(width/1).
:- dynamic(gym/2).
:- dynamic(fence/2).
:- dynamic(posPlayer/2).

initMap :- 
  /* Get Map Size */
  random(10, 20, H),
  random(10, 20, W),
  asserta(height(H)),
  asserta(width(W)),

  /* Get Player Position */
  random(1, H, YPlayer),
  random(1, W, XPlayer),
  asserta(posPlayer(XPlayer, YPlayer)),

  /* Get Gym Position */
  random(1, H, XGym),
  random(1, W, YGym),
  asserta(gym(XGym, YGym))
  .

isEdgeW(_, Y) :- Y =:= 0, !.
isEdgeA(X, _) :- X =:= 0, !.
isEdgeS(_, Y) :- 
  height(YMax),
  YEdge is YMax + 1,
  Y =:= YEdge, !.
isEdgeD(X, _) :- 
  width(XMax),
  XEdge is XMax + 1,
  X =:= XEdge, !.
isEdge(X, Y) :- isEdgeW(X, Y); isEdgeA(X, Y); isEdgeS(X, Y); isEdgeD(X, Y).

printPos(X, Y) :- gym(X, Y), !, write('G').
printPos(X, Y) :- posPlayer(X, Y), !, write('P').
printPos(X, Y) :- isEdge(X, Y), !, write('X').
printPos(X, Y) :- tokemon(_, _, X, Y, _, _), !, write('T').
printPos(X, Y) :- write('-'), !.

map :-
  width(W),
  height(H),
  XMin is 0,
  XMax is W + 1,
  YMin is 0,
  YMax is H + 1,
  forall(between(YMin, YMax, J), (
    forall(between(XMin, XMax, I),(
      printPos(I, J)
    )),
    nl
  )),
  !
  .

w :-
  retract(posPlayer(X, Y)),
  YNew is Y - 1,
  (
    Y > 1, \+ (fence(X, YNew)) ->
    asserta(posPlayer(X, YNew));
    asserta(posPlayer(X, Y))
  ), roamAllTokemon,
  map
  .

a :-
  retract(posPlayer(X, Y)),
  XNew is X - 1,
  (
    X > 1, \+(fence(XNew, Y)) ->
    asserta(posPlayer(XNew, Y));
    asserta(posPlayer(X, Y))
  ), roamAllTokemon,
  map
  .

s :-
  retract(posPlayer(X, Y)),
  height(YMax),
  YNew is Y + 1,
  (
    Y < YMax, \+ (fence(X, YNew)) ->
    asserta(posPlayer(X, YNew));
    asserta(posPlayer(X, Y))
  ), roamAllTokemon, 
  map
  .

d :-
  retract(posPlayer(X, Y)),
  width(XMax),
  XNew is X + 1,
  (
    X < XMax, \+(fence(XNew, Y)) ->
      asserta(posPlayer(XNew, Y))
    ; asserta(posPlayer(X, Y))
  ), roamAllTokemon,
  map
  .