:- dynamic(height/1).
:- dynamic(width/1).
:- dynamic(gym/2).
:- dynamic(fence/2).
:- dynamic(posPlayer/2).
:- dynamic(visible/0).

hesoyam :- \+(visible), asserta(visible), write('You can now see all the Tokemons.'), nl, !.
hesoyam :- visible, retract(visible), write('Now you\'re just a peasant Tokemon trainer.'), nl, !.

initMap :- 
  /* Get Map Size */
  random(10, 21, H),
  random(10, 21, W),
  asserta(height(H)),
  asserta(width(W)),

  NRandom is ((H * W) div 15) + 1,
  random(1, NRandom, NFence),
  generateFence(NFence),

  /* Get Gym Position */
  random(1, 4, NGymRandom),
  generateGym(NGymRandom)
  .

generateFence(0) :- !.
generateFence(N) :-
  height(H), width(W),
  repeat,
    random(1, H, YFence), random(1, W, XFence),
    (fence(XFence, YFence) -> fail
    ; gym(XFence, YFence) -> fail
    ; asserta(fence(XFence, YFence)), !
    ),
  Next is N - 1,
  generateFence(Next),
  !.

generateGym(0) :- !.
generateGym(N) :-
  height(H), width(W),
  repeat,
    random(1, H, YGym), random(1, W, XGym),
    (gym(XGym, YGym) -> fail
    ; fence(XGym, YGym) -> fail
    ; asserta(gym(XGym, YGym)), !
    ),
  Next is N - 1,
  generateGym(Next),
  !.


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
printPos(X, Y) :- fence(X, Y), !, write('X').
printPos(X, Y) :- visible, tokemon(Tokemon, X, Y, _, 0), normal(Tokemon), write('T').
printPos(X, Y) :- tokemon(Tokemon, X, Y, _, 0), legendary(Tokemon), write('L').
printPos(_, _) :- write('-'), !.

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
