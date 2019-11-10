:- include('map.pl').
:- include('tools.pl').
:- include('tokemon.pl').
:- include('player.pl').
:- dynamic(status/1).

/* STATUS : KONDISI PERMAINAN */
status(idle).

start :- \+status(idle), write('waduh sori ga bisa nih gan'),!, fail.
start :-
    retract(status(idle)),
    asserta(status(roam)),
    initMap,
    random(1, 3, N),
    initNormal(N),
    initPlayer,
    map.
