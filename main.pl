:- include('map.pl').
:- include('tools.pl').
:- include('tokemon.pl').
:- include('player.pl').

start :-
    initMap,
    random(1, 3, N),
    initNormal(N),
    initPlayer,
    map.
