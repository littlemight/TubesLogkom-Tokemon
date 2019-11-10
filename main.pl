:- include('map.pl').
:- include('tools.pl').
:- include('tokemon.pl').
start :-
    initMap,
    random(1, 6, N),
    initNormal(N),
    map.