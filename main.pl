:- include('map.pl').
:- include('tools.pl').
:- include('tokemon.pl').
:- include('player.pl').
:- include('help.pl').
:- dynamic(status/1).
:- include('loadsave.pl').

/* STATUS : KONDISI PERMAINAN */


start :- status(_), write('You have already started the game!'),!, fail.
start :-
	write('Selamat datang di Tokemon murahan, ini adalah build alpha. Maklum ya'), nl,
	write('Semua status Tokemon masih belum fix, jadi maklum ya kalo dapet Tokemon yang HP nya 1.'), nl,
	write('Sekarang juga lagi God Mode (kamu bisa lihat Tokemon di Peta), not included in final game :)'), nl,
	
	help,
	asserta(status(roam)),
    initMap,
    random(1, 3, N),
    initNormal(N),
    random(2, 3, NLegendary),
    initLegendary(NLegendary),
    initPlayer
    .
quit :-
    reset,
    write('Kau keluar dari game :))').