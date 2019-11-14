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
    write('                  ___            ___           ___           ___           ___           ___ '),nl,
    write('      ___        /  /\\         /__/|         /  /\\         /__/\\         /  /\\         /__/\\ '),nl,   
    write('     /  /\\     /  /::\\        |  |:|        /  /:/_       |  |::\\       /  /::\\        \\  \\:\\ '),nl, 
    write('    /  /:/     /  /:/\\:\\      |  |:|       /  /:/ /\\      |  |:|:\\     /  /:/\\:\\        \\  \\:\\  '),nl,
    write('   /  /:/     /  /:/  \\:\\   __|  |:|      /  /:/ /:/_   __|__|:|\\:\\   /  /:/  \\:\\   _____\\__\\:\\  '),nl,
    write('  /  /::\\    /__/:/ \\__\\:\\ /__/\\_|:|____ /__/:/ /:/ /\\ /__/::::| \\:\\ /__/:/ \\__\\:\\ /__/::::::::\\  '),nl,
    write(' /__/:/\\:\\   \\  \\:\\ /  /:/ \\  \\:\\/:::::/ \\  \\:\\/:/ /:/ \\  \\:\\~~\\__\\/ \\  \\:\\ /  /:/ \\  \\:\\~~\\~~\\/ '),nl,
    write(' \\__\\/  \\:\\   \\  \\:\\  /:/   \\  \\::/~~~~  \\  \\::/ /:/   \\  \\:\\        \\  \\:\\  /:/   \\  \\:\\  ~~~  '),nl,
    write('      \\ \\:\\   \\  \\:\\/:/     \\  \\:\\        \\  \\:\\/:/     \\  \\:\\        \\  \\:\\/:/     \\  \\:\\      '),nl,
    write('       \\__\\/    \\  \\::/       \\  \\:\\        \\  \\::/       \\  \\:\\        \\  \\::/       \\  \\:\\    '),nl,
    write('                 \\__\\/         \\__\\/         \\__\\/         \\__\\/         \\__\\/         \\__\\/    '),nl,nl,

	write('Selamat datang di Tokemon 10.0, game ini tidak ada hubungannya sama sekali dengan *okemon :)'), nl,
	write('Tokemon-tokemon yang ada di database sangat bervariasi, dan pada awal game diberikan random kepada pemain'), nl,
	write('Jadi kalau dapet tokemon yang cupu restart aja ya game-nya hehehehe'),nl,
	help,
	asserta(status(roam)),
    initMap,
    random(5, 10, N),
    initNormal(N),
    random(2, 3, NLegendary),
    initLegendary(NLegendary),
    initPlayer
    .
quit :-
    reset,
    write('Kau keluar dari game :))').