:- include('map.pl').
:- include('tools.pl').
:- include('tokemon.pl').
:- include('player.pl').
:- include('help.pl').
:- include('loadsave.pl').
:- include('compass.pl').
:- dynamic(status/1).

/* STATUS : KONDISI PERMAINAN */
command(start).
command(help).
command(quit).
command(w). command(a). command(s). command(d).
command(map).
command(heal).
command(status).
command(pick(_)).
command(attack).
command(specialAttack).
command(fight).
command(run).
command(capture).
command(ignore).
command(drop(_)).
command(hesoyam).
command(tokemonTingle).
command(status(_)).

processInput(X) :- \+command(X), write('Invalid command!'), nl.
processInput(X) :- command(X), X.

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
    initPlayer,
    repeat,
        write('>>> '),
        read(X),
        processInput(X),
        nl, X = quit
    , !.

quit :-
    reset,
    write('Kau keluar dari game :))').