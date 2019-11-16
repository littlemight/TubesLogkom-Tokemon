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
command(status).
command(listing).
command(saveGame(_)).
command(loadGame(_)).

processInput(X) :- \+command(X), write('Invalid command!'), nl.
processInput(X) :- command(X), X.
processInput(X) :- X = quit, halt, !.

start :- status(_), write('You have already started the game!'),nl,!, fail.
start :-
    reset,
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

    write('Welcome to Tokemon 10.0, this game has no connection whatsoever with P*okemon :)'), nl,
    write('You will be given a random tokemon at the start of the game, it may be strong, it may be weak'), nl,
    write('If your tokemon is weak af, just restart the game'), nl,

	help,
	asserta(status(roam)),
    initMap,
    random(5, 10, N),
    resetLvl,
    initPlayer,
    initNormal(N),
    random(2, 4, NLegendary),
    initLegendary(NLegendary),
    repeat,
        write('>>> '),
        read(X),
        processInput(X),
        nl, X = quit
    , !.

quit :- \+status(_), write('You have not started any game yet!'),nl,!.

quit :-
    reset,
    write('You quit the game').
