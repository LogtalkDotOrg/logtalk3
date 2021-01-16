% NANI SEARCH - A sample adventure game
%
% Adventure in Prolog, Amzi!, http://www.amzi.com/AdventureInProlog
%
% Nani Search is designed to illustrate Prolog programming.  It
% is an implementation of the principle example used in
% this tutorial.

:- object(nani_search).

	:- info([
		version is 1:0:0,
		author is 'Amzi! inc.; Logtalk port by Paulo Moura',
		date is 2021-01-16,
		comment is 'NANI SEARCH - A sample adventure game.'
	]).

	:- public(main/0).
	main :-
		nani_search.

	:- private(location/2).
	:- dynamic(location/2).

	:- private(here/1).
	:- dynamic(here/1).

	:- private(turned_on/1).
	:- dynamic(turned_on/1).

	:- private(turned_off/1).
	:- dynamic(turned_off/1).

	:- private(have/1).
	:- dynamic(have/1).

	nani_search :-
		% predicates which are not compiled
		init_dynamic_facts,
		write('NANI SEARCH - A Sample Adventure Game'), nl,
		write('Copyright (C) Amzi! inc. 1990-2010'), nl,
		write('No rights reserved, use it as you wish'), nl,
		nl,
		write('Nani Search is designed to illustrate Prolog programming.'), nl,
		write('As such, it might be the simplest adventure game.'), nl,
		write('The game is the primary example used in this tutorial.'), nl,
		write('Full source is included as well.'), nl,
		nl,
		write('Your persona as the adventurer is that of a three year'), nl,
		write('old.  The Nani is your security blanket.  It is getting'), nl,
		write('late and you''re tired, but you can''t go to sleep'), nl,
		write('without your Nani.  Your mission is to find the Nani.'), nl,
		nl,
		write('You control the game by using simple English commands'), nl,
		write('expressing the action you wish to take.  You can go to'), nl,
		write('other rooms, look at your surroundings, look in things'), nl,
		write('take things, drop things, eat things, inventory the'), nl,
		write('things you have, and turn things on and off.'), nl,
		nl,
		write('Hit any key to continue.'), get_code(_),
		write('Type "help" if you need more help on mechanics.'), nl,
		write('Type "hint" if you want a big hint.'), nl,
		write('Type "quit" if you give up.'), nl,
		nl,
		write('Enjoy the hunt.'), nl,
		% give a look before starting the game
		look,
		command_loop.

	% command_loop - repeats until either the nani is found or the
	%     player types quit

	command_loop:-
		repeat,
			get_command(X),
			do(X),
		(nanifound; X == quit),
		!.

	% do - matches the input command with the predicate which carries out
	%     the command.  More general approaches which might work in the
	%     listener are not supported in the compiler.  This approach
	%     also gives tighter control over the allowable commands.

	%     The cuts prevent the forced failure at the end of "command_loop"
	%     from backtracking into the command predicates.

	do(goto(X)) :- goto(X),!.
	do(nshelp) :- nshelp,!.
	do(hint) :- hint,!.
	do(inventory) :- inventory,!.
	do(take(X)) :- take(X),!.
	do(drop(X)) :- drop(X),!.
	do(eat(X)) :- eat(X),!.
	do(look) :- look,!.
	do(turn_on(X)) :- turn_on(X),!.
	do(turn_off(X)) :- turn_off(X),!.
	do(look_in(X)) :- look_in(X),!.
	do(quit) :- quit,!.

	% These are the predicates which control exit from the game.  If
	% the player has taken the nani, then the call to "have(nani)" will
	% succeed and the command_loop will complete.  Otherwise it fails
	% and command_loop will repeat.

	nanifound :-
		have(nani),
		write('Congratulations, you saved the Nani.'), nl,
		write('Now you can rest secure.'), nl, nl.

	quit :-
		write('Giving up?  It''s going to be a scary night'), nl,
		write('and when you get the Nani it''s not going'), nl,
		write('to smell right.'), nl, nl.

	% The help command

	nshelp :-
		write('Use simple English sentences to enter commands.'), nl,
		write('The commands can cause you to:'), nl,
		nl,
		write('   go to a room          (ex. go to the office)'), nl,
		write('   look around           (ex. look)'), nl,
		write('   look in something     (ex. look in the desk)'), nl,
		write('   take something        (ex. take the apple)'), nl,
		write('   drop something        (ex. drop the apple)'), nl,
		write('   eat something         (ex. eat the apple)'), nl,
		write('   turn something on     (ex. turn on the light)'), nl,
		write('   inventory your things (ex. inventory)'), nl,
		nl,
		write('The examples are verbose, terser commands and synonyms'), nl,
		write('are usually accepted.'), nl, nl,
		write('Hit any key to continue.'), nl,
		get_code(_),
		look.

	hint :-
		write('You need to get to the cellar, and you can''t unless'), nl,
		write('you get some light.  You can''t turn on the cellar'), nl,
		write('light, but there is a flash light in the desk in the'), nl,
		write('office you might use.'), nl, nl,
		look.

	% Initial facts describing the world.  Rooms and doors do not change,
	% so they are compiled.

	room(office).
	room(kitchen).
	room('dining room').
	room(hall).
	room(cellar).

	door(office, hall).
	door(hall, 'dining room').
	door('dining room', kitchen).
	door(kitchen, cellar).
	door(kitchen, office).

	connect(X, Y) :-
		door(X, Y).
	connect(X, Y) :-
		door(Y, X).

	% These facts are all subject to change during the game, so rather
	% than being compiled, they are "asserted" to the listener at
	% run time.  This predicate is called when "nanisrch" starts up.

	init_dynamic_facts :-
		assertz(location(desk, office)),
		assertz(location(apple, kitchen)),
		assertz(location(flashlight, desk)),
		assertz(location('washing machine', cellar)),
		assertz(location(nani, 'washing machine')),
		assertz(location(table, kitchen)),
		assertz(location(crackers, desk)),
		assertz(location(broccoli, kitchen)),
		assertz(here(kitchen)),
		assertz(turned_off(flashlight)).

	furniture(desk).
	furniture('washing machine').
	furniture(table).

	edible(apple).
	edible(crackers).

	tastes_yuchy(broccoli).

	%%%%%%%% COMMANDS %%%%%%%%%%%%%%%%%%%%%%%%%%

	% goto moves the player from room to room
	goto(Room) :-
		% check for legal move
		can_go(Room),
		% check for special conditions
		puzzle(goto(Room)),
		% go there and tell the player
		moveto(Room),
		look.
	goto(_) :-
		look.

	% if there is a connection it is a legal move
	can_go(Room) :-
		here(Here),
		connect(Here, Room),
		!.
	can_go(Room) :-
		respond(['You can''t get to ', Room, ' from here']),
		fail.

	 % update the logicbase with the new room
	moveto(Room) :-
		retractall(here(_)),
		asserta(here(Room)).

	% look lists the things in a room, and the connections
	look :-
		here(Here),
		respond(['You are in the ', Here]),
		write('You can see the following things:'), nl,
		list_things(Here),
		write('You can go to the following rooms:'), nl,
		list_connections(Here).

	list_things(Place) :-
		location(X, Place),
		write('  '), write(X), nl,
		fail.
	list_things(_).

	list_connections(Place) :-
		connect(Place, X),
		write('  '), write(X), nl,
		fail.
	list_connections(_).

	% look_in allows the player to look inside a thing which might
	% contain other things
	look_in(Thing) :-
		% make sure there's at least one
		location(_, Thing),
		write('The '), write(Thing), write(' contains:'), nl,
		list_things(Thing).
	look_in(Thing) :-
		respond(['There is nothing in the ', Thing]).

	% take allows the player to take something.  As long as the thing is
	% contained in the room it can be taken, even if the adventurer hasn't
	% looked in the the container which contains it.  Also the thing
	% must not be furniture.
	take(Thing) :-
		is_here(Thing),
		is_takable(Thing),
		move(Thing, have),
		respond(['You now have the ', Thing]).

	is_here(Thing) :-
		here(Here),
		contains(Thing,Here),
		% don't backtrack
		!.
	is_here(Thing) :-
		respond(['There is no ',Thing,' here']),
		fail.

	 % recursive definition to find things contained in things etc
	contains(Thing, Here) :-
		location(Thing, Here).
	contains(Thing, Here) :-
		location(Thing, X),
		contains(X, Here).

	is_takable(Thing) :-
		furniture(Thing),
		% you can't take the furniture
		respond(['You can''t pick up a ', Thing]),
		!,
		fail.
	% not furniture, ok to take
	is_takable(_).

	move(Thing, have) :-
		% take it from its old place and add to your possessions
		retractall(location(Thing, _)),
		asserta(have(Thing)).

	% drop - allows the player to transfer a possession to a room

	drop(Thing) :-
		% you must have the thing to drop it
		have(Thing),
		% where are we
		here(Here),
		retractall(have(Thing)),
		asserta(location(Thing, Here)).
	drop(Thing) :-
		respond(['You don''t have the ', Thing]).

	% eat, because every adventure game lets you eat stuff.

	eat(Thing) :-
		have(Thing),
		eat2(Thing).
	eat(Thing) :-
		respond(['You don''t have the ', Thing]).

	eat2(Thing) :-
		edible(Thing),
		retractall(have(Thing)),
		respond(['That ', Thing, ' was good']).
	eat2(Thing) :-
		tastes_yuchy(Thing),
		respond(['Three year olds don''t eat ', Thing]).
	eat2(Thing) :-
		respond(['You can''t eat a ', Thing]).

	% inventory list your possesions

	inventory :-
		% make sure you have at least one thing
		have(_),
		write('You have: '),nl,
		list_possessions.
	inventory :-
		write('You have nothing'),nl.

	list_possessions :-
		have(X),
		write('  '), write(X), nl,
		fail.
	list_possessions.

	% turn_on recognizes two cases.  If the player tries to simply turn
	% on the light, it is assumed this is the room light, and the
	% appropriate error message is issued.  Otherwise turn_on has to
	% refer to an object which is turned_off.

	turn_on(light) :-
		respond(['You can''t reach the switch and there''s nothing to stand on']).
	turn_on(Thing) :-
		have(Thing),
		turn_on2(Thing).
	turn_on(Thing) :-
		respond(['You don''t have the ', Thing]).

	turn_on2(Thing) :-
		turned_on(Thing),
		respond([Thing, ' is already on']).
	turn_on2(Thing) :-
		turned_off(Thing),
		retractall(turned_off(Thing)),
		asserta(turned_on(Thing)),
		respond([Thing, ' turned on']).
	turn_on2(Thing) :-
		respond(['You can''t turn a ', Thing, ' on']).

	% turn_off - I didn't feel like implementing turn_off

	turn_off(_) :-
		respond(['I lied about being able to turn things off']).

	% The only special puzzle in Nani Search has to do with going to the
	% cellar.  Puzzle is only called from goto for this reason.  Other
	% puzzles pertaining to other commands could easily be added.

	puzzle(goto(cellar)) :-
		have(flashlight),
		turned_on(flashlight),
		!.
	puzzle(goto(cellar)) :-
		write('You can''t go to the cellar because it''s dark in the'), nl,
		write('cellar, and you''re afraid of the dark.'), nl,
		!,
		fail.
	puzzle(_).

	% respond simplifies writing a mixture of literals and variables

	respond([]) :-
		write('.'), nl, nl.
	respond([H|T]) :-
		write(H),
		respond(T).

	% Simple English command listener.  It does some semantic checking
	% and allows for various synonyms.  Within a restricted subset of
	% English, a command can be phrased many ways.  Also non grammatical
	% constructs are understood, for example just giving a room name
	% is interpreted as the command to goto that room.

	% Some interpretation is based on the situation.  Notice that when
	% the player says turn on the light it is ambiguous.  It could mean
	% the room light (which can't be turned on in the game) or the
	% flash light.  If the player has the flash light it is interpreted
	% as flash light, otherwise it is interpreted as room light.

	get_command(C) :-
		% read a sentence and puts [it,in,list,form]
		readlist(L),
		% call the grammar for command
		command(X, L, []),
		% make the command list a structure
		C =.. X,
		!.
	get_command(_) :-
		respond(['I don''t understand, try again or type help']),
		fail.

	% The grammar doesn't have to be real English.  There are two
	% types of commands in Nani Search, those with and without a
	% single argument.  A special case is also made for the command
	% goto which can be activated by simply giving a room name.

	command([Pred, Arg]) --> verb(Type, Pred), nounphrase(Type, Arg).
	command([Pred]) --> verb(intran, Pred).
	command([goto, Arg]) --> noun(go_place, Arg).

	% Recognize three types of verbs.  Each verb corresponds to a command,
	% but there are many synonyms allowed.  For example the command
	% turn_on will be triggered by either "turn on" or "switch on".

	verb(go_place, goto) --> go_verb.
	verb(thing, V) --> tran_verb(V).
	verb(intran, V) --> intran_verb(V).

	go_verb --> [go].
	go_verb --> [go,to].
	go_verb --> [g].

	tran_verb(take) --> [take].
	tran_verb(take) --> [pick,up].
	tran_verb(drop) --> [drop].
	tran_verb(drop) --> [put].
	tran_verb(drop) --> [put,down].
	tran_verb(eat) --> [eat].
	tran_verb(turn_on) --> [turn,on].
	tran_verb(turn_on) --> [switch,on].
	tran_verb(turn_off) --> [turn,off].
	tran_verb(look_in) --> [look,in].
	tran_verb(look_in) --> [look].
	tran_verb(look_in) --> [open].

	intran_verb(inventory) --> [inventory].
	intran_verb(inventory) --> [i].
	intran_verb(look) --> [look].
	intran_verb(look) --> [look,around].
	intran_verb(look) --> [l].
	intran_verb(quit) --> [quit].
	intran_verb(quit) --> [exit].
	intran_verb(quit) --> [end].
	intran_verb(quit) --> [bye].
	intran_verb(nshelp) --> [help].
	intran_verb(hint) --> [hint].

	% a noun phrase is just a noun with an optional determiner in front.

	nounphrase(Type, Noun) --> det,noun(Type, Noun).
	nounphrase(Type, Noun) --> noun(Type, Noun).

	det --> [the].
	det --> [a].

	% Nouns are defined as rooms, or things located somewhere.  We define
	% special cases for those things represented in Nani Search by two
	% words.  We can't expect the user to type the name in quotes.

	noun(go_place, R) --> [R], {room(R)}.
	noun(go_place, 'dining room') --> [dining, room].

	noun(thing, T) --> [T], {location(T, _)}.
	noun(thing, T) --> [T], {have(T)}.
	noun(thing, flashlight) --> [flash, light].
	noun(thing, 'washing machine') --> [washing, machine].
	noun(thing, 'dirty clothes') --> [dirty, clothes].

	% If the player has just typed light, it can be interpreted three ways.
	% If a room name is before it, it must be a room light.  If the
	% player has the flash light, assume it means the flash light.  Otherwise
	% assume it is the room light.

	noun(thing,light) --> [X,light], {room(X)}.
	noun(thing,flashlight) --> [light], {have(flashlight)}.
	noun(thing,light) --> [light].

	% readlist - read a list of words, based on a Clocksin & Mellish
	% example.

	readlist(L) :-
		write('> '),
		read_word_list(L), writeq(L), nl.

	read_word_list([W| Ws]) :-
		get_code(C),
		readword(C, W, C1),       % Read word starting with C, C1 is first new
		restsent(C1, Ws), !.      % character - use it to get rest of sentence

	restsent(C, []) :- lastword(C), !. % Nothing left if hit last-word marker
	restsent(C, [W1| Ws]) :-
		readword(C, W1, C1),      % Else read next word and rest of sentence
		restsent(C1, Ws).

	readword(C, W, C2) :-
		char_code(' ', C),
		!,
		get_code(C1),
		readword(C1, W, C2).
	readword(C, W, C1) :-         % Some words are single characters
		single_char(C),           % i.e. punctuation
		!,
		char_code(W, C),          % get as an atom
		get_code(C1).
	readword(C, W, C1) :-
		is_num(C),                % if we have a number --
		!,
		number_word(C, W, C1, _). % convert it to a genuine number
	readword(C, W, C2) :-         % otherwise if character does not
		in_word(C, NewC),         % delineate end of word - keep
		!,
		get_code(C1),             % accumulating them until
		restword(C1, Cs, C2),     % we have all the word
		atom_codes(W, [NewC|Cs]). % then make it an atom
	readword(C1, W, C2) :-        % otherwise
		get_code(C1), writeq(c1-C1), nl,
		readword(C1, W, C2).      % start a new word

	restword(C, [NewC| Cs], C2) :-
		in_word(C, NewC),
		!,
		get_code(C1),
		restword(C1, Cs, C2).
	restword(C, [], C).

	single_char(0',).
	single_char(0';).
	single_char(0':).
	single_char(0'?).
	single_char(0'!).
	single_char(0'.).

	in_word(C, C) :- C >= 0'a, C =< 0'z.
	in_word(C, L) :- C >= 0'A, C =< 0'Z, L is C + 32.
	in_word(0'\', 0'\').
	in_word(0'-, 0'-).

	% Have character C (known integer) - keep reading integers and build
	% up the number until we hit a non-integer. Return this in C1,
	% and return the computed number in W.

	number_word(C, W, C1, Pow10) :-
		is_num(C),
		!,
		get_code(C2),
		number_word(C2, W1, C1, P10),
		Pow10 is P10 * 10,
		W is truncate(((C - 0'0) * Pow10) + W1).
	number_word(C, 0, C, 0.1).


	is_num(C) :-
		C =< 0'9,
		C >= 0'0.

	% These symbols delineate end of sentence

	% end if new line entered
	lastword(10).
	lastword(0'.).
	lastword(0'!).
	lastword(0'?).

:- end_object.
