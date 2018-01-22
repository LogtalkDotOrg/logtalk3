%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%"Spider" -- A Sample Adventure Game in Prolog
% David Matuszek, Villanova University
% http://www.csc.villanova.edu/~dmatusze/resources/prolog/spider.html


:- object(spider).

	:- info([
		version is 1.0,
		author is 'David Matuszek, Villanova University. Adapted to Logtalk by Paulo Moura.',
		date is 2010/09/10,
		comment is 'Spider - A Sample Adventure Game.'
	]).

	%:- initialization(start).

	:- public(start/0).

	:- private([at/2, i_am_at/1, alive/1]).
	:- dynamic([at/2, i_am_at/1, alive/1]).

	% This defines my current location

	i_am_at(meadow).

	% These facts describe how the rooms are connected.

	path(spider, d, cave).
	path(cave, u, spider).

	path(cave, w, cave_entrance).
	path(cave_entrance, e, cave).

	path(cave_entrance, s, meadow).
	path(meadow, n, cave_entrance) :-
		at(flashlight, in_hand).
	path(meadow, n, cave_entrance) :-
		write('Go into that dark cave without a light?  Are you crazy?'), nl,
		nl,
		fail.

	path(meadow, s, building).
	path(building, n, meadow).

	path(building, w, cage).
	path(cage, e, building).

	path(closet, w, building).
	path(building, e, closet) :-
		at(key, in_hand).
	path(building, e, closet) :-
		write('The door appears to be locked.'), nl,
		nl,
		fail.

	% These facts tell where the various objects in the game are located.

	at(ruby, spider).
	at(key, cave_entrance).
	at(flashlight, building).
	at(sword, closet).

	% This fact specifies that the spider is alive.

	alive(spider).

	% These rules describe how to pick up an object.

	take(X) :-
		(	at(X, in_hand) ->
			write('You are already holding it!'), nl,
			nl
		;	i_am_at(Place),
			at(X, Place) ->
			retract(at(X, Place)),
			assertz(at(X, in_hand)),
			write('OK.'), nl
		;	write('I do not see it here.'), nl,
			nl
		).

	% These rules describe how to put down an object.

	drop(X) :-
		(	at(X, in_hand),
			i_am_at(Place) ->
			retract(at(X, in_hand)),
			assertz(at(X, Place)),
			write('OK.'), nl
		;	write('You are not holding it!'), nl,
			nl
		).


	% This rule tells how to move in a given direction.

	go(Direction) :-
		(	i_am_at(Here),
			path(Here, Direction, There) ->
			retract(i_am_at(Here)),
			assertz(i_am_at(There)),
			look
		;	write('You cannot go that way.'), nl,
			nl
		).


	% This rule tells how to look about you.

	look :-
		i_am_at(Place),
		describe(Place),
		notice_objects_at(Place).


	% These rules set up a loop to mention all the objects in your vicinity.

	notice_objects_at(Place) :-
		at(X, Place),
		write('There is a '), write(X), write(' here.'), nl,
		fail.

	notice_objects_at(_) :-
		nl.

	% These rules tell how to handle killing the lion and the spider.

	kill :-
		i_am_at(cage),
		write('Oh, bad idea!  You have just been eaten by a lion.'), nl,
		nl,
		die.

	kill :-
		i_am_at(cave),
		write('This is not working.  The spider leg is about as tough'), nl,
		write('as a telephone pole, too.'), nl,
		nl.

	kill :-
		i_am_at(spider),
		at(sword, in_hand),
		retract(alive(spider)),
		write('You hack repeatedly at the back of the spider.  Slimy ichor'), nl,
		write('gushes out of the back of the spider, and gets all over you.'), nl,
		write('I think you have killed it, despite the continued twitching.'), nl,
		nl.

	kill :-
		i_am_at(spider),
		write('Beating on the back of the spider with your fists has no'), nl,
		write('effect.  This is probably just as well.'), nl,
		nl.

	kill :-
		write('I see nothing inimical here.'), nl,
		nl.


	% This rule tells how to die.

	die :-
		finish.


	finish :-
		write('The game is over. Please enter the "halt." command.'), nl,
		nl.


	% This rule just prints out game instructions.

	help :-
		write('Enter commands using standard Prolog syntax.'), nl,
		write('Available commands are:'), nl,
		write('start.                  -- to start the game.'), nl,
		write('n.  s.  e.  w.  u.  d.  -- to go in that direction.'), nl,
		write('take(Object).           -- to pick up an object.'), nl,
		write('drop(Object).           -- to put down an object.'), nl,
		write('kill.                   -- to attack an enemy.'), nl,
		write('look.                   -- to look around you again.'), nl,
		write('help.                   -- to see this message again.'), nl,
		write('halt.                   -- to end the game.'), nl,
		nl.

	do(help) :- !, help.
	do(look) :- !, look.
	do(kill) :- !, kill.
	do(halt) :- !.
	do(take(X)) :- !, take(X).
	do(drop(X)) :- !, drop(X).
	/* These rules define the six direction letters as calls to go/1. */
	do(n) :- !, go(n).
	do(s) :- !, go(s).
	do(e) :- !, go(e).
	do(w) :- !, go(w).
	do(u) :- !, go(u).
	do(d) :- !, go(d).
	do(_) :-
		write('I don''t understand'), nl,
		nl,
		fail.


	% This rule prints out instructions and tells where you are.

	start :-
		do(help),
		do(look),
		repeat,
			write('> '),
			read(Command), nl,
			once(do(Command)),
		Command == halt,
		halt.

	% These rules describe the various rooms.  Depending on
	% circumstances, a room may have more than one description.

	describe(meadow) :-
		(	at(ruby, in_hand) ->
			write('Congratulations!!  You have recovered the ruby'), nl,
			write('and won the game.'), nl,
			nl,
			finish
		;	write('You are in a meadow.  To the north is the dark mouth'), nl,
			write('of a cave; to the south is a small building.  Your'), nl,
			write('assignment, should you decide to accept it, is to'), nl,
			write('recover the famed Bar-Abzad ruby and return it to'), nl,
			write('this meadow.'), nl
		).

	describe(building) :-
		write('You are in a small building.  The exit is to the north.'), nl,
		write('There is a barred door to the west, but it seems to be'), nl,
		write('unlocked.  There is a smaller door to the east.'), nl.

	describe(cage) :-
		write('You are in a den of the lion!  The lion has a lean and'), nl,
		write('hungry look.  You better get out of here!'), nl.

	describe(closet) :-
		write('This is nothing but an old storage closet.'), nl.

	describe(cave_entrance) :-
		write('You are in the mouth of a dank cave.  The exit is to'), nl,
		write('the south; there is a large, dark, round passage to'), nl,
		write('the east.'), nl.

	describe(cave) :-
		(	alive(spider), at(ruby, in_hand) ->
			write('The spider sees you with the ruby and attacks!!!'), nl,
			write('    ...it is over in seconds....'), nl,
			nl,
			die
		;	alive(spider) ->
			write('There is a giant spider here!  One hairy leg, about the'), nl,
			write('size of a telephone pole, is directly in front of you!'), nl,
			write('I would advise you to leave promptly and quietly....'), nl
		;	write('Yecch!  There is a giant spider here, twitching.'), nl
		).

	describe(spider) :-
		(	alive(spider) ->
			write('You are on top of a giant spider, standing in a rough'), nl,
			write('mat of coarse hair.  The smell is awful.'), nl
		;	write('Oh, gross!  You are on top of a giant dead spider!'), nl
		).

:- end_object.
