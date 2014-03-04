%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/* "Sleepy" -- a sample adventure game, by David Matuszek. */

% http://www.csc.vill.edu/~dmatusze/resources/prolog/sleepy.html

:- object(sleepy).

	:- info([
		version is 1.0,
		author is 'David Matuszek, Villanova University. Adapted to Logtalk by Paulo Moura.',
		date is 2010/09/10,
		comment is 'Sleepy - A Sample Adventure Game.'
	]).

	%:- initialization(start).

	:- public(start/0).

	:- private([at/2, i_am_at/1, i_am_holding/1, alive/1, lit/1, visible_object/1]).
	:- dynamic([at/2, i_am_at/1, i_am_holding/1, alive/1, lit/1, visible_object/1]).


	/* This defines my current location. */

	i_am_at(bedroom).

	i_am_holding(nothing).

	/* These facts describe how the rooms are connected. */

	path(bedroom, n, den) :- lit(bedroom).
	path(bedroom, n, den) :-
		write('You trip over something in the dark.'), nl,
		!, fail.
	path(den, s, bedroom).

	path(bedroom, d, bed).
	path(bed, u, bedroom).


	% These facts tell where the various objects in the game
	%   are located.

	at(flyswatter, den).
	at(fly, bedroom).
	at('light switch', den).
	at('light switch', bedroom).


	/* These facts specify some game-specific information. */

	alive(fly).

	lit(bedroom).
	lit(den).

	visible_object('light switch').

	/* These rules describe how to pick up an object. */

	take(fly) :-
		write('It is too fast for you!'), nl,
		nl,
		!, fail.

	take('light switch') :-
		take(switch).

	take(switch) :-
		write('It is firmly embedded in the wall!'), nl,
		nl,
		!, fail.

	take(X) :-
		(	i_am_holding(X) ->
			write('You are already holding it!'), nl,
			nl
		;	i_am_at(Place),
			at(X, Place) ->
			retract(at(X, Place)),
			assertz(i_am_holding(X)),
			write('OK.'), nl
		;	write('I do not see it here.'), nl,
			nl
		).

	/* These rules describe how to put down an object. */

	drop(X) :-
		(	i_am_holding(X) ->
			i_am_at(Place),
			retract(i_am_holding(X)),
			assertz(at(X, Place)),
			write('OK.'), nl
		;	write('You are not holding it!'), nl,
			nl
		).

	do(sleep) :- !, sleep.
	do(help) :-  !, help.
	do(look) :-  !, look.
	do(on) :-  !, on.
	do(off) :- !, off.
	do(halt) :- !.
	do(take(X)) :- !, take(X).
	do(drop(X)) :- !, drop(X).
	do(use(X)) :-  !, use(X).
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


	/* This rule tells how to move in a given direction. */

	go(Direction) :-
		(	i_am_at(Here),
			path(Here, Direction, There) ->
			retract(i_am_at(Here)),
			assertz(i_am_at(There)),
			look
		;	write('You can not go that way.'), nl,
			nl
		).

	/* This rule tells how to look about you. */

	look :-
		i_am_at(Place),
		describe(Place),
		notice_objects_at(Place).


	/* These rules set up a loop to mention all the objects in your vicinity. */

	notice_objects_at(Place) :-
		lit(Place),
		at(X, Place),
		visible_object(X),
		write('There is a '), write(X), write(' here.'), nl,
		fail.

	notice_objects_at(_) :-
		nl.


	/* These rules are specific to this particular game. */

	use(flyswatter) :-
		swat(fly).

	use(bed) :-
		i_am_at(bedroom),
		go(d).

	use(bed) :-
		write('It is in the bedroom!'), nl,
		nl,
		!, fail.

	use(switch) :-
		(	i_am_at(Place),	lit(Place) ->
			off
		;	on
		).

	on :-
		i_am_at(bed),
		write('You can not reach the light switch from here.'), nl,
		nl,
		!, fail.

	on :-
		i_am_at(Place),
		lit(Place),
		!,
		write('The lights are already on.'), nl,
		nl.

	on :-
		i_am_at(Place),
		assertz(lit(Place)),
		write('The room lights come on.'), nl,
		optional_buzz_off,
		!,
		look.

	off :-
		i_am_at(bed),
		write('You can not reach the light switch from here.'), nl,
		nl,
		!, fail.

	off :-
		i_am_at(Place),
		retract(lit(Place)),
		optional_buzz_off,
		!,
		write('It is now dark in here.'), nl.

	off :-
		write('The lights are already off.'), nl.

	sleep :-
		\+ i_am_at(bed),
		write('You find it hard to sleep standing up.'), nl,
		nl,
		!, fail.

	sleep :-
		lit(bedroom),
		write('You can not get to sleep with the light on.'), nl,
		nl,
		!, fail.

	sleep :-
		lit(den),
		write('The light from the den is keeping you awake.'), nl,
		nl,
		!, fail.

	sleep :- 
		(i_am_holding(flyswatter); at(flyswatter, bed)),
		write('What? Sleep with a dirty old flyswatter?'), nl,
		nl,
		!, fail.

	sleep :-
		alive(fly),
		write('As soon as you start to doze off, a fly lands'), nl,
		write('on your face and wakes you up again.'), nl,
		nl,
		make_visible(fly),
		make_visible(flyswatter),
		!, fail.

	sleep :-
		write('Ahhh...you (yawn) made...it...zzzzzzzz.'), nl,
		nl,
		finish.

	swat(fly) :-
		swat.

	swat :-
		i_am_at(Place),
		\+ lit(Place),
		write('You flail aimlessly in the dark!'), nl.

	swat :-
		\+ i_am_holding(flyswatter),
		write('You are not holding the flyswatter.'), nl,
		!, fail.

	swat :-
		\+ alive(fly),
		write('He is dead, Jim.'), nl.

	swat :-
		i_am_at(Place),
		\+ at(fly, Place),
		write('You swish the flyswatter through the air.'), nl.

	/* Have flyswatter, room is lit, fly is here and alive. */

	swat :-
		(	buzz_off ->
			write('The fly escapes into the other room.'), nl
		;	write('Success! You killed that pesky fly!'), nl,
			retract(alive(fly))
		).

	%swat :- /* For debugging... */
	%	write('You must have forgotten a case!', nl).

	make_visible(X) :-
		(	visible_object(X) ->
			true
		;	assertz(visible_object(X))
		).

	buzz_off :-
		(	at(fly, bedroom),
			lit(den) ->
			retract(at(fly, bedroom)),
			assertz(at(fly, den))
		;	at(fly, den),
			lit(bedroom) ->
			retract(at(fly, den)),
			assertz(at(fly, bedroom))
		).

	optional_buzz_off :-
		buzz_off.
	optional_buzz_off.


	/* Under UNIX, the "halt." command quits Prolog but does not
	   remove the output window. On a PC, however, the window
	   disappears before the final output can be seen. Hence this
	   routine requests the user to perform the final "halt." */

	finish :-
		write('The game is over. Please enter the "halt." command.'), nl,
		nl.



	/* This rule just prints out game help. */

	help :-
		write('Enter commands using standard Prolog syntax.'), nl,
		write('Available commands are:'), nl,
		write('start.                  -- to start the game.'), nl,
		write('n.  s.  e.  w.  u.  d.  -- to go in that direction.'), nl,
		write('take(Object).           -- to pick up an object.'), nl,
		write('drop(Object).           -- to put down an object.'), nl,
		write('use(Object).            -- to manipulate an object.'), nl,
		write('look.                   -- to look around you again.'), nl,
		write('on.  off.               -- to control the room lights.'), nl,
		write('sleep.                  -- to try to go to sleep.'), nl,
		write('help.                   -- to see this message again.'), nl,
		write('halt.                   -- to end the game.'), nl,
		nl.


	/* This rule prints out instructions and tells where you are. */

	start :-
		do(help),
		do(look),
		repeat,
			write('> '),
			read(Command), nl,
			once(do(Command)),
		Command == halt,
		halt.


	/* These rules describe the various rooms.  Depending on
	   circumstances, a room may have more than one description. */

	describe(bedroom) :-
		(	lit(bedroom) ->
			write('You are in a bedroom with a large, comfortable bed.'), nl,
			write('It has been a long, tiresome day, and you would like'), nl,
			write('nothing better than to go to sleep.'), nl
		;	write('You are in your bedroom. It is nice and dark.'), nl
		).

	describe(bed) :-
		write('You are in bed, and it feels great!'), nl.

	describe(den) :-
		(	lit(den) ->
			write('You are in your den. There is a lot of stuff here,'), nl,
			write('but you are too sleepy to care about most of it.'), nl
		;	write('You are in your den. It is dark.'), nl
		).

:- end_object.
