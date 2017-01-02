%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


% example adapted from: http://www1.pacific.edu/~dcliburn/141/wumpusPROLOG.html

% PROLOG version of HUNT THE WUMPUS
% Written in January of 2002 by Dan Cliburn

% Consult this file in a PROLOG interpreter
% and type:  main.  to play.

% Commands can be written with short sentences.
% They all start with a verb and some end with
% a prepositional phrase (depending on the command).
% Here is the list:
%
% look.
% inventory.
% talk (to a living thing).
% goto (a location).
% fight (a monster).
% buy (an item).
% sell (an item).
% take (an item).
% board (transportation).
% light (an item).

% Acknowledgement goes to Amzi! inc. for their
% excellent online tutorial and samples (some
% of which are used in this game).  See
% http://www.amzi.com/AdventureInProlog/index.htm


:- object(wumpus).

	:- info([
		version is 1.0,
		author is 'Dan Cliburn. Adapted to Logtalk by Paulo Moura.',
		date is 2010/09/10,
		comment is 'Hunt the Wumpus text adventure.'
	]).

	%:- initialization(main).

	:- public(main/0).

	:- private([in/1, item/1, have/1, lit/1, location/2, path/2, slain/1, transporation/1]).
	:- dynamic([in/1, item/1, have/1, lit/1, location/2, path/2, slain/1, transporation/1]).

	% The facts about the world at the beginning of the game.
	% First, The places the player can visit
	place(praire).
	place(canyon).
	place(foothills).
	place(mountain).
	place(dock).
	place(town).
	place(shop).
	place(forest).
	place(woods).
	place(sinkhole).
	place(cave).
	place(river).

	% Takeable items
	item(hay).
	item(diamond).
	item(matches).

	% Things the player can buy
	buyable(sword).
	buyable(bow).

	% Things the player can sell
	sellable(boat).
	sellable(diamond).

	% Things the player can use for transportation
	transportation(boat).

	% People the player can interact with in the game
	person(fisherman).
	person(blacksmith).
	person(archer).

	% Monsters the player encounters
	monster(troll).
	monster(wumpus).

	% Properties of objects
	lightable(hay).

	% Locations of objects in places
	location(boat, river).
	location(hay, woods).
	location(sword, shop).
	location(bow, canyon).
	location(diamond, river).
	location(matches, foothills).
	location(fisherman, dock).
	location(blacksmith, shop).
	location(archer, canyon).
	location(troll, mountain).
	location(wumpus, forest).

	% Ways to get from one place to another
	path(sinkhole, cave).
	path(praire, sinkhole).
	path(praire, canyon).
	path(praire, foothills).
	path(foothills, mountain).
	path(foothills, town).
	path(town, shop).
	path(town, dock).
	path(town, woods).
	path(woods, forest).
	path(praire, woods).

	% Now the rules of the game
	% First we define that paths work in both directions
	pathway(X, Y) :- path(Y, X).
	pathway(X, Y) :- path(X, Y).

	% Now various rules that implement all of our games commands
	list_things(Place) :-
		location(X, Place),
		write('  '),
		write(X), nl,
		fail.
	list_things(_).

	list_paths(Place) :-
		pathway(Place, X),
		write('  '),
		write(X), nl,
		fail.
	list_paths(_).

	help :-
		write('Commands can be written with short sentences.'), nl,
		write('They all start with a verb and some end with'), nl,
		write('a prepositional phrase (depending on the command).'), nl,
		write('Here is the list:'), nl,
		write('  help'), nl,
		write('  look'), nl,
		write('  inventory'), nl,
		write('  talk (to a living thing)'), nl,
		write('  goto (a location)'), nl,
		write('  fight (a monster)'), nl,
		write('  buy (an item)'), nl,
		write('  sell (an item)'), nl,
		write('  take (an item)'), nl,
		write('  board (transportation)'), nl,
		write('  light (an item)'), nl,
		write('  end'), nl.

	look :-
		\+ in(cave),
		in(Place),
		write('You are in the '), write(Place), nl,
		write('You can see:'), nl,
		list_things(Place),
		write('You can go to:'), nl,
		list_paths(Place).
	look :-
		in(cave),
		lit(hay),
		write('You see a rocky slope leading towards an underground'), nl,
		write('river that you may be able to get to.'), nl,
		write('You can go to: '), nl,
		asserta(path(river,cave)),
		write('  '),
		write(sinkhole), nl,
		write('  '),
		write(river), nl.
	look :-
		in(cave),
		\+ lit(hay),
		write('It is very dark in here'), nl,
		write('You can go to: '), nl,
		write('  '),
		write(sinkhole), nl.

	list_inventory :-
		have(X), write('  '),
		write(X),
		fail.
	list_inventory.

	inventory :-
		write('You have: '),
		list_inventory,
		nl.

	goto(river) :-
		can_go(river),
		move(river),
		retract(path(river, cave)),
		write('OUCH!  You stumble down the slope and nearly twist'), nl,
		write('your ankle.  There''s no way you are getting back'), nl,
		write('up that way!  Your hay has almost completely burned up.'), nl,
		retract(have(hay)),
		retract(lit(hay)),
		look.
	goto(Place) :-
		can_go(Place),
		move(Place),
		look.
	goto(_) :-
		write('Uh... I''m not sure how to do that.'), nl,
		fail.

	can_go(Place) :-
		in(X),
		pathway(X, Place).

	move(Place) :-
		retract(in(_)),
		asserta(in(Place)).

	take(X) :-
		can_take(X),
		take_object(X).

	can_take(Thing) :-
		in(Place),
		item(Thing),
		location(Thing, Place).
	can_take(Thing) :-
		in(Place),
		item(Thing),
		location(Thing, Place),
		buyable(Thing),
		write('Hey, you have to buy the '), write(Thing),
		write('!'),
		nl, fail.
	can_take(Thing) :-
		\+ buyable(Thing),
		write('You can''t do that'),
		nl, fail.

	take_object(X) :-
		retract(location(X, _)),
		asserta(have(X)),
		write('taken'), nl.

	got_matches :-
		have(matches).
	got_matches :-
		\+ have(matches),
		write('You need matches'), nl,
		fail.

	got_item(X) :-
		have(X).
	got_item(X) :-
		\+ have(X),
		write('You can''t light what you don''t have'), nl,
		fail.

	can_light(X) :-
		lightable(X).
	can_light(X) :-
		\+ lightable(X),
		write('You can''t light that'), nl,
		fail.

	light(matches) :-
		have(matches),
		write('A match quickly burns out'), nl.
	light(matches) :-
		write('You don''t have any matches'), nl.

	light(X) :-
		got_item(X),
		can_light(X),
		got_matches,
		write(X),
		write(' is now burning brightly'),
		asserta(lit(X)).

	ride :-
		in(river),
		write('You board the boat and it wisks you on an exciting'), nl,
		write('rafting trip down the river.  You end up at a dock.'), nl,
		retract(in(_)),
		asserta(in(dock)),
		retract(location(boat, river)),
		asserta(location(boat, dock)),
		look.
	ride :-
		\+ in(river),
		write('That was fun!').

	board(Thing) :-
		in(Place),
		transportation(Thing),
		location(Thing, Place),
		ride.
	board(Thing) :-
		\+ transporation(Thing),
		write('You can''t get into that!'), nl,
		fail.

	talk(fisherman) :-
		in(dock),
		location(fisherman, dock),
		write('I normally fish at this time of day, but'), nl,
		write('my boat sank last week.  I''ve been looking'), nl,
		write('to buy another.'), nl.
	talk(archer) :-
		in(canyon),
		location(archer, canyon),
		write('I''ve been shooting arrows over this canyon'), nl,
		write('for years.  My arms are starting to get tired'), nl,
		write('of pulling this bow string back.  I''d sell it for'), nl,
		write('something cool, perhaps a rare gem!'), nl.
	talk(blacksmith) :-
		in(shop),
		location(sword, shop),
		write('You need money if you want to buy that sword.'), nl.
	talk(blacksmith) :-
		in(shop),
		\+ location(sword, shop),
		write('I don''t have anything to sell right now!'), nl.
	talk(X) :-
		\+ person(X),
		\+ monster(X),
		write('Talking to inanimate objects is a sign of a weak mind!'), nl,
		fail.
	talk(X) :-
		in(Y),
		monster(X),
		location(X, Y),
		write('That creature would much rather eat you than talk to you!'), nl,
		write('You''d probably best be running away now...'), nl.
	talk(X) :-
		person(X),
		in(Y),
		\+ location(X, Y),
		write('I probably can''t yell loud enough for him to hear me!'), nl,
		fail.

	sell(boat) :-
		in(dock),
		location(boat, dock),
		write('Great, I need one of those.  Here''s some money.'), nl,
		write('The fisherman gives you money, jumps in the boat,'), nl,
		write('and rows away.'), nl,
		retract(location(boat, dock)),
		retract(location(fisherman, dock)),
		asserta(have(money)).
	sell(diamond) :-
		in(canyon),
		location(bow, canyon),
		have(diamond),
		write('Sure, I''ll trade you my bow for that diamond!  I''m'), nl,
		write('fresh out of arrows though!  The archer grabs the'), nl,
		write('diamond, tosses you the bow, and runs off gleefully!'), nl,
		retract(location(archer, canyon)),
		retract(location(bow, canyon)),
		retract(have(diamond)),
		asserta(have(bow)).
	sell(_) :-
		write('I''m not sure that''s possible.'), nl,
		fail.

	buy(bow) :-
		in(canyon),
		location(bow,canyon),
		have(diamond),
		write('Sure, I''ll trade you my bow for that diamond!  I''m'), nl,
		write('fresh out of arrows though!  The archer grabs the'), nl,
		write('diamond, tosses you the bow, and runs off gleefully!'), nl,
		retract(location(archer, canyon)),
		retract(location(bow, canyon)),
		retract(have(diamond)),
		asserta(have(bow)).
	buy(sword) :-
		in(shop),
		location(sword, shop),
		have(money),
		write('The blacksmith takes your money and gives you the sword.'), nl,
		retract(have(money)),
		retract(location(sword, shop)),
		asserta(have(sword)).
	buy(sword) :-
		in(shop),
		location(sword, shop),
		\+ have(money),
		write('You need money to buy the sword'), nl.
	buy(_) :-
		write('How do you propose to do that?'), nl,
		fail.

	fight(X) :-
		person(X),
		write('Trying to fight other humans will just land you in jail!'), nl.
	fight(troll) :-
		in(X),
		location(troll, X),
		have(sword),
		write('With your trusty sword you quickly slay the troll!'), nl,
		write('You find in his pile of treasures a golden arrow.'), nl,
		retract(location(troll, mountain)),
		asserta(location(arrow, mountain)),
		asserta(item(arrow)).
	fight(troll) :-
		in(X),
		location(troll, X),
		\+ have(sword),
		write('You begin attacking the troll with martial arts ... wait'), nl,
		write('a minute, you don''t know any martial arts.  The troll'), nl,
		write('rips you limb from limb and has you for breakfast'), nl,
		write('... Suddenly you wake up.  It was all a bad dream ...'), nl,
		write('or was it???'), nl,
		retract(in(X)),
		asserta(in(woods)).
	fight(wumpus) :-
		in(X),
		location(wumpus, X),
		have(bow),
		have(arrow),
		write('The wumpus looks at you annoyed and begins to snarl.'), nl,
		write('You pull our the archer''s bow and draw back the golden'), nl,
		write('arrow.  The arrow flies through the air and pierces the'), nl,
		write('wumpus''s scales.  He falls over dead.  YOU HAVE SAVED'), nl,
		write('THE DAY!  You will be a hero in town forever.'), nl,
		retract(location(wumpus, X)),
		retract(have(arrow)),
		asserta(slain(wumpus)).
	fight(wumpus) :-
		in(X),
		location(wumpus, X),
		\+ have(bow),
		write('You walk up to the wumpus and notice that his scales are'), nl,
		write('extremely thick.  It would probably take a golden arrow to'), nl,
		write('get through those so you decide to poke him in the eye.  He'), nl,
		write('gets very annoyed.  Your last memories are of the wumpus'), nl,
		write('swinging you around and around over his head ... Oh, OUCH,'), nl,
		write('Argh ...  Later that day you wake up unsure of your surroundings.'), nl,
		retract(in(X)),
		asserta(in(sinkhole)).
	fight(_) :-
		write('How exactly do you propose I do that?'), nl,
		fail.

	% End of rules for the game.  The rest of the rules are for
	% simple natural language processing of the commands.

	% The following code was written by Amzi! inc. for reading a
	% whole line of text and converting it into a list.  See:
	% http://www.amzi.com/AdventureInProlog/index.htm

	read_list(L) :-
		read_line(CL),
		phrase(wordlist(L), CL), !.

	read_line(L) :-
		get_code(C),
		buildlist(C, L).

	buildlist(10, []) :- !.
	buildlist(13, []) :- !.
	buildlist(C, [C| X]) :-
		get_code(C2),
		buildlist(C2, X).

	wordlist([X| Y]) --> word(X), whitespace, wordlist(Y).
	wordlist([X]) --> whitespace, wordlist(X).
	wordlist([X]) --> word(X).
	wordlist([X]) --> word(X), whitespace.

	word(W) --> charlist(X), {atom_codes(W, X)}.

	charlist([X| Y]) --> chr(X), charlist(Y).
	charlist([X]) --> chr(X).

	chr(X) --> [X], {X >= 48}.

	whitespace --> whsp, whitespace.
	whitespace --> whsp.

	whsp --> [X], {X < 48}.
	%  end of the code from Amzi! inc.

	% The elim predicate takes out the words "to" and "the" from
	% a list.  We don't need them to process commands.
	elim(CL, [V,to,the,N]) :-
		CL = [V,N], !.
	elim(CL, [V,to,N]) :-
		CL = [V,N], !.
	elim(CL,[V,the,N]) :-
		CL = [V,N], !.
	elim(CL, Y) :-
		CL = Y.

	command(CL, L) :-
		elim(NL, L),
		CL =.. NL, !.

	get_command(C) :-
		read_list(L),
		command(C, L), !.
	get_command(_) :-
		write('I don''t understand'), nl, fail.

	main :-
		write('Welcome to Hunt the Wumpus'), nl,
		write('  To begin, type:  look'), nl,
		write('  For help, type:  help'), nl, nl,
		repeat,
			write('>command: '),
			get_command(X),
			do(X), nl,
		end_condition(X).

	end_condition(end).
	end_condition(_) :-
		slain(wumpus),
		write('Congratulations').

	do(help) :- (help), !.
	do(goto(X)) :- goto(X), !.
	do(go(X)) :- goto(X), !.
	do(inventory) :- inventory, !.
	do(look) :- look, !.
	do(take(X)) :- take(X), !.
	do(talk(X)) :- talk(X), !.
	do(fight(X)) :- fight(X), !.
	do(buy(X)) :- buy(X), !.
	do(sell(X)) :- sell(X), !.
	do(board(X)) :- board(X), !.
	do(light(X)) :- light(X), !.
	do(end).
	do(_) :-
		write('I don''t understand.'), nl, !.

	% Define the player's initial location when the game begins
	in(praire).

:- end_object.
