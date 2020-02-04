%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- object(expert,
	imports(protected::descriptors)).

	:- info([
		author is 'Paulo Moura',
		version is 1:2:0,
		date is 2018-09-24,
		comment is 'Expert system for bird identification.',
		source is 'Example adapted from an Amzi! Inc Prolog book.'
	]).

	:- public(identify/0).
	:- mode(identify, one).
	:- info(identify/0, [
		comment is 'Starts a bird identification session.'
	]).

	:- private(known_/3).
	:- dynamic(known_/3).
	:- mode(known_(?nonvar, ?nonvar, ?nonvar), zero_or_more).
	:- info(known_/3, [
		comment is 'Table of already known facts.',
		argnames is ['Answer', 'Attribute', 'Value']
	]).

	identify :-
		retractall(known_(_, _, _)),
		write('Bird identification expert system'), nl, nl,
		forall(
			(order::leaf(Bird), check(Bird)),
			(nl, write('Possible identification: '), write(Bird), nl)
		),
		nl, write('No (more) candidates found.').

	check(Bird) :-
		forall(
			(::descriptor(Name/Arity), functor(Predicate, Name, Arity), Bird::Predicate),
			call(Predicate)
		).

	bill(X) :-
		ask(bill, X).

	cheek(X) :-
		ask(cheek, X).

	color(X) :-
		ask(color, X).

	eats(X) :-
		ask(eats, X).

	feed(X) :-
		ask(feed,X).

	feet(X) :-
		ask(feet, X).

	flight(X) :-
		menuask(flight, X, [ponderous, powerful, agile, flap_glide, other]).

	flight_profile(X) :-
		menuask(flight_profile, X, [flat, v_shaped, other]).

	head(X) :-
		ask(head,X).

	live(X) :-
		ask(live, X).

	neck(X) :-
		ask(neck, X).

	nostrils(X) :-
		ask(nostrils, X).

	size(X) :-
		menuask(size, X, [large, plump, medium, small]).

	tail(X) :-
		menuask(tail, X, [narrow_at_tip, forked, long_rusty, square, other]).

	throat(X) :-
		ask(throat, X).

	voice(X) :-
		ask(voice,X).

	wings(X) :-
		ask(wings, X).

	ask(Attribute, Value) :-
		known_(yes, Attribute, Value),
		!.
	ask(Attribute, Value) :-
		known_(_, Attribute, Value),
		!, fail.
	ask(Attribute, _) :-
		known_(yes, Attribute, _),
		!, fail.
	ask(Attribute, Value) :-
		write(Attribute), write(': '), write(Value), write('? (yes or no): '),
		flush_output,
		read(Answer),
		asserta(known_(Answer, Attribute, Value)),
		Answer = yes.

	menuask(Attribute,Value, _) :-
		known_(yes, Attribute, Value),
		!.
	menuask(Attribute, _, _) :-
		known_(yes, Attribute, _),
		!, fail.
	menuask(Attribute, AskValue, Menu) :-
		nl, write('What is the value for '), write(Attribute), write('?'), nl,
		display_menu(Menu),
		write('Enter the number of choice> '),
		flush_output,
		read(Num), nl,
		pick_menu(Num, AnswerValue, Menu),
		asserta(known_(yes,Attribute,AnswerValue)),
		AskValue = AnswerValue.

	display_menu(Menu) :-
		display_menu(Menu, 1).

	display_menu([], _).
	display_menu([Item| Rest], N) :-
		write(N), write(' : '), write(Item), nl,
		NN is N + 1,
		display_menu(Rest, NN).

	pick_menu(N, Val, Menu) :-
		integer(N),
		pic_menu(1, N, Val, Menu),
		!.
	pick_menu(Val, Val, _).


	pic_menu(_, _, none_of_the_above, []).
	pic_menu(N, N, Item, [Item| _]).
	pic_menu(Ctr, N, Val, [_| Rest]) :-
		NextCtr is Ctr + 1,
		pic_menu(NextCtr, N, Val, Rest).

:- end_object.
