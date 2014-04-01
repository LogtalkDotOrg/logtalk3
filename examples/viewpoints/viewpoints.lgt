%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% general information about Joe:

:- object(joe_person).

	:- public([
		grow_older/0, address/1, age/1, name/1,
		phone/1, score/1, set_score/1
	]).

	:- dynamic([
		age/1, score/1
	]).

	grow_older :-					% this predicate uses property sharing, i.e.
		retract(age(Old)),			% the property and its value are shared by all 
		New is Old + 1,				% descendant prototypes/viewpoints; changes
		asserta(age(New)).			% are shared no matter which viewpoint receives
									% the grow_older/1 message
	address('8 Octave Street').

	age(30).

	name('John').

	phone(11-11-11-11).

	score(0).						% default value for the score/1 property,
									% shared by all descendant prototypes/viewpoints;
	set_score(Score) :-				% changing the default value results in
		::retractall(score(_)),		% in a local value stored in the descendant
		::asserta(score(Score)).	% prototype that received the set_score/1 message

:- end_object.


% information on Joe as an employee:

:- object(joe_employee,
	extends(joe_person)).

	:- public([
		works_for/1, salary/1, give_raise/1
	]).

	:- dynamic(salary/1).

	works_for('ToonTown').

	salary(1500).

	give_raise(Raise) :-			% another example of property sharing
		retract(salary(Old)),
		New is Old + Raise,
		asserta(salary(New)).

:- end_object.


% information on Joe as an chess player:

:- object(joe_chess_player,
	extends(joe_person)).

	:- public(category/1).

	category('National Master').

:- end_object.


% information on Joe as a movies fan:

:- object(joe_film_enthusiast,
	extends(joe_person)).

	:- public([
		favorite_actor/1, favorite_film/1, favorite_director/1
	]).

	favorite_actor('Fred Filistone').

	favorite_film('The Wizard of Oz').

	favorite_director('Krzystof Kieslowski').

:- end_object.


% information on Joe as a sportsman:

:- object(joe_sportsman,
	extends(joe_person)).

	:- public([
		sport/1, stamina/1, weight/1
	]).

	sport(snowboard).

	stamina(30).

	weight(111).

:- end_object.
