%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "threads/birthdays" example.']).

	:- set_logtalk_flag(unknown_entities, silent).

	test(birthdays_1) :-
		agent::(new(paul, 40, male), new(nathalie, 32, female)).

	test(birthdays_2) :-
		paul::new_friend(nathalie).

	test(birthdays_3) :-
		set_logtalk_flag(events, allow),
		{nathalie::birthday}.

	test(birthdays_4) :-
		nathalie::age(Age),
		Age == 33.

	test(birthdays_5) :-
		{nathalie::birthday}.

	cleanup :-
		set_logtalk_flag(events, deny).

:- end_object.
