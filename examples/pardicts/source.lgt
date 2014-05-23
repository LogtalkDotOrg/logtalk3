%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_logtalk_flag(hook, pardicts_hook).


:- object(obj(_)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/05/23,
		comment is 'Simple object for testing goal-expansion of access to a dictionary parameter.']).

	:- public(init/1).
	init(Pairs) :-
		parameter_create(Pairs).

	:- public(get/2).
	get(Key, Value) :-
		get_parameter(Key, Value).

	:- public(b_set/2).
	b_set(Key, Value) :-
		b_set_parameter(Key, Value).

	:- public(nb_set/2).
	nb_set(Key, Value) :-
		nb_set_parameter(Key, Value).

:- end_object.
