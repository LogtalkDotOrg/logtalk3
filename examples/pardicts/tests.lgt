%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/05/23,
		comment is 'Unit tests for the "pardicts" example.'
	]).

	test(dicts_1) :-
		obj(Dict)::init([a-1, b-2, c-3]),
		setof(Value, Key^(obj(Dict)::get(Key, Value)), Values),
		Values == [1, 2, 3].

	test(dicts_2) :-
		obj(Dict)::init([a-1, b-2, c-3]),
		obj(Dict)::nb_set(b, 9),
		obj(Dict)::get(b, Value),
		Value == 9.

:- end_object.
