%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% source file level set_logtalk_flag/2 directives are local to the file
:- set_logtalk_flag(complements, deny).
:- set_logtalk_flag(context_switching_calls, allow).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/10/07,
		comment is 'Unit tests for the set_logtalk_flag/2 built-in directive.'
	]).

	% entity level  set_logtalk_flag/2 directives are local to the entity
	:- set_logtalk_flag(complements, allow).
	:- set_logtalk_flag(context_switching_calls, deny).

	test(set_logtalk_flag_2_1) :-
		this(This),
		object_property(This, complements).

	test(set_logtalk_flag_2_2) :-
		this(This),
		\+ object_property(This, context_switching_calls).

	test(set_logtalk_flag_2_3) :-
		bottom::p(X),
		X == top.

	test(set_logtalk_flag_2_4) :-
		logtalk_load(patch),
		bottom::p(X),
		X == patch.

:- end_object.
