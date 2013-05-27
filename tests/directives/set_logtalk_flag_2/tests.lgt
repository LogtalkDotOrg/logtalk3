
% source file level set_logtalk_flag/2 directives are local to the file
:- set_logtalk_flag(complements, deny).
:- set_logtalk_flag(context_switching_calls, allow).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/05/27,
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

:- end_object.
