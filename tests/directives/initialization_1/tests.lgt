
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/05/28,
		comment is 'Unit tests for the initialization/1 built-in directive.'
	]).

	:- set_logtalk_flag(context_switching_calls, allow).
	:- set_logtalk_flag(dynamic_declarations, allow).
	:- set_logtalk_flag(missing_directives, silent).

	:- initialization(assertz(foo)).

	test(initialization_1_1) :-
		current_predicate(foo/0),
		predicate_property(foo, private),
		predicate_property(foo, (dynamic)).

	test(initialization_1_2) :-
		this(This),
		This<<foo.

:- end_object.
