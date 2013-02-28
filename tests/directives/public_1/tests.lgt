
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/02/28,
		comment is 'Unit tests for the public/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- public(a/0).

	:- public([b/1, c/2]).

	test(public_1_1) :-
		predicate_property(a, (public)),
		predicate_property(a, static).

	test(public_1_2) :-
		predicate_property(b(_), (public)),
		predicate_property(b(_), static).

	test(public_1_3) :-
		predicate_property(c(_,_), (public)),
		predicate_property(c(_,_), static).

:- end_object.
