
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/02/28,
		comment is 'Unit tests for the private/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- private(a/0).

	:- private([b/1, c/2]).

	test(private_1_1) :-
		predicate_property(a, private),
		predicate_property(a, static).

	test(private_1_2) :-
		predicate_property(b(_), private),
		predicate_property(b(_), static).

	test(private_1_3) :-
		predicate_property(c(_,_), private),
		predicate_property(c(_,_), static).

:- end_object.
