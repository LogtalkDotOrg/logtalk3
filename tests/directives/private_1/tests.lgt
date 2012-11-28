
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/28,
		comment is 'Unit tests for the private/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- private(a/0).

	:- private([b/1, c/2]).

	:- private(d/3, e/4, f/5).

	test(private_1_1) :-
		predicate_property(a, private),
		predicate_property(a, static).

	test(private_1_2) :-
		predicate_property(b(_), private),
		predicate_property(b(_), static).

	test(private_1_3) :-
		predicate_property(c(_,_), private),
		predicate_property(c(_,_), static).

	test(private_1_4) :-
		predicate_property(d(_,_,_), private),
		predicate_property(d(_,_,_), static).

	test(private_1_5) :-
		predicate_property(e(_,_,_,_), private),
		predicate_property(e(_,_,_,_), static).

	test(private_1_6) :-
		predicate_property(f(_,_,_,_,_), private),
		predicate_property(f(_,_,_,_,_), static).

:- end_object.
