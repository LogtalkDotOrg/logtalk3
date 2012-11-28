
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/28,
		comment is 'Unit tests for the public/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- public(a/0).

	:- public([b/1, c/2]).

	:- public(d/3, e/4, f/5).

	test(public_1_1) :-
		predicate_property(a, (public)),
		predicate_property(a, static).

	test(public_1_2) :-
		predicate_property(b(_), (public)),
		predicate_property(b(_), static).

	test(public_1_3) :-
		predicate_property(c(_,_), (public)),
		predicate_property(c(_,_), static).

	test(public_1_4) :-
		predicate_property(d(_,_,_), (public)),
		predicate_property(d(_,_,_), static).

	test(public_1_5) :-
		predicate_property(e(_,_,_,_), (public)),
		predicate_property(e(_,_,_,_), static).

	test(public_1_6) :-
		predicate_property(f(_,_,_,_,_), (public)),
		predicate_property(f(_,_,_,_,_), static).

:- end_object.
