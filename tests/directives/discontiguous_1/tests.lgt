
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/05/27,
		comment is 'Unit tests for the discontiguous/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- private(a/1).
	:- discontiguous(a/1).

	:- private([b/1, c/1]).
	:- discontiguous([b/1, c/1]).

	a(1).
	b(1).
	a(2).
	c(1).
	b(2).
	c(2).

	test(discontiguous_1_1) :-
		findall(X, a(X), L),
		L = [1, 2].

	test(discontiguous_1_2) :-
		findall(X, b(X), L),
		L = [1, 2].

	test(discontiguous_1_3) :-
		findall(X, c(X), L),
		L = [1, 2].

:- end_object.
