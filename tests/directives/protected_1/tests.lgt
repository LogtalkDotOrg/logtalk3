
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/28,
		comment is 'Unit tests for the protected/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- protected(a/0).

	:- protected([b/1, c/2]).

	:- protected(d/3, e/4, f/5).

	test(protected_1_1) :-
		predicate_property(a, protected),
		predicate_property(a, static).

	test(protected_1_2) :-
		predicate_property(b(_), protected),
		predicate_property(b(_), static).

	test(protected_1_3) :-
		predicate_property(c(_,_), protected),
		predicate_property(c(_,_), static).

	test(protected_1_4) :-
		predicate_property(d(_,_,_), protected),
		predicate_property(d(_,_,_), static).

	test(protected_1_5) :-
		predicate_property(e(_,_,_,_), protected),
		predicate_property(e(_,_,_,_), static).

	test(protected_1_6) :-
		predicate_property(f(_,_,_,_,_), protected),
		predicate_property(f(_,_,_,_,_), static).

:- end_object.
