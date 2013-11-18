
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/11/18,
		comment is 'Unit tests for the coinductive/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- public(a/1).
	:- coinductive(a/1).

	test(coinductive_1_1) :-
		predicate_property(a(_), logtalk),
		predicate_property(a(_), (public)),
		predicate_property(a(_), scope(public)),
		predicate_property(a(_), static),
		predicate_property(a(_), coinductive(Template)),
		Template == a(+).

	:- public(b/2).
	:- coinductive(b(+, -)).

	test(coinductive_1_2) :-
		predicate_property(b(_, _), logtalk),
		predicate_property(b(_, _), (public)),
		predicate_property(b(_, _), scope(public)),
		predicate_property(b(_, _), static),
		predicate_property(b(_, _), coinductive(Template)),
		Template == b(+, -).

:- end_object.
