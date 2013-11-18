
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/11/18,
		comment is 'Unit tests for the alias/3 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- alias(lgtunit, run/0, run_alias/0).

	test(alias_3_1) :-
		predicate_property(run_alias, logtalk),
		predicate_property(run_alias, (public)),
		predicate_property(run_alias, scope(public)),
		predicate_property(run_alias, static),
		predicate_property(run_alias, alias_of(run)),
		predicate_property(run_alias, declared_in(lgtunit)),
		predicate_property(run_alias, defined_in(lgtunit)).

:- end_object.
