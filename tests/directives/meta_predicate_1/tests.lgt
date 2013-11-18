
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/11/18,
		comment is 'Unit tests for the meta_predicate/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- public(foo/1).
	:- meta_predicate(foo(0)).

	test(meta_predicate_1_1) :-
		predicate_property(foo(_), logtalk),
		predicate_property(foo(_), (public)),
		predicate_property(foo(_), scope(public)),
		predicate_property(foo(_), static),
		predicate_property(foo(_), meta_predicate(Template)),
		Template == foo(0).

	:- public(bar/2).
	:- meta_predicate(bar(^, *)).

	test(meta_predicate_1_2) :-
		predicate_property(bar(_,_), logtalk),
		predicate_property(bar(_,_), (public)),
		predicate_property(bar(_,_), scope(public)),
		predicate_property(bar(_,_), static),
		predicate_property(bar(_,_), meta_predicate(Template)),
		Template == bar(^, *).

	:- public(baz/3).
	:- meta_predicate(baz(2, *, *)).

	test(meta_predicate_1_3) :-
		predicate_property(baz(_,_,_), logtalk),
		predicate_property(baz(_,_,_), (public)),
		predicate_property(baz(_,_,_), scope(public)),
		predicate_property(baz(_,_,_), static),
		predicate_property(baz(_,_,_), meta_predicate(Template)),
		Template == baz(2, *, *).

	:- public(qux/2).
	:- meta_predicate(qux(::, *)).

	test(meta_predicate_1_4) :-
		predicate_property(qux(_,_), logtalk),
		predicate_property(qux(_,_), (public)),
		predicate_property(qux(_,_), scope(public)),
		predicate_property(qux(_,_), static),
		predicate_property(qux(_,_), meta_predicate(Template)),
		Template == qux(::, *).

:- end_object.
