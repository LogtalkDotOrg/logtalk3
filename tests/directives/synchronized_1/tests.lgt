
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/28,
		comment is 'Unit tests for the synchronized/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- private(a/0).
	:- synchronized(a/0).

	:- private([b/1, c/2]).
	:- synchronized([b/1, c/2]).

	:- private(d/3, e/4, f/5).
	:- synchronized(d/3, e/4, f/5).

	:- if(current_logtalk_flag(threads, supported)).

		test(synchronized_1_1) :-
			predicate_property(a, private),
			predicate_property(a, synchronized).

		test(synchronized_1_2) :-
			predicate_property(b(_), private),
			predicate_property(b(_), synchronized).

		test(synchronized_1_3) :-
			predicate_property(c(_,_), private),
			predicate_property(c(_,_), synchronized).

		test(synchronized_1_4) :-
			predicate_property(d(_,_,_), private),
			predicate_property(d(_,_,_), synchronized).

		test(synchronized_1_5) :-
			predicate_property(e(_,_,_,_), private),
			predicate_property(e(_,_,_,_), synchronized).

		test(synchronized_1_6) :-
			predicate_property(f(_,_,_,_,_), private),
			predicate_property(f(_,_,_,_,_), synchronized).

	:- else.

		% when threads are not supported, the synchronized/1 directive is ignored

		test(synchronized_1_1) :-
			predicate_property(a, private),
			\+ predicate_property(a, synchronized).

		test(synchronized_1_2) :-
			predicate_property(b(_), private),
			\+ predicate_property(b(_), synchronized).

		test(synchronized_1_3) :-
			predicate_property(c(_,_), private),
			\+ predicate_property(c(_,_), synchronized).

		test(synchronized_1_4) :-
			predicate_property(d(_,_,_), private),
			\+ predicate_property(d(_,_,_), synchronized).

		test(synchronized_1_5) :-
			predicate_property(e(_,_,_,_), private),
			\+ predicate_property(e(_,_,_,_), synchronized).

		test(synchronized_1_6) :-
			predicate_property(f(_,_,_,_,_), private),
			\+ predicate_property(f(_,_,_,_,_), synchronized).

	:- endif.

:- end_object.
