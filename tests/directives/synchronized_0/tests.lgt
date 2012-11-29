
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/28,
		comment is 'Unit tests for the synchronized/0 built-in directive.'
	]).

	:- synchronized.

	:- if(current_logtalk_flag(threads, supported)).

		test(synchronized_0_1) :-
			this(This),
			object_property(This, synchronized).

			% all predicates in a synchronized entity are implicitly synchronized

		:- private(p/0).

		test(synchronized_0_2) :-
			predicate_property(p, private),
			predicate_property(p, synchronized).

	:- else.

		% when threads are not supported, the synchronized/0 directive is ignored

		test(synchronized_0_1) :-
			this(This),
			\+ object_property(This, synchronized).

		:- private(p/0).

		test(synchronized_0_2) :-
			predicate_property(p, private),
			\+ predicate_property(p, synchronized).

	:- endif.

:- end_object.
