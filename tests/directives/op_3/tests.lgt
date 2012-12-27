
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/26,
		comment is 'Unit tests for the op/3 built-in directive.'
	]).

	:- private(op(501, xfx, foo)).
	:- private(op(601, xfy, [bar, baz])).

	test(op_3_1) :-
		current_op(Priority, Specifier, foo),
		Priority == 501,
		Specifier == xfx.

	test(op_3_2) :-
		current_op(Priority, Specifier, bar),
		Priority == 601,
		Specifier == xfy.

	test(op_3_3) :-
		current_op(Priority, Specifier, baz),
		Priority == 601,
		Specifier == xfy.

:- end_object.
