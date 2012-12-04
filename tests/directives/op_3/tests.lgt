
:- object(op_3_test_object_1).

	:- set_logtalk_flag(context_switching_calls, allow).

	% scoped operarors; seen by the reflection built-in methods
	:- public(op(501, xfx, abc)).
	:- protected(op(501, xfx, def)).
	:- private(op(501, xfx, ghi)).

	% local operator; invisible to the reflection built-in methods
	:- op(501, xfx, jkl).

	% test operator overriding
	:- public(op(600, xfx, :)).

	% test for call in "self"
	:- public(operators/1).
	operators(Operators) :-
		setof(Operator, ::current_op(601, xfx, Operator), Operators).

:- end_object.



:- object(op_3_test_object_2,
	extends(op_3_test_object_1)).

	:- public(op(601, xfx, opq)).
	:- protected(op(601, xfx, rst)).
	:- private(op(601, xfx, uvw)).

	% local operator; invisible to the reflection built-in methods
	:- op(601, xfx, xyz).

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/04,
		comment is 'Unit tests for the op/3 built-in directive.'
	]).

	test(op_3_1) :-
		setof(Operator, op_3_test_object_1<<current_op(501, xfx, Operator), Operators),
		Operators == [abc, def, ghi].

	test(op_3_2) :-
		setof(Operator, op_3_test_object_1::current_op(501, xfx, Operator), Operators),
		Operators == [abc].

	test(op_3_3) :-
		op_3_test_object_1::current_op(600, xfx, :),
		\+ op_3_test_object_1::current_op(600, xfy, :).

	test(op_3_4) :-
		\+ op_3_test_object_2::current_op(501, xfx, _).

	test(op_3_5) :-
		op_3_test_object_2::operators(Operators),
		Operators == [opq, rst].

:- end_object.
