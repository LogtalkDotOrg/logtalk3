
:- object(op_3_test_object_1).

	:- set_logtalk_flag(context_switching_calls, allow).

	% scoped operarors; seen by the reflection built-in methods
	:- public(op(501, xfx, abc)).
	:- protected(op(501, xfx, def)).
	:- private(op(501, xfx, ghi)).

	% local operator; invisible to the reflection built-in methods
	:- op(501, xfx, jkl).

	% also test operator overriding
	:- public(op(600, xfx, :)).

:- end_object.



:- object(op_3_test_object_2,
	extends(op_3_test_object_1)).

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/04,
		comment is 'Unit tests for the op/3 built-in directive.'
	]).

	succeeds(op_3_1) :-
		setof(Operator, op_3_test_object_1<<current_op(501, xfx, Operator), Operators),
		Operators == [abc, def, ghi].

	succeeds(op_3_2) :-
		setof(Operator, op_3_test_object_1::current_op(501, xfx, Operator), Operators),
		Operators == [abc].

	fails(op_3_3) :-
		op_3_test_object_2::current_op(501, xfx, _).

	succeeds(op_3_4) :-
		op_3_test_object_1::current_op(600, xfx, :),
		\+ op_3_test_object_1::current_op(600, xfy, :).

:- end_object.
