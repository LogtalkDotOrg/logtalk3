
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the extends_object/2-3 built-in predicate.'
	]).

	% extends_object/2 tests

	throws(extends_object_2_1, error(type_error(object_identifier, 1), logtalk(extends_object(1, _), _))) :-
		extends_object(1, _).

	throws(extends_object_2_2, error(type_error(object_identifier, 1), logtalk(extends_object(_, 1), _))) :-
		extends_object(_, 1).

	% extends_object/3 tests

	throws(extends_object_3_1, error(type_error(object_identifier, 1), logtalk(extends_object(1, _, _), _))) :-
		extends_object(1, _, _).

	throws(extends_object_3_2, error(type_error(object_identifier, 1), logtalk(extends_object(_, 1, _), _))) :-
		extends_object(_, 1, _).

	throws(extends_object_3_3, error(type_error(scope, 1), logtalk(extends_object(_, _, 1), _))) :-
		extends_object(_, _, 1).

:- end_object.
