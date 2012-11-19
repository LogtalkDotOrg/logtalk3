
:- object(specializes_class2_3,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the specializes_class/2-3 built-in predicate.'
	]).

	% specializes_class/2 tests

	throws(specializes_class_2_1, error(type_error(object_identifier, 1), logtalk(specializes_class(1, _), _))) :-
		specializes_class(1, _).

	throws(specializes_class_2_2, error(type_error(object_identifier, 1), logtalk(specializes_class(_, 1), _))) :-
		specializes_class(_, 1).

	% specializes_class/3 tests

	throws(specializes_class_3_1, error(type_error(object_identifier, 1), logtalk(specializes_class(1, _, _), _))) :-
		specializes_class(1, _, _).

	throws(specializes_class_3_2, error(type_error(object_identifier, 1), logtalk(specializes_class(_, 1, _), _))) :-
		specializes_class(_, 1, _).

	throws(specializes_class_3_3, error(type_error(scope, 1), logtalk(specializes_class(_, _, 1), _))) :-
		specializes_class(_, _, 1).

:- end_object.
