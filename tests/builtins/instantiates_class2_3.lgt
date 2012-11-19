
:- object(instantiates_class2_3,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the instantiates_class/2-3 built-in predicate.'
	]).

	% instantiates_class/2 tests

	throws(instantiates_class_2_1, error(type_error(object_identifier, 1), logtalk(instantiates_class(1, _), _))) :-
		instantiates_class(1, _).

	throws(instantiates_class_2_2, error(type_error(object_identifier, 1), logtalk(instantiates_class(_, 1), _))) :-
		instantiates_class(_, 1).

	% instantiates_class/3 tests

	throws(instantiates_class_3_1, error(type_error(object_identifier, 1), logtalk(instantiates_class(1, _, _), _))) :-
		instantiates_class(1, _, _).

	throws(instantiates_class_3_2, error(type_error(object_identifier, 1), logtalk(instantiates_class(_, 1, _), _))) :-
		instantiates_class(_, 1, _).

	throws(instantiates_class_3_3, error(type_error(scope, 1), logtalk(instantiates_class(_, _, 1), _))) :-
		instantiates_class(_, _, 1).

:- end_object.
