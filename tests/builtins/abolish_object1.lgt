
:- object(abolish_object1,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the abolish_object/1 built-in predicate.'
	]).

	throws(abolish_object_1, error(instantiation_error, logtalk(abolish_object(_), _))) :-
		abolish_object(_).

	throws(abolish_object_2, error(type_error(object_identifier, 1), logtalk(abolish_object(1), _))) :-
		abolish_object(1).

	throws(abolish_object_3, error(existence_error(object, non_exisiting_object), logtalk(abolish_object(non_exisiting_object), _))) :-
		abolish_object(non_exisiting_object).

	throws(abolish_object_4, error(permission_error(modify, static_object, logtalk), logtalk(abolish_object(logtalk), _))) :-
		abolish_object(logtalk).

:- end_object.
