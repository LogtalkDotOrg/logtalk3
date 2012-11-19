
:- object(abolish_category1,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the abolish_category/1 built-in predicate.'
	]).

	throws(abolish_category_1, error(instantiation_error, logtalk(abolish_category(_), _))) :-
		abolish_category(_).

	throws(abolish_category_2, error(type_error(category_identifier, 1), logtalk(abolish_category(1), _))) :-
		abolish_category(1).

	throws(abolish_category_3, error(existence_error(category, non_exisiting_category), logtalk(abolish_category(non_exisiting_category), _))) :-
		abolish_category(non_exisiting_category).

:- end_object.
