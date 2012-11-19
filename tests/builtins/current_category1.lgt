
:- object(current_category1,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the current_category/1 built-in predicate.'
	]).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

	throws(current_category_1_1, error(type_error(category_identifier, 1), logtalk(current_category(1), _))) :-
		current_category(1).

	fails(current_category_2_2) :-
		current_category(non_exisiting_category).

:- end_object.
