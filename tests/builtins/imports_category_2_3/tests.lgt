
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the imports_category/2-3 built-in predicates.'
	]).

	% imports_category/2 tests

	throws(imports_category_2_1, error(type_error(object_identifier, 1), logtalk(imports_category(1, _), _))) :-
		imports_category(1, _).

	throws(imports_category_2_2, error(type_error(category_identifier, 1), logtalk(imports_category(_, 1), _))) :-
		imports_category(_, 1).

	% imports_category/3 tests

	throws(imports_category_3_1, error(type_error(object_identifier, 1), logtalk(imports_category(1, _, _), _))) :-
		imports_category(1, _, _).

	throws(imports_category_3_2, error(type_error(category_identifier, 1), logtalk(imports_category(_, 1, _), _))) :-
		imports_category(_, 1, _).

	throws(imports_category_3_3, error(type_error(scope, 1), logtalk(imports_category(_, _, 1), _))) :-
		imports_category(_, _, 1).

:- end_object.
