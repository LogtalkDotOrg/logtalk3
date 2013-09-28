
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the create_category/4 built-in predicate.'
	]).

	throws(create_category_1_1, error(instantiation_error, logtalk(create_category(_, _, _, _), _))) :-
		create_category(_, _, _, _).

	throws(create_category_1_2, error(type_error(category_identifier, 1), logtalk(create_category(1, [], [], []), _))) :-
		create_category(1, [], [], []).

	throws(create_category_1_3, error(permission_error(modify, protocol, monitoring), logtalk(create_category(monitoring, [], [], []), _))) :-
		create_category(monitoring, [], [], []).

	throws(create_category_1_4, error(type_error(list, atom), logtalk(create_category(_, atom, [], []), _))) :-
		create_category(_, atom, [], []).

	throws(create_category_1_5, error(type_error(list, atom), logtalk(create_category(_, [], atom, []), _))) :-
		create_category(_, [], atom, []).

	throws(create_category_1_6, error(type_error(list, atom), logtalk(create_category(_, [], [], atom), _))) :-
		create_category(_, [], [], atom).

:- end_object.
