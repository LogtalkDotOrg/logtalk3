%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2014/11/14,
		comment is 'Unit tests for the create_category/4 built-in predicate.'
	]).

	throws(create_category_1_01, error(instantiation_error, logtalk(create_category(_, _, _, _), _))) :-
		create_category(_, _, _, _).

	throws(create_category_1_02, error(type_error(category_identifier, 1), logtalk(create_category(1, [], [], []), _))) :-
		create_category(1, [], [], []).

	throws(create_category_1_03, error(permission_error(modify, protocol, monitoring), logtalk(create_category(monitoring, [], [], []), _))) :-
		create_category(monitoring, [], [], []).

	throws(create_category_1_04, error(type_error(list, atom), logtalk(create_category(_, atom, [], []), _))) :-
		create_category(_, atom, [], []).

	throws(create_category_1_05, error(type_error(list, atom), logtalk(create_category(_, [], atom, []), _))) :-
		create_category(_, [], atom, []).

	throws(create_category_1_06, error(type_error(list, atom), logtalk(create_category(_, [], [], atom), _))) :-
		create_category(_, [], [], atom).

	throws(create_category_1_07, error(_, logtalk(create_category(_, [], [dynamic(foo/1), synchronized(foo/1)], []), _))) :-
		create_category(_, [], [dynamic(foo/1), synchronized(foo/1)], []).

	throws(create_category_1_08, error(_, logtalk(create_category(_, [], [synchronized(foo/1), dynamic(foo/1)], []), _))) :-
		create_category(_, [], [synchronized(foo/1), dynamic(foo/1)], []).

	throws(create_category_1_09, error(_, logtalk(create_category(_, [], [public(map/2), meta_predicate(map(1,*))], [(map(Cl,El) :- call(Cl,El,_))]), _))) :-
		create_category(_, [], [public(map/2), meta_predicate(map(1,*))], [(map(Cl,El) :- call(Cl,El,_))]).

	succeeds(create_category_1_10) :-
		create_object(Category, [], [], []),
		atom(Category).

	succeeds(create_category_1_11) :-
		create_category(create_category_4_test_category, [], [], []),
		abolish_category(create_category_4_test_category).

	succeeds(create_category_1_12) :-
		create_category(Category, [], [], [foo(1), (bar(X) :- foo(X))]),
		abolish_category(Category).

	succeeds(create_category_1_13) :-
		create_category(Parent, [], [public([p/1, q/1])], [p(0), q(0)]),
		create_category(Descendant, [extends(Parent)], [], [p(1), (p(X) :- ^^p(X)), q(1), (q(X) :- ::p(X))]),
		abolish_category(Descendant),
		abolish_category(Parent).

	succeeds(create_category_1_14) :-
		create_category(Category, [], [op(567, xfx, foo)], []),
		{\+ current_op(567, xfx, foo)},
		abolish_category(Category).

:- end_object.
