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
		comment is 'Unit tests for the create_object/4 built-in predicate.'
	]).

	throws(create_object_1_01, error(instantiation_error, logtalk(create_object(_, _, _, _), _))) :-
		create_object(_, _, _, _).

	throws(create_object_1_02, error(type_error(object_identifier, 1), logtalk(create_object(1, [], [], []), _))) :-
		create_object(1, [], [], []).

	throws(create_object_1_03, error(permission_error(modify, object, logtalk), logtalk(create_object(logtalk, [], [], []), _))) :-
		create_object(logtalk, [], [], []).

	throws(create_object_1_04, error(permission_error(modify, protocol, monitoring), logtalk(create_object(monitoring, [], [], []), _))) :-
		create_object(monitoring, [], [], []).

	throws(create_object_1_05, error(type_error(list, atom), logtalk(create_object(_, atom, [], []), _))) :-
		create_object(_, atom, [], []).

	throws(create_object_1_06, error(type_error(list, atom), logtalk(create_object(_, [], atom, []), _))) :-
		create_object(_, [], atom, []).

	throws(create_object_1_07, error(type_error(list, atom), logtalk(create_object(_, [], [], atom), _))) :-
		create_object(_, [], [], atom).

	throws(create_object_1_08, error(_, logtalk(create_object(_, [], [dynamic(foo/1), synchronized(foo/1)], [foo(1)]), _))) :-
		create_object(_, [], [dynamic(foo/1), synchronized(foo/1)], [foo(1)]).

	throws(create_object_1_09, error(_, logtalk(create_object(_, [], [synchronized(foo/1), dynamic(foo/1)], [foo(1)]), _))) :-
		create_object(_, [], [synchronized(foo/1), dynamic(foo/1)], [foo(1)]).

	throws(create_object_1_10, error(_, logtalk(create_object(_, [], [public(map/2), meta_predicate(map(1,*))], [(map(Cl,El) :- call(Cl,El,_))]), _))) :-
		create_object(_, [], [public(map/2), meta_predicate(map(1,*))], [(map(Cl,El) :- call(Cl,El,_))]).

	succeeds(create_object_1_11) :-
		create_object(Object, [], [], []),
		(	atom(Object) ->
			true
		;	compound(Object)
		).

	succeeds(create_object_1_12) :-
		create_object(create_object4_test_object, [], [], []),
		abolish_object(create_object4_test_object).

	succeeds(create_object_1_13) :-
		create_object(Object, [], [], [foo(1), (bar(X) :- foo(X))]),
		abolish_object(Object).

	succeeds(create_object_1_14) :-
		create_object(Parent, [], [public([p/1, q/1])], [p(0), q(0)]),
		create_object(Descendant, [extends(Parent)], [], [p(1), (p(X) :- ^^p(X)), q(1), (q(X) :- ::p(X))]),
		abolish_object(Descendant),
		abolish_object(Parent).

	succeeds(create_object_1_15) :-
		create_object(Object, [], [op(567, xfx, foo)], []),
		{\+ current_op(567, xfx, foo)},
		abolish_object(Object).

:- end_object.
