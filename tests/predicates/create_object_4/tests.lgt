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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the create_object/4 built-in predicate.'
	]).

	throws(create_object_1_1, error(instantiation_error, logtalk(create_object(_, _, _, _), _))) :-
		create_object(_, _, _, _).

	throws(create_object_1_2, error(type_error(object_identifier, 1), logtalk(create_object(1, [], [], []), _))) :-
		create_object(1, [], [], []).

	throws(create_object_1_3, error(permission_error(modify, object, logtalk), logtalk(create_object(logtalk, [], [], []), _))) :-
		create_object(logtalk, [], [], []).

	throws(create_object_1_4, error(permission_error(modify, protocol, monitoring), logtalk(create_object(monitoring, [], [], []), _))) :-
		create_object(monitoring, [], [], []).

	throws(create_object_1_5, error(type_error(list, atom), logtalk(create_object(_, atom, [], []), _))) :-
		create_object(_, atom, [], []).

	throws(create_object_1_6, error(type_error(list, atom), logtalk(create_object(_, [], atom, []), _))) :-
		create_object(_, [], atom, []).

	throws(create_object_1_7, error(type_error(list, atom), logtalk(create_object(_, [], [], atom), _))) :-
		create_object(_, [], [], atom).

	succeeds(create_object_1_8) :-
		create_object(Object, [], [], []),
		(	atom(Object) ->
			true
		;	compound(Object)
		).

	succeeds(create_object_1_9) :-
		create_object(create_object4_test_object, [], [], []),
		abolish_object(create_object4_test_object).

	succeeds(create_object_1_10) :-
		create_object(Object, [], [], [foo(1), (bar(X) :- foo(X))]),
		abolish_object(Object).

:- end_object.
