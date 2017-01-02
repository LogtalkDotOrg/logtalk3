%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2016/11/11,
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

	throws(create_object_1_08, error(permission_error(modify, dynamic_predicate, foo/1), logtalk(create_object(_, [], [dynamic(foo/1), synchronized(foo/1)], [foo(1)]), _))) :-
		create_object(_, [], [dynamic(foo/1), synchronized(foo/1)], [foo(1)]).

	throws(create_object_1_09, error(permission_error(modify, synchronized_predicate, foo/1), logtalk(create_object(_, [], [synchronized(foo/1), dynamic(foo/1)], [foo(1)]), _))) :-
		create_object(_, [], [synchronized(foo/1), dynamic(foo/1)], [foo(1)]).

	throws(create_object_1_10, error(domain_error({1}, 2), logtalk(create_object(_, [], [public(map/2), meta_predicate(map(1,*))], [(map(Cl,El) :- call(Cl,El,_))]), _))) :-
		create_object(_, [], [public(map/2), meta_predicate(map(1,*))], [(map(Cl,El) :- call(Cl,El,_))]).

	throws(create_object_1_11, error(permission_error(repeat, entity_relation, implements/1), logtalk(create_object(_, [implements(protocol1), implements(protocol2)], [], []), _))) :-
		create_object(_, [implements(protocol1), implements(protocol2)], [], []).

	throws(create_object_1_12, error(permission_error(repeat, entity_relation, imports/1), logtalk(create_object(_, [imports(category1), imports(category2)], [], []), _))) :-
		create_object(_, [imports(category1), imports(category2)], [], []).

	throws(create_object_1_13, error(permission_error(repeat, entity_relation, extends/1), logtalk(create_object(_, [extends(object1), extends(object2)], [], []), _))) :-
		create_object(_, [extends(object1), extends(object2)], [], []).

	throws(create_object_1_14, error(permission_error(repeat, entity_relation, instantiates/1), logtalk(create_object(_, [instantiates(class1), instantiates(class2)], [], []), _))) :-
		create_object(_, [instantiates(class1), instantiates(class2)], [], []).

	throws(create_object_1_15, error(permission_error(repeat, entity_relation, specializes/1), logtalk(create_object(_, [specializes(class1), specializes(class2)], [], []), _))) :-
		create_object(_, [specializes(class1), specializes(class2)], [], []).

	succeeds(create_object_1_16) :-
		create_object(Object, [], [], []),
		(	atom(Object) ->
			true
		;	compound(Object)
		).

	succeeds(create_object_1_17) :-
		create_object(create_object4_test_object, [], [], []),
		abolish_object(create_object4_test_object).

	succeeds(create_object_1_18) :-
		create_object(Object, [], [], [foo(1), (bar(X) :- foo(X))]),
		abolish_object(Object).

	succeeds(create_object_1_19) :-
		create_object(Parent, [], [public([p/1, q/1])], [p(0), q(0)]),
		create_object(Descendant, [extends(Parent)], [], [p(1), (p(X) :- ^^p(X)), q(1), (q(X) :- ::p(X))]),
		abolish_object(Descendant),
		abolish_object(Parent).

	succeeds(create_object_1_20) :-
		create_object(Object, [], [op(567, xfx, foo)], []),
		{\+ current_op(567, xfx, foo)},
		abolish_object(Object).

:- end_object.
