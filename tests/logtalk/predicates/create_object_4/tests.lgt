%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.3,
		author is 'Paulo Moura',
		date is 2017/01/17,
		comment is 'Unit tests for the create_object/4 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	throws(create_object_4_01, error(instantiation_error, logtalk(create_object(_, _, _, _), _))) :-
		create_object(_, _, _, _).

	throws(create_object_4_02, error(type_error(object_identifier, 1), logtalk(create_object(1, [], [], []), _))) :-
		create_object(1, [], [], []).

	throws(create_object_4_03, error(permission_error(modify, object, logtalk), logtalk(create_object(logtalk, [], [], []), _))) :-
		create_object(logtalk, [], [], []).

	throws(create_object_4_04, error(permission_error(modify, protocol, monitoring), logtalk(create_object(monitoring, [], [], []), _))) :-
		create_object(monitoring, [], [], []).

	throws(create_object_4_05, error(type_error(list, atom), logtalk(create_object(_, atom, [], []), _))) :-
		create_object(_, atom, [], []).

	throws(create_object_4_06, error(type_error(list, atom), logtalk(create_object(_, [], atom, []), _))) :-
		create_object(_, [], atom, []).

	throws(create_object_4_07, error(type_error(list, atom), logtalk(create_object(_, [], [], atom), _))) :-
		create_object(_, [], [], atom).

	throws(create_object_4_08, error(permission_error(modify, dynamic_predicate, foo/1), logtalk(create_object(_, [], [dynamic(foo/1), synchronized(foo/1)], [foo(1)]), _))) :-
		create_object(_, [], [dynamic(foo/1), synchronized(foo/1)], [foo(1)]).

	throws(create_object_4_09, error(permission_error(modify, synchronized_predicate, foo/1), logtalk(create_object(_, [], [synchronized(foo/1), dynamic(foo/1)], [foo(1)]), _))) :-
		create_object(_, [], [synchronized(foo/1), dynamic(foo/1)], [foo(1)]).

	throws(create_object_4_10, error(domain_error({1}, 2), logtalk(create_object(_, [], [public(map/2), meta_predicate(map(1,*))], [(map(Cl,El) :- call(Cl,El,_))]), _))) :-
		create_object(_, [], [public(map/2), meta_predicate(map(1,*))], [(map(Cl,El) :- call(Cl,El,_))]).

	throws(create_object_4_11, error(permission_error(repeat, entity_relation, implements/1), logtalk(create_object(_, [implements(protocol1), implements(protocol2)], [], []), _))) :-
		create_object(_, [implements(protocol1), implements(protocol2)], [], []).

	throws(create_object_4_12, error(permission_error(repeat, entity_relation, imports/1), logtalk(create_object(_, [imports(category1), imports(category2)], [], []), _))) :-
		create_object(_, [imports(category1), imports(category2)], [], []).

	throws(create_object_4_13, error(permission_error(repeat, entity_relation, extends/1), logtalk(create_object(_, [extends(object1), extends(object2)], [], []), _))) :-
		create_object(_, [extends(object1), extends(object2)], [], []).

	throws(create_object_4_14, error(permission_error(repeat, entity_relation, instantiates/1), logtalk(create_object(_, [instantiates(class1), instantiates(class2)], [], []), _))) :-
		create_object(_, [instantiates(class1), instantiates(class2)], [], []).

	throws(create_object_4_15, error(permission_error(repeat, entity_relation, specializes/1), logtalk(create_object(_, [specializes(class1), specializes(class2)], [], []), _))) :-
		create_object(_, [specializes(class1), specializes(class2)], [], []).

	succeeds(create_object_4_16) :-
		create_object(Object, [], [], []),
		(	atom(Object) ->
			true
		;	compound(Object)
		).

	succeeds(create_object_4_17) :-
		create_object(create_object4_test_object, [], [], []),
		abolish_object(create_object4_test_object).

	succeeds(create_object_4_18) :-
		create_object(Object, [], [], [foo(1), (bar(X) :- foo(X))]),
		abolish_object(Object).

	succeeds(create_object_4_19) :-
		create_object(Parent, [], [public([p/1, q/1])], [p(0), q(0)]),
		create_object(Descendant, [extends(Parent)], [], [p(1), (p(X) :- ^^p(X)), q(1), (q(X) :- ::p(X))]),
		abolish_object(Descendant),
		abolish_object(Parent).

	succeeds(create_object_4_20) :-
		create_object(Object, [], [op(567, xfx, foo)], []),
		{\+ current_op(567, xfx, foo)},
		abolish_object(Object).

	throws(create_object_4_21, error(permission_error(implement, self, _), logtalk(create_object(Object, [implements(Object)], [], []), _))) :-
		create_object(Object, [implements(Object)], [], []).

	throws(create_object_4_22, error(permission_error(import, self, _), logtalk(create_object(Object, [imports(Object)], [], []), _))) :-
		create_object(Object, [imports(Object)], [], []).

	throws(create_object_4_23, error(permission_error(extend, self, _), logtalk(create_object(Object, [extends(Object)], [], []), _))) :-
		create_object(Object, [extends(Object)], [], []).

	throws(create_object_4_24, error(permission_error(specialize, self, _), logtalk(create_object(Object, [specializes(Object)], [], []), _))) :-
		create_object(Object, [specializes(Object)], [], []).

	throws(create_object_4_25, error(type_error(protocol, Object1), logtalk(create_object(Object2, [implements(Object1)], [], []), _))) :-
		create_object(Object1, [], [], []),
		create_object(Object2, [implements(Object1)], [], []).

	throws(create_object_4_26, error(type_error(category, Object1), logtalk(create_object(Object2, [imports(Object1)], [], []), _))) :-
		create_object(Object1, [], [], []),
		create_object(Object2, [imports(Object1)], [], []).

	throws(create_object_4_27, error(domain_error(class, Object1), logtalk(create_object(Object2, [instantiates(Object1)], [], []), _))) :-
		create_object(Object1, [], [], []),
		create_object(Object2, [instantiates(Object1)], [], []).

	throws(create_object_4_28, error(domain_error(class, Object1), logtalk(create_object(Object2, [specializes(Object1)], [], []), _))) :-
		create_object(Object1, [], [], []),
		create_object(Object2, [specializes(Object1)], [], []).

	throws(create_object_4_29, error(permission_error(extend, prototype, Prototype), logtalk(create_object(Object, [instantiates(Class), extends(Prototype)], [], []), _))) :-
		create_object(Prototype, [], [], []),
		create_object(Class, [instantiates(Class)], [], []),
		create_object(Object, [instantiates(Class), extends(Prototype)], [], []).

	throws(create_object_4_30, error(permission_error(instantiate, class, Class), logtalk(create_object(Object, [extends(Prototype), instantiates(Class)], [], []), _))) :-
		create_object(Prototype, [], [], []),
		create_object(Class, [instantiates(Class)], [], []),
		create_object(Object, [extends(Prototype), instantiates(Class)], [], []).

	throws(create_object_4_31, error(permission_error(specialize, class, Class), logtalk(create_object(Object, [extends(Prototype), specializes(Class)], [], []), _))) :-
		create_object(Prototype, [], [], []),
		create_object(Class, [instantiates(Class)], [], []),
		create_object(Object, [extends(Prototype), specializes(Class)], [], []).

:- end_object.
