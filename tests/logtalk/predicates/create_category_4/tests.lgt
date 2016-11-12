%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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

	throws(create_category_1_07, error(permission_error(modify, dynamic_predicate, foo/1), logtalk(create_category(_, [], [dynamic(foo/1), synchronized(foo/1)], []), _))) :-
		create_category(_, [], [dynamic(foo/1), synchronized(foo/1)], []).

	throws(create_category_1_08, error(permission_error(modify, synchronized_predicate, foo/1), logtalk(create_category(_, [], [synchronized(foo/1), dynamic(foo/1)], []), _))) :-
		create_category(_, [], [synchronized(foo/1), dynamic(foo/1)], []).

	throws(create_category_1_09, error(domain_error({1}, 2), logtalk(create_category(_, [], [public(map/2), meta_predicate(map(1,*))], [(map(Cl,El) :- call(Cl,El,_))]), _))) :-
		create_category(_, [], [public(map/2), meta_predicate(map(1,*))], [(map(Cl,El) :- call(Cl,El,_))]).

	throws(create_category_1_10, error(permission_error(repeat, entity_relation, implements/1), logtalk(create_category(_, [implements(protocol1), implements(protocol2)], [], []), _))) :-
		create_category(_, [implements(protocol1), implements(protocol2)], [], []).

	throws(create_category_1_11, error(permission_error(repeat, entity_relation, extends/1), logtalk(create_category(_, [extends(category1), extends(category2)], [], []), _))) :-
		create_category(_, [extends(category1), extends(category2)], [], []).

	throws(create_category_1_12, error(permission_error(repeat, entity_relation, complements/1), logtalk(create_category(_, [complements(object1), complements(object2)], [], []), _))) :-
		create_category(_, [complements(object1), complements(object2)], [], []).

	succeeds(create_category_1_13) :-
		create_object(Category, [], [], []),
		atom(Category).

	succeeds(create_category_1_14) :-
		create_category(create_category_4_test_category, [], [], []),
		abolish_category(create_category_4_test_category).

	succeeds(create_category_1_15) :-
		create_category(Category, [], [], [foo(1), (bar(X) :- foo(X))]),
		abolish_category(Category).

	succeeds(create_category_1_16) :-
		create_category(Parent, [], [public([p/1, q/1])], [p(0), q(0)]),
		create_category(Descendant, [extends(Parent)], [], [p(1), (p(X) :- ^^p(X)), q(1), (q(X) :- ::p(X))]),
		abolish_category(Descendant),
		abolish_category(Parent).

	succeeds(create_category_1_17) :-
		create_category(Category, [], [op(567, xfx, foo)], []),
		{\+ current_op(567, xfx, foo)},
		abolish_category(Category).

:- end_object.
