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


% entities for testing predicate aliases defined in protocols and categories

:- protocol(alias_2_test_protocol_1).

	:- public(a/1).

:- end_protocol.


:- protocol(alias_2_test_protocol_2,
	extends(alias_2_test_protocol_1)).

	:- alias(alias_2_test_protocol_1, [
		a/1 as b/1
	]).

:- end_protocol.


:- category(alias_2_test_category,
	implements(alias_2_test_protocol_2)).

	:- alias(alias_2_test_protocol_2, [
		b/1 as c/1
	]).

	c(1).

:- end_category.


:- object(alias_2_test_prototype,
	imports(alias_2_test_category)).

:- end_object.


% objects for testing predicate aliases defined in classes and instances

:- object(alias_2_test_metaclass,
	instantiates(alias_2_test_metaclass)).

:- end_object.


:- object(alias_2_test_class,
	instantiates(alias_2_test_metaclass)).

	:- public(p/1).
	p(1).

:- end_object.


:- object(alias_2_test_subclass,
	instantiates(alias_2_test_metaclass),
	specializes(alias_2_test_class)).

	:- alias(alias_2_test_class, [
		p/1 as q/1
	]).

:- end_object.


:- object(alias_2_test_instance,
	instantiates(alias_2_test_subclass)).

	:- alias(alias_2_test_subclass, [
		p/1 as r/1
	]).

:- end_object.


% unit tests

:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2015/01/23,
		comment is 'Unit tests for the alias/2 built-in directive.'
	]).

	% test all possible syntaxes for the directive and the definition
	% of one or more than one alias per directive

	:- alias(lgtunit, [run/0 as run_alias1/0]).
	:- alias(lgtunit, [run/0 :: run_alias2/0]).
	:- alias(lgtunit, [
		run/0 as run_alias3/0, run/0 as run_alias4/0
	]).

	% tests for predicate aliases defined in prototypes

	test(alias_2_1) :-
		predicate_property(run_alias1, alias_of(run)),
		predicate_property(run_alias1, alias_declared_in(tests)),
		predicate_property(run_alias1, declared_in(lgtunit)),
		predicate_property(run_alias1, defined_in(lgtunit)).

	test(alias_2_2) :-
		predicate_property(run_alias2, alias_of(run)),
		predicate_property(run_alias2, alias_declared_in(tests)),
		predicate_property(run_alias2, declared_in(lgtunit)),
		predicate_property(run_alias2, defined_in(lgtunit)).

	test(alias_2_3) :-
		predicate_property(run_alias3, alias_of(run)),
		predicate_property(run_alias3, alias_declared_in(tests)),
		predicate_property(run_alias3, declared_in(lgtunit)),
		predicate_property(run_alias3, defined_in(lgtunit)).

	test(alias_2_4) :-
		predicate_property(run_alias4, alias_of(run)),
		predicate_property(run_alias4, alias_declared_in(tests)),
		predicate_property(run_alias4, declared_in(lgtunit)),
		predicate_property(run_alias4, defined_in(lgtunit)).

	% tests for predicate aliases defined in protocols and categories

	test(alias_2_5) :-
		alias_2_test_prototype::predicate_property(c(_), alias_of(b(_))),
		alias_2_test_prototype::predicate_property(c(_), alias_declared_in(alias_2_test_category)),
		alias_2_test_prototype::predicate_property(c(_), declared_in(alias_2_test_protocol_1)),
		alias_2_test_prototype::predicate_property(c(_), defined_in(alias_2_test_category)).

	test(alias_2_6) :-
		alias_2_test_prototype::predicate_property(b(_), alias_of(a(_))),
		alias_2_test_prototype::predicate_property(b(_), alias_declared_in(alias_2_test_protocol_2)),
		alias_2_test_prototype::predicate_property(b(_), declared_in(alias_2_test_protocol_1)).

	% tests for predicate aliases defined in instances and classes

	test(alias_2_7) :-
		alias_2_test_instance::predicate_property(q(_), alias_of(p(_))),
		alias_2_test_instance::predicate_property(q(_), alias_declared_in(alias_2_test_subclass)),
		alias_2_test_instance::predicate_property(q(_), declared_in(alias_2_test_class)),
		alias_2_test_instance::predicate_property(q(_), defined_in(alias_2_test_class)).

	test(alias_2_8) :-
		alias_2_test_instance::predicate_property(r(_), alias_of(p(_))),
		alias_2_test_instance::predicate_property(r(_), alias_declared_in(alias_2_test_instance)),
		alias_2_test_instance::predicate_property(r(_), declared_in(alias_2_test_class)),
		alias_2_test_instance::predicate_property(r(_), defined_in(alias_2_test_class)).

:- end_object.
