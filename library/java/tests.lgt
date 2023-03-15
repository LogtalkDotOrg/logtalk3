%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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
		version is 0:6:0,
		author is 'Paulo Moura',
		date is 2023-03-15,
		comment is 'Unit tests for the "java" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	cover(java(_, _)).
	cover(java(_)).
	cover(java).

	% new/1-2 tests

	test(java_2_new_1_01, true) :-
		java('java.lang.Object')::new(_).

	test(java_2_new_1_02, true(number(Time))) :-
		java('java.util.Date')::new(Date),
		java(Date, Time)::getTime.

	test(java_2_new_2_02, true) :-
		java('java.lang.String')::new([abc], _).

	% invoke/1-2 tests

	test(java_2_invoke_1_01, true(integer(HashCode))) :-
		java('java.lang.Object')::new(Reference),
		java(Reference, HashCode)::invoke(hashCode).

	test(java_2_invoke_1_02, true(integer(HashCode))) :-
		java('java.lang.Object')::new(Reference),
		java('java.lang.System', HashCode)::invoke(identityHashCode(Reference)).

	test(java_2_invoke_1_03, true(integer(HashCode))) :-
		java('java.lang.String')::new([abc], Reference),
		java('java.lang.System', HashCode)::invoke(identityHashCode(Reference)).

	test(java_2_invoke_2_01, true(integer(HashCode))) :-
		java('java.lang.Object')::new(Reference),
		java('java.lang.System', HashCode)::invoke(identityHashCode, [Reference]).

	test(java_03, true(atom(Version))) :-
		java('java.lang.System', Version)::invoke(getProperty('java.version')).

	% forwarding tests

	test(java_2_forward_1_01, true) :-
		java('java.lang.System')::getProperty('java.version').

	test(java_2_forward_1_02, true(atom(Version))) :-
		java('java.lang.System', Version)::getProperty('java.version').

	test(java_2_forward_1_03, true(Integer == 123)) :-
		java('java.lang.Integer', Integer)::parseInt('123').

	test(java_2_forward_1_04, true(integer(HashCode))) :-
		java('java.lang.Object')::new(Reference),
		java(Reference, HashCode)::hashCode.

	test(java_2_forward_1_05, true(integer(HashCode))) :-
		java('java.lang.Object')::new(Reference),
		java('java.lang.System', HashCode)::identityHashCode(Reference).

	test(java_2_forward_1_06, true(float(Float))) :-
		java('java.util.Random')::new(Random),
		java(Random)::setSeed(12345),
		java(Random, Float)::nextFloat.

	test(java_2_forward_1_07, true(integer(Int))) :-
		java('java.util.Random')::new([12345], Random),
		java(Random, Int)::nextInt.

	% get_field/2 tests

	test(java_2_get_field_2_01, true(Pi =~= 3.141592653589793)) :-
		java('java.lang.Math')::get_field('PI', Pi).

	test(java_2_get_field_2_02, true(integer(Year))) :-
		java('java.util.Calendar', Calendar)::getInstance,
		java(Calendar)::get_field('YEAR', Year).

	% "java" object utility predicate tests

	test(java_true_1_01, true(ground(Reference))) :-
		java::true(Reference).

	test(java_false_1_01, true(ground(Reference))) :-
		java::false(Reference).

	test(java_void_1_01, true(ground(Reference))) :-
		java::void(Reference).

	test(java_null_1_01, true(ground(Reference))) :-
		java::null(Reference).

	test(java_is_true_1_01, true) :-
		java::true(Reference),
		java::is_true(Reference).

	test(java_is_false_1_01, true) :-
		java::false(Reference),
		java::is_false(Reference).

	test(java_is_void_1_01, true) :-
		java::void(Reference),
		java::is_void(Reference).

	test(java_is_null_1_01, true) :-
		java::null(Reference),
		java::is_null(Reference).

	test(java_is_object_1_01, true) :-
		java('java.lang.Object')::new(Reference),
		java::is_object(Reference).

	test(java_value_reference_2_01, true) :-
		java::value_reference(true, Reference),
		java::is_true(Reference).

	test(java_value_reference_2_02, true) :-
		java::value_reference(false, Reference),
		java::is_false(Reference).

	test(java_value_reference_2_03, true) :-
		java::value_reference(void, Reference),
		java::is_void(Reference).

	test(java_value_reference_2_04, true) :-
		java::value_reference(null, Reference),
		java::is_null(Reference).

	% terms_to_array/2 and array_to_terms/2 tests

	test(java_arrays_01, true(Terms == [a,42,foo])) :-
		java::terms_to_array([a,42,foo], Array),
		java::array_to_terms(Array, Terms).

	test(java_arrays_02, true(Terms =~= [2.72,3.14,9.8])) :-
		java::terms_to_array([2.72,3.14,9.8], Array),
		java::array_to_terms(Array, Terms).

	test(java_arrays_03, true(List == [0.0, 0])) :-
		java::terms_to_array([0.0, 0], Array),
		java::array_to_terms(Array, List).

	test(java_arrays_04, true) :-
		java::terms_to_array([a,42,foo], Array),
		java::array_to_terms(Array, Terms, Length),
		^^assertion(terms, Terms == [a,42,foo]),
		^^assertion(length, Length == 3).

	test(java_arrays_05, true(List == [x, [1, a, 7, [y, z]], k, [], foo(bar)])) :-
		java::terms_to_array([x, [1, a, 7, [y,z]], k, [], foo(bar)], Array),
		java::array_to_terms(Array, List).

	% array_list/2 tests

	test(java_array_list_2_01, true(List == [A,B,C])) :-
		java('java.lang.String')::new([a], A),
		java('java.lang.String')::new([b], B),
		java('java.lang.String')::new([c], C),
		java::array_list(Array, [A, B, C]),
		java::array_list(Array, List).

	% iterator_element/2 tests

	test(java_iterator_element_2_01, true(Elements == [a,b,c])) :-
		java('java.util.ArrayList')::new(ArrayList),
		java(ArrayList)::(add(a), add(b), add(c)),
		java(ArrayList, Iterator)::iterator,
		findall(Element, java::iterator_element(Iterator,Element), Elements).

	% map_element/2 tests

	test(java_map_element_2_01, true(Elements == [a-1,b-2,c-3])) :-
		java('java.util.TreeMap')::new(Map),
		forall(
			list::member(Key-Value0, [a-1,b-2,c-3]),
			(	java('java.lang.Integer')::new([Value0], Value),
				java(Map)::put(Key, Value)
			)
		),
		findall(
			Key-Value,
			(	java::map_element(Map, Key-Value0),
				java(Value0, Value)::intValue
			),
			Elements
		).

	% set_element/2 tests

	test(java_set_element_2_01, true(Elements == [a,b,c])) :-
		java('java.util.TreeSet')::new(Set),
		java(Set)::(add(a), add(b), add(c)),
		findall(Element, java::set_element(Set,Element), Elements).

	% test using Java iterators

	test(java_iterator, true(Names == ['Paulo', 'Carlos', 'Helena'])) :-
		java('java.util.ArrayList')::new(ArrayList),
		java(ArrayList)::(add('Paulo'), add('Carlos'), add('Helena')),
		java(ArrayList, Iterator)::iterator,
		findall(
			Name,
			(	repeat,
				java(Iterator, HasNext)::hasNext,
				(	java::is_true(HasNext) ->
					java(Iterator, Name)::next
				;	!,
					fail
				)
			),
			Names
		).

:- end_object.
