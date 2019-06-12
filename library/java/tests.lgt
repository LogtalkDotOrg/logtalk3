%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
		version is 0.3,
		author is 'Paulo Moura',
		date is 2019/06/12,
		comment is 'Unit tests for the "java" library.'
	]).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	cover(java(_, _)).
	cover(java).

	% "java(_, _)" object tests

	test(java_2_new_1_01, true) :-
		java('java.lang.Object')::new(_).

	test(java_2_new_2_01, true) :-
		java('java.lang.String')::new([abc], _).

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

	test(java_2_forward_1_01, true(integer(HashCode))) :-
		java('java.lang.Object')::new(Reference),
		java(Reference, HashCode)::hashCode.

	test(java_2_forward_1_02, true(integer(HashCode))) :-
		java('java.lang.Object')::new(Reference),
		java('java.lang.System', HashCode)::identityHashCode(Reference).

	test(java_2_get_field_2_01, true(Pi =~= 3.141592653589793)) :-
		java('java.lang.Math')::get_field('PI', Pi).

	% "java" object tests

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

	test(java_arrays_01, true(Terms == [a,42,foo])) :-
		java::terms_to_array([a,42,foo], Array),
		java::array_to_terms(Array, Terms).

	test(java_arrays_02, true(Terms =~= [2.72,3.14,9.8])) :-
		java::terms_to_array([2.72,3.14,9.8], Array),
		java::array_to_terms(Array, Terms).

	test(java_arrays_03, true) :-
		java::terms_to_array([a,42,foo], Array),
		java::array_to_terms(Array, Terms, Length),
		^^assertion(terms, Terms == [a,42,foo]),
		^^assertion(length, Length == 3).

	test(java_iterator_element_2_01, true(Elements == [a,b,c])) :-
		java('java.util.ArrayList')::new(ArrayList),
		java(ArrayList)::(add(a), add(b), add(c)),
		java(ArrayList, Iterator)::iterator,
		findall(Element, java::iterator_element(Iterator,Element), Elements).

:- end_object.
