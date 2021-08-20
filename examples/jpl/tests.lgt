%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:5:0,
		author is 'Paulo Moura and Sergio Castro',
		date is 2021-08-20,
		comment is 'Unit tests for the "jpl" example.'
	]).

	test(java_01, true) :-
		java('java.lang.System')::getProperty('java.version').

	test(java_02, true(atom(Version))) :-
		java('java.lang.System', Version)::getProperty('java.version').

	test(java_03, true(atom(Version))) :-
		java('java.lang.System', Version)::invoke(getProperty('java.version')).

	test(java_04, true(Integer == 123)) :-
		java('java.lang.Integer', Integer)::parseInt('123').

	test(java_05, true(float(Pi))) :-
		java('java.lang.Math')::get_field('PI', Pi).

	test(java_06, true(integer(Year))) :-
		java('java.util.Calendar', Calendar)::getInstance,
		java(Calendar)::get_field('YEAR', Year).

	test(java_07, true(number(Time))) :-
		java('java.util.Date')::new(Date),
		java(Date, Time)::getTime.

	test(java_08, true(float(Float))) :-
		java('java.util.Random')::new(Random),
		java(Random)::setSeed(12345),
		java(Random, Float)::nextFloat.

	test(java_09, true(integer(Int))) :-
		java('java.util.Random')::new([12345], Random),
		java(Random, Int)::nextInt.

	test(java_10, true(Names == ['Paulo', 'Carlos', 'Helena'])) :-
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

	test(java_11, true(List == [x, [1, a, 7, [y, z]], k, [], 3.14, foo(bar)])) :-
		java::terms_to_array([x, [1, a, 7, [y,z]], k, [], 3.14, foo(bar)], Array),
		java::array_to_terms(Array, List).

:- end_object.
