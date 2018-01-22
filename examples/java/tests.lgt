%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
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
		version is 1.4,
		author is 'Paulo Moura and Sergio Castro',
		date is 2017/01/16,
		comment is 'Unit tests for the "java" example.'
	]).

	test(java_01) :-
		java('java.lang.System')::getProperty('java.version').

	test(java_02) :-
		java('java.lang.System', Version)::getProperty('java.version'),
		atom(Version).

	test(java_03) :-
		java('java.lang.System', Version)::invoke(getProperty('java.version')),
		atom(Version).

	test(java_04) :-
		java('java.lang.Integer', Integer)::parseInt('123'),
		Integer == 123.

	test(java_05) :-
		java('java.lang.Math')::get_field('PI', Pi),
		float(Pi).

	test(java_06) :-
		java('java.util.Calendar', Calendar)::getInstance,
		java(Calendar)::get_field('YEAR', Year),
		integer(Year).

	test(java_07) :-
		java('java.util.Date')::new(Date),
		java(Date, Time)::getTime,
		number(Time).

	test(java_08) :-
		java('java.util.Random')::new(Random),
		java(Random)::setSeed(12345),
		java(Random, Float)::nextFloat,
		float(Float).

	test(java_09) :-
		java('java.util.Random')::new([12345], Random),
		java(Random, Int)::nextInt,
		integer(Int).

	test(java_10) :-
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
		),
		Names == ['Paulo', 'Carlos', 'Helena'].

	test(java_11) :-
		java::terms_to_array([x, [1, a, 7, [y,z]], k, [], 3.14, foo(bar)], Array),
		java::array_to_terms(Array, List),
		List == [x, [1, a, 7, [y, z]], k, [], 3.14, foo(bar)].

:- end_object.
