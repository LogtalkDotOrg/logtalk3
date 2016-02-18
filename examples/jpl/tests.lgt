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
		version is 1.1,
		author is 'Paulo Moura and Sergio Castro',
		date is 2015/10/06,
		comment is 'Unit tests for the "jpl" example.'
	]).

	cover(java(_,_)).
	cover(java(_)).

	test(jpl_1) :-
		java('java.lang.System')::getProperty('java.version').

	test(jpl_2) :-
		java('java.lang.System', Version)::getProperty('java.version'),
		atom(Version).

	test(jpl_3) :-
		java('java.lang.System', Version)::invoke(getProperty('java.version')),
		atom(Version).

	test(jpl_4) :-
		java('java.lang.Math')::get_field('PI', Pi),
		float(Pi).

	- test(jpl_5) :-
		java('java.awt.Rectangle')::new([100, 20], Rectangle),
		java(Rectangle)::set_field(width, 300),
		java(Rectangle)::get_field(width, Value),
		Value == 300.

	test(jpl_6) :-
		java('java.util.ArrayList')::new(ArrayList),
		java(ArrayList)::(add('Paulo'), add('Carlos'), add('Helena')),
		java(ArrayList, Iterator)::iterator,
		findall(
			Name, 
			(	repeat,
				java(Iterator, HasNext)::hasNext,
				(	HasNext == @true ->
					java(Iterator, Name)::next
				;	!,
					fail
				)
			),
			Names
		),
		Names == ['Paulo', 'Carlos', 'Helena'].

:- end_object.
