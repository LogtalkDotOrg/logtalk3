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
		author is 'Paulo Moura',
		date is 2015/04/05,
		comment is 'Unit tests for the ISO Prolog standard (\\)/1 built-in function.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.4.5.4

	succeeds(iso_bitwise_complement_1_01) :-
		{X is '\\'('\\'(10))},
		X == 10.

	succeeds(iso_bitwise_complement_1_02) :-
		{X is \(\(10))},
		X == 10.

	succeeds(iso_bitwise_complement_1_03) :-
		% assumes two's complement representation for negative integers
		{X is \(10)},
		X == -11.

	throws(iso_bitwise_complement_1_04, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '\\'(N)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_bitwise_complement_1_05, error(type_error(integer,2.5),_)) :-
		{_X is '\\'(2.5)}.

	% tests from the Logtalk portability work

	throws(lgt_bitwise_complement_1_06, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '\\'(Foo)}.

	throws(lgt_bitwise_complement_1_07, error(type_error(evaluable,foo/1),_)) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is '\\'(Foo)}.

	throws(lgt_bitwise_complement_1_08, error(type_error(evaluable,foo/2),_)) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is '\\'(Foo)}.

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).

:- end_object.
