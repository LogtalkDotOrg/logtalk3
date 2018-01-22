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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/04/05,
		comment is 'Unit tests for the ISO Prolog standard xor/2 built-in function.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.4.6.4

	succeeds(iso_xor_2_01) :-
		{X is xor(10, 12)},
		X == 6.

	succeeds(iso_xor_2_02) :-
		{X is xor(125, 255)},
		X == 130.

	succeeds(iso_xor_2_03) :-
		% implementation defined value
		{_X is xor(-10, 12)}.

	% tests from the Logtalk portability work

	throws(lgt_xor_2_04, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is xor(10, N)}.

	throws(lgt_xor_2_05, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is xor(N, 12)}.

	throws(lgt_xor_2_06, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is xor(10, Foo)}.

	throws(lgt_xor_2_07, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is xor(Foo, 12)}.

	throws(lgt_xor_2_08, error(type_error(evaluable,foo/1),_)) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is xor(10, Foo)}.

	throws(lgt_xor_2_09, error(type_error(evaluable,foo/1),_)) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is xor(Foo, 12)}.

	throws(lgt_xor_2_10, error(type_error(evaluable,foo/2),_)) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is xor(2, Foo)}.

	throws(lgt_xor_2_11, error(type_error(evaluable,foo/2),_)) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is xor(Foo, 3)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).

:- end_object.
