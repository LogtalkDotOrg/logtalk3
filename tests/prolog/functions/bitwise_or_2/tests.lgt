%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		comment is 'Unit tests for the ISO Prolog standard (\\/)/1 built-in function.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.4.4.4

	succeeds(iso_bitwise_or_2_01) :-
		{X is '\\/'(10, 12)},
		X == 14.

	succeeds(iso_bitwise_or_2_02) :-
		{X is \/(10, 12)},
		X == 14.

	succeeds(iso_bitwise_or_2_03) :-
		{X is '\\/'(125, 255)},
		X == 255.

	succeeds(iso_bitwise_or_2_04) :-
		% assumes two's complement representation for negative integers
		{X is '\\/'(-10, 12)},
		X == -2.

	throws(iso_bitwise_or_2_05, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '\\/'(77, N)}.

	throws(iso_bitwise_or_2_06, error(type_error(evaluable,foo/0),_)) :-
		% example fixed in ISO/IEC 13211-1:1995/Cor.1:2007
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '\\/'(Foo, 2)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_bitwise_or_2_07, error(type_error(integer,10.0),_)) :-
		{_X is '\\/'(10.0, 12)}.

	% tests from the Logtalk portability work

	throws(lgt_bitwise_or_2_08, error(type_error(integer,12.0),_)) :-
		{_X is '\\/'(10, 12.0)}.

	throws(lgt_bitwise_or_1_09, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '\\/'(N, 12)}.

	throws(lgt_bitwise_or_2_10, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '\\/'(10, Foo)}.

	throws(lgt_bitwise_or_2_11, error(type_error(evaluable,foo/1),_)) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is '\\/'(Foo, 12)}.

	throws(lgt_bitwise_or_2_12, error(type_error(evaluable,foo/1),_)) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is '\\/'(10, Foo)}.

	throws(lgt_bitwise_or_2_13, error(type_error(evaluable,foo/2),_)) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is '\\/'(Foo, 12)}.

	throws(lgt_bitwise_or_2_14, error(type_error(evaluable,foo/2),_)) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is '\\/'(10, Foo)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).

:- end_object.
