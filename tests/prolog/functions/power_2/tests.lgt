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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/04/05,
		comment is 'Unit tests for the ISO Prolog standard (**)/2 built-in function.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.3.1.4

	succeeds(iso_power_2_01) :-
		{X is '**'(5, 3)},
		X =~= 125.0.

	succeeds(iso_power_2_02) :-
		{X is '**'(-5.0, 3)},
		X =~= -125.0.

	succeeds(iso_power_2_03) :-
		{X is '**'(5, -1)},
		X =~= 0.2.

	throws(iso_power_2_04, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '**'(77, N)}.

	throws(iso_power_2_05, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '**'(Foo, 3)}.

	succeeds(iso_power_2_06) :-
		{X is '**'(5, 3.0)},
		X =~= 125.0.

	succeeds(iso_power_2_07) :-
		{X is '**'(0, 0.0)},
		X =~= 1.0.

	throws(lgt_power_2_08, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '**'(N, 77)}.

	throws(iso_power_2_09, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '**'(5, Foo)}.

	throws(iso_power_2_10, error(type_error(evaluable,foo/1),_)) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is '**'(Foo, 3)}.

	throws(iso_power_2_11, error(type_error(evaluable,foo/1),_)) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is '**'(5, Foo)}.

	throws(iso_power_2_12, error(type_error(evaluable,foo/2),_)) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is '**'(Foo, 3)}.

	throws(iso_power_2_13, error(type_error(evaluable,foo/2),_)) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is '**'(5, Foo)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).

:- end_object.
