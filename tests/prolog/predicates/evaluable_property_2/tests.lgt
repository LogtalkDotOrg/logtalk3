%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:3:1,
		author is 'Paulo Moura',
		date is 2023-07-15,
		comment is 'Unit tests for the proposed evaluable_property/2 built-in predicate.'
	]).

	test(commons_evaluable_property_2_01, error(instantiation_error)) :-
		{evaluable_property(_, _)}.

	test(commons_evaluable_property_2_02, error(type_error(callable,1))) :-
		{evaluable_property(1, _)}.

	test(commons_evaluable_property_2_03, errors([type_error(callable,1), domain_error(evaluable_property,1)])) :-
		{evaluable_property(abs(_), 1)}.

	test(commons_evaluable_property_2_04, errors([type_error(callable,1), domain_error(evaluable_property,1)])) :-
		{evaluable_property(foo(_), 1)}.

	test(commons_evaluable_property_2_05, error(domain_error(evaluable_property,foobar))) :-
		{evaluable_property(abs(_), foobar)}.

	test(commons_evaluable_property_2_06, error(domain_error(evaluable_property,foobar))) :-
		{evaluable_property(foo(_), foobar)}.

	test(commons_evaluable_property_2_07, true) :-
		{evaluable_property(abs(_), built_in)}.

	test(commons_evaluable_property_2_08, true) :-
		{evaluable_property(abs(_), static)}.

	test(commons_evaluable_property_2_09, false) :-
		{evaluable_property(abs(_), (dynamic))}.

	test(commons_evaluable_property_2_10, false) :-
		{evaluable_property(abs(_), foreign)}.

	test(commons_evaluable_property_2_11, true(Templates == [number-number])) :-
		findall(Type-Return, {evaluable_property(abs(_), template(abs(Type), Return))}, Templates).

	test(commons_evaluable_property_2_12, true) :-
		{evaluable_property(pi, built_in)}.

	test(commons_evaluable_property_2_13, true) :-
		{evaluable_property(pi, static)}.

	test(commons_evaluable_property_2_14, false) :-
		{evaluable_property(pi, (dynamic))}.

	test(commons_evaluable_property_2_15, false) :-
		{evaluable_property(pi, foreign)}.

	test(commons_evaluable_property_2_16, true(Templates == [pi-float])) :-
		findall(Constant-Return, {evaluable_property(pi, template(Constant, Return))}, Templates).

:- end_object.
