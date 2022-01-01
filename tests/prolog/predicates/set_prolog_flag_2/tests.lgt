%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2021-07-08,
		comment is 'Unit tests for the ISO Prolog standard set_prolog_flag/2 built-in predicate.'
	]).

	:- uses(lgtunit, [
		assertion/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.17.1.4

	test(iso_set_prolog_flag_2_01, true) :-
		{set_prolog_flag(unknown, fail)},
		{current_prolog_flag(unknown, fail)}.

	test(iso_set_prolog_flag_2_02, error(instantiation_error)) :-
		{set_prolog_flag(_X, off)}.

	test(iso_set_prolog_flag_2_03, error(type_error(atom,5))) :-
		{set_prolog_flag(5, decimals)}.

	test(iso_set_prolog_flag_2_04, error(domain_error(prolog_flag,date))) :-
		{set_prolog_flag(date, 'July 1999')}.

	test(iso_set_prolog_flag_2_05, error(domain_error(flag_value,debug+trace))) :-
		{set_prolog_flag(debug, trace)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(eddbali_set_prolog_flag_2_06, error(permission_error(modify,flag,max_arity))) :-
		{set_prolog_flag(max_arity, 40)}.

	% tests from the Logtalk portability work

	test(lgt_set_prolog_flag_2_07, error(instantiation_error)) :-
		{set_prolog_flag(double_quotes, _)}.

	test(lgt_set_prolog_flag_2_08, error(domain_error(flag_value,double_quotes+foo))) :-
		{set_prolog_flag(double_quotes, foo)}.

	test(lgt_set_prolog_flag_2_09, error(instantiation_error)) :-
		{set_prolog_flag(unknown, _)}.

	test(lgt_set_prolog_flag_2_10, error(domain_error(flag_value,unknown+foo))) :-
		{set_prolog_flag(unknown, foo)}.

	test(lgt_set_prolog_flag_2_11, error(instantiation_error)) :-
		{set_prolog_flag(char_conversion, _)}.

	test(lgt_set_prolog_flag_2_12, error(domain_error(flag_value,char_conversion+foo))) :-
		{set_prolog_flag(char_conversion, foo)}.

	% check that the set_prolog_flag/2 recognizes all flags, thus including read-only flags
	test(lgt_set_prolog_flag_2_13, true) :-
		forall(
			{current_prolog_flag(Flag, Value)},
			assertion(flag(Flag), catch({set_prolog_flag(Flag, Value)}, error(Error,_), Error \== domain_error(prolog_flag,Flag)))
		).

:- end_object.
