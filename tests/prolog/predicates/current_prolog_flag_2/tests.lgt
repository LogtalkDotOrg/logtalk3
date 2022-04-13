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
		date is 2022-04-13,
		comment is 'Unit tests for the ISO Prolog standard current_prolog_flag/2 built-in predicate.'
	]).

	:- uses(lgtunit, [
		assertion/1, assertion/2
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.17.2.4

	test(iso_current_prolog_flag_2_01, true) :-
		{current_prolog_flag(debug, off)}.

	test(iso_current_prolog_flag_2_02, true((Bounded == true; Bounded == false))) :-
		{current_prolog_flag(bounded, Bounded)}.

	test(iso_current_prolog_flag_2_03, true) :-
		(	{current_prolog_flag(bounded, true)} ->
			{current_prolog_flag(max_integer, MaxInteger)},
			integer(MaxInteger)
		;	true
		).

	test(iso_current_prolog_flag_2_04, true) :-
		(	{current_prolog_flag(bounded, true)} ->
			{current_prolog_flag(min_integer, MinInteger)},
			integer(MinInteger)
		;	true
		).

	test(iso_current_prolog_flag_2_05, true((IntegerRoundingFunction == down; IntegerRoundingFunction == toward_zero))) :-
		{current_prolog_flag(integer_rounding_function, IntegerRoundingFunction)}.

	test(iso_current_prolog_flag_2_06, true((CharConversion == on; CharConversion == off))) :-
		{current_prolog_flag(char_conversion, CharConversion)}.

	test(iso_current_prolog_flag_2_07, true((Debug == on; Debug == off))) :-
		{current_prolog_flag(debug, Debug)}.

	test(iso_current_prolog_flag_2_08, true) :-
		{current_prolog_flag(max_arity, MaxArity)},
		(	MaxArity == unbounded ->
			true
		;	integer(MaxArity), MaxArity > 0
		).

	test(iso_current_prolog_flag_2_09, true((Unknown == error; Unknown == fail; Unknown == warning))) :-
		{current_prolog_flag(unknown, Unknown)}.

	test(iso_current_prolog_flag_2_10, true((DoubleQuotes == chars; DoubleQuotes == codes; DoubleQuotes == atom))) :-
		{current_prolog_flag(double_quotes, DoubleQuotes)}.

	test(iso_current_prolog_flag_2_11, error(type_error(atom,5))) :-
		{current_prolog_flag(5, _V)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(eddbali_current_prolog_flag_2_12, true) :-
		{set_prolog_flag(unknown, warning), current_prolog_flag(unknown, warning)}.

	test(eddbali_current_prolog_flag_2_13, false) :-
		{set_prolog_flag(unknown, warning), current_prolog_flag(unknown,error)}.

	test(eddbali_current_prolog_flag_2_14, error(domain_error(prolog_flag,warning))) :-
		{current_prolog_flag(warning,_)}.

	test(eddbali_current_prolog_flag_2_15, error(type_error(atom,1+2))) :-
		{current_prolog_flag(1+2, flag)}.

	% tests for de facto standard flags defined by the Prolog Commons initiative

	test(commons_current_prolog_flag_2_16, true(atom(Dialect))) :-
		{current_prolog_flag(dialect, Dialect)}.

	test(commons_current_prolog_flag_2_17, true((integer(Major), integer(Minor), integer(Patch)))) :-
		{current_prolog_flag(version_data, VersionData)},
		compound(VersionData),
		VersionData =.. [_Dialect, Major, Minor, Patch| _Others].

	test(commons_current_prolog_flag_2_18, true(functor(VersionData, Dialect, _))) :-
		{current_prolog_flag(dialect, Dialect), current_prolog_flag(version_data, VersionData)}.

	% tests from the Logtalk portability work

	test(lgt_current_prolog_flag_2_19, true) :-
		forall(
			{current_prolog_flag(Flag, _)},
			assertion(atom(Flag))
		).

	test(lgt_current_prolog_flag_2_20, true) :-
		forall(
			{current_prolog_flag(Flag, Value)},
			assertion(flag(Flag), nonvar(Value))
		).

	% tests for the new occurs_check flag that's becoming a de facto standard

	test(lgt_current_prolog_flag_2_21, true((Value == false; Value == true; Value == error)), [condition(catch(current_prolog_flag(occurs_check,_),_,fail))]) :-
		{current_prolog_flag(occurs_check, Value)}.

:- end_object.
