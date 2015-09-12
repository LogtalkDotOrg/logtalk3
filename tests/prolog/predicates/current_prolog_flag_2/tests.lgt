%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/10/14,
		comment is 'Unit tests for the ISO Prolog standard current_prolog_flag/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.17.2.4

	succeeds(iso_current_prolog_flag_2_01) :-
		{current_prolog_flag(debug, off)}.

	succeeds(iso_current_prolog_flag_2_02) :-
		{current_prolog_flag(bounded, Bounded)},
		(	Bounded == true ->
			true
		;	Bounded == false
		).

	succeeds(iso_current_prolog_flag_2_03) :-
		(	{current_prolog_flag(bounded, true)} ->
			{current_prolog_flag(max_integer, MaxInteger)},
			integer(MaxInteger)
		;	true
		).

	succeeds(iso_current_prolog_flag_2_04) :-
		(	{current_prolog_flag(bounded, true)} ->
			{current_prolog_flag(min_integer, MinInteger)},
			integer(MinInteger)
		;	true
		).

	succeeds(iso_current_prolog_flag_2_05) :-
		{current_prolog_flag(integer_rounding_function, IntegerRoundingFunction)},
		(	IntegerRoundingFunction == down ->
			true
		;	IntegerRoundingFunction == toward_zero
		).

	succeeds(iso_current_prolog_flag_2_06) :-
		{current_prolog_flag(char_conversion, CharConversion)},
		(	CharConversion == on ->
			true
		;	CharConversion == off
		).

	succeeds(iso_current_prolog_flag_2_07) :-
		{current_prolog_flag(debug, Debug)},
		(	Debug == on ->
			true
		;	Debug == off
		).

	succeeds(iso_current_prolog_flag_2_08) :-
		{current_prolog_flag(max_arity, MaxArity)},
		(	MaxArity == unbounded ->
			true
		;	integer(MaxArity), MaxArity > 0
		).

	succeeds(iso_current_prolog_flag_2_09) :-
		{current_prolog_flag(unknown, Unknown)},
		(	Unknown == error ->
			true
		;	Unknown == fail ->
			true
		;	Unknown == warning
		).

	succeeds(iso_current_prolog_flag_2_10) :-
		{current_prolog_flag(double_quotes, DoubleQuotes)},
		(	DoubleQuotes == chars ->
			true
		;	DoubleQuotes == codes ->
			true
		;	DoubleQuotes == atom
		).

	throws(iso_current_prolog_flag_2_11, error(type_error(atom,5),_)) :-
		{current_prolog_flag(5, _V)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	succeeds(eddbali_current_prolog_flag_2_12) :-
		{set_prolog_flag(unknown, warning), current_prolog_flag(unknown, warning)}.

	fails(eddbali_current_prolog_flag_2_13) :-
		{set_prolog_flag(unknown, warning), current_prolog_flag(unknown,error)}.

	throws(eddbali_current_prolog_flag_2_14, error(domain_error(prolog_flag,warning),_)) :-
		{current_prolog_flag(warning,_)}.

	throws(eddbali_current_prolog_flag_2_15, error(type_error(atom,1+2),_)) :-
		{current_prolog_flag(1+2, flag)}.

	% tests for de facto standard flags defined by the Prolog Commons initiative

	succeeds(commons_current_prolog_flag_2_16) :-
		{current_prolog_flag(dialect, Dialect)},
		atom(Dialect).

	succeeds(commons_current_prolog_flag_2_17) :-
		{current_prolog_flag(version_data, VersionData)},
		compound(VersionData),
		VersionData =.. [_Dialect, Major, Minor, Patch| _Others],
		integer(Major), integer(Minor), integer(Patch).

	succeeds(commons_current_prolog_flag_2_18) :-
		{current_prolog_flag(dialect, Dialect), current_prolog_flag(version_data, VersionData)},
		functor(VersionData, Dialect, _).

:- end_object.
