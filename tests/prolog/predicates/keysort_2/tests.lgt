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
		date is 2015/05/05,
		comment is 'Unit tests for the ISO Prolog standard keysort/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.4.4.4

	succeeds(iso_keysort_2_01) :-
		{keysort([1-1, 1-1], Sorted)},
		Sorted == [1-1, 1-1].

	succeeds(iso_keysort_2_02) :-
		{keysort([2-99, 1-a, 3-f(_), 1-z, 1-a, 2-44], Sorted)},
		Sorted = [1-a, 1-z, 1-a, 2-99, 2-44, 3-f(_)].

	succeeds(iso_keysort_2_03) :-
		{keysort([X-1,1-1],[2-1,1-1])},
		X == 2.

	- succeeds(iso_keysort_2_04) :-
		% STO; Undefined.
		{Pairs = [1-2|Pairs], keysort(Pairs, _Sorted)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		succeeds(iso_keysort_2_05) :-
			{keysort([V-V], V)}.
	:- else.
		- succeeds(iso_keysort_2_05) :-
			% STO; Undefined.
			{keysort([V-V], V)}.
	:- endif.

	% tests from the ECLiPSe test suite

	throws(eclipse_keysort_2_06, error(instantiation_error,_)) :-
		{keysort(_, _)}.

	throws(eclipse_keysort_2_07, error(instantiation_error,_)) :-
		{keysort([1-a|_], _)}.

	throws(eclipse_keysort_2_08, error(type_error(list,3),_)) :-
		{keysort(3, _)}.

	throws(eclipse_keysort_2_09, error(type_error(list,[1-a|b]),_)) :-
		{keysort([1-a|b], _)}.

	throws(eclipse_keysort_2_10, error(type_error(list,3),_)) :-
		{keysort([], 3)}.

	throws(eclipse_keysort_2_11, error(type_error(list,[1-a|b]),_)) :-
		{keysort([], [1-a|b])}.

	throws(eclipse_keysort_2_12, error(instantiation_error,_)) :-
		{keysort([_], _)}.

	throws(eclipse_keysort_2_13, error(type_error(pair,1/a),_)) :-
		{keysort([1/a], _)}.

	throws(eclipse_keysort_2_14, error(type_error(pair,1/a),_)) :-
		{keysort([], [1/a])}.

:- end_object.
