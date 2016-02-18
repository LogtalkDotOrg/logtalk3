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
		version is 1.4,
		author is 'Paulo Moura',
		date is 2015/04/01,
		comment is 'Unit tests for the ISO Prolog standard compare/3 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.4.2.4

	succeeds(iso_compare_3_01) :-
		{compare(Order, 3, 5)},
		Order == (<).

	succeeds(iso_compare_3_02) :-
		{compare(Order, d, d)},
		Order == (=).

	succeeds(iso_compare_3_03) :-
		{compare(Order, Order, <)},
		Order == (<).

	fails(iso_compare_3_04) :-
		{compare(<, <, <)}.

	throws(iso_compare_3_05, error(type_error(atom,1+2),_)) :-
		{compare(1+2, 3, 3.0)}.

	throws(iso_compare_3_06, error(domain_error(order,>=),_)) :-
		{compare(>=, 3, 3.0)}.

	% standard order tests from the Logtalk portability work

	succeeds(lgt_compare_3_07) :-
		{compare(<, _X, 1.1)}.

	succeeds(lgt_compare_3_08) :-
		{compare(<, 1.1, 1)}.

	succeeds(lgt_compare_3_09) :-
		{compare(>, 1, 1.1)}.

	succeeds(lgt_compare_3_10) :-
		{compare(Order, 1.1, 1)},
		Order == (<).

	succeeds(lgt_compare_3_11) :-
		{compare(Order, 1, 1.1)},
		Order == (>).

	succeeds(lgt_compare_3_12) :-
		{compare(Order, 1.0, 1)},
		Order == (<).

	succeeds(lgt_compare_3_13) :-
		{compare(Order, 1, 1.0)},
		Order == (>).

	succeeds(lgt_compare_3_14) :-
		{compare(<, 1, a)}.

	succeeds(lgt_compare_3_15) :-
		{compare(<, a, a(_))}.

	succeeds(lgt_compare_3_16) :-
		{compare(<, a(_), a(_,_))}.

	succeeds(lgt_compare_3_17) :-
		{compare(<, b(_), a(_,_))}.

	succeeds(lgt_compare_3_18) :-
		{compare(<, a(1,2), a(1,3))}.

	succeeds(lgt_compare_3_19) :-
		{compare(<, a(1,2), b(1,2))}.

	% other tests

	succeeds(lgt_compare_3_20) :-
		{compare(>, (4,1,0), (4,0,1))}.

	fails(lgt_compare_3_21) :-
		{compare(>, (4,0,1), (4,1,0))}.

	fails(lgt_compare_3_22) :-
		{compare(<, (4,1,0), (4,0,1))}.

	succeeds(lgt_compare_3_23) :-
		{compare(<, (4,0,1), (4,1,0))}.

	succeeds(lgt_compare_3_24) :-
		{compare(>, (4,1,0), (4,0,1))}.

	fails(lgt_compare_3_25) :-
		{compare(>, (4,0,1), (4,1,0))}.

	fails(lgt_compare_3_26) :-
		{compare(<, (4,1,0), (4,0,1))}.

	succeeds(lgt_compare_3_27) :-
		{compare(<, (4,0,1), (4,1,0))}.

	succeeds(lgt_compare_3_28) :-
		{compare(Order, 1, 1+2)},
		Order == (<).

	succeeds(lgt_compare_3_29) :-
		{compare(Order, 1+2, 1)},
		Order == (>).

:- end_object.
