%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		date is 2021-08-18,
		comment is 'Unit tests for the ISO Prolog standard float/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.3.4.4

	test(iso_float_1_predicate_01, true) :-
		{float(3.3)}.

	test(iso_float_1_predicate_02, true) :-
		{float(-3.3)}.

	test(iso_float_1_predicate_03, false) :-
		{float(3)}.

	test(iso_float_1_predicate_04, false) :-
		{float(atom)}.

	test(iso_float_1_predicate_05, false) :-
		{float(_X)}.

	% tests from the Logtalk portability work

	test(iso_float_1_predicate_06, true) :-
		{float(1.0)}.

:- end_object.
