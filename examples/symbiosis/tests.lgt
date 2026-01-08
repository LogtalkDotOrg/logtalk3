%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2024-11-09,
		comment is 'Unit tests for the "symbiosis" example.'
	]).

	test(symbiosis_1, true) :-
		symbiosis::p.

	test(symbiosis_2, true(L == [97, 98, 99])) :-
		symbiosis::q(L).

	test(symbiosis_3, true(L == [1, 2, 3])) :-
		symbiosis::r(L).

	test(symbiosis_4, true(L == [2, 3, 4])) :-
		symbiosis::s(L).

	test(symbiosis_5, true(L == [2, 3, 4])) :-
		symbiosis::t(L).

:- end_object.
