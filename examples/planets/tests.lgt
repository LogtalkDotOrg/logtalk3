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
		date is 2020-12-24,
		comment is 'Unit tests for the "planets" example.'
	]).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	cover(m1).
	cover(m2).
	cover(planet).
	cover(earth).
	cover(mars).

	test(planets_01, true(W1 =~= 29.41995)) :-
		earth::weight(m1, W1).

	test(planets_02, true(W1 =~= 11.162279999999999)) :-
		mars::weight(m1, W1).

	test(planets_03, true(W2 =~= 39.2266)) :-
		earth::weight(m2, W2).

	test(planets_04, true(W2 =~= 14.88304)) :-
		mars::weight(m2, W2).

:- end_object.
