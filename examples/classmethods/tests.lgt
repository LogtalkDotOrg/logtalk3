%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:3:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2020-12-24,
		comment is 'Unit tests for the "classmethods" example.'
	]).

	:- uses(lgtunit, [op(700, xfx, =~=), (=~=)/2]).

	cover(metacircle).
	cover(circle).
	cover(c42).

	test(classmethods_01, true(Area =~= 3.14159265358979)) :-
		circle::area(1.0, Area).

	test(classmethods_02, true(Area =~= 24.630086404144)) :-
		c42::area(Area).

	test(classmethods_03, true(Area =~= 4.5238934211693)) :-
		circle::new(1.2, 7.9, 2.0, Circle),
		Circle::area(Area).

:- end_object.
