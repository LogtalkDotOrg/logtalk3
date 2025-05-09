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
		version is 1:2:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2023-06-04,
		comment is 'Unit tests for the "assign_parameters" example.'
	]).

	cover(rectangle(_, _, _)).
	cover(fsm(_, _, _)).

	test(assignvars_1, true) :-
		rectangle(2, 3, S)::init,
		rectangle(2, 3, S)::position(X0, Y0),
		^^assertion(X0-Y0 == 0-0),
		rectangle(2, 3, S)::move(3, 7),
		rectangle(2, 3, S)::position(X1, Y1),
		^^assertion(X1-Y1 == 3-7),
		rectangle(2, 3, S)::move(2, 5),
		rectangle(2, 3, S)::position(X2, Y2),
		^^assertion(X2-Y2 == 2-5),
		rectangle(2, 3, S)::area(Area),
		^^assertion(Area == 6).

	test(assignvars_2, true(Solutions == [[red-0-red, red-1-green, red-2-red, yellow-0-red, yellow-1-green, yellow-2-red, green-0-yellow, green-1-yellow, green-2-red]-red-[red]])) :-
		^^suppress_text_output,
		findall(T-I-F, {fsm(T, I, F)}::recognise([0,1,1,2,1,2,0]), Solutions).

	test(assignvars_3, false) :-
		^^suppress_text_output,
		{fsm(_T, _I, _F)}::recognise([0,1,1,2,1,2,1,0]).

:- end_object.
