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
		version is 1:1:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2024-01-15,
		comment is 'Unit tests for the "threads/tak" example.'
	]).

	test(tak_1, true(R == 7)) :-
		tak(1)::tak(18, 12, 6, R).

	test(tak_2, true(R == 7)) :-
		tak(3)::tak(18, 12, 6, R).

	test(tak_3, true(R == 14)) :-
		tak(1)::tak(21, 14, 7, R).

	test(tak_4, true(R == 14)) :-
		tak(3)::tak(21, 14, 7, R).

:- end_object.
