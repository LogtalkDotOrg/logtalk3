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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-02-24,
		comment is 'Unit tests for the "units" example.'
	]).

	cover(dict(_)).

	test(units_dict_1_succeeds, true(Dict == [a=1,b=2,c=3,d=4,e=5])) :-
		dict([a=1,b=2,c=3,d=4,e=5])::dict(Dict).

	test(units_lookup_2_succeeds, true(C == 3)) :-
		dict([a=1,b=2,c=3,d=4,e=5])::lookup(c, C).

	test(units_lookup_2_fails, false) :-
		dict([a=1,b=2,c=3,d=4,e=5])::lookup(f, _).

:- end_object.
