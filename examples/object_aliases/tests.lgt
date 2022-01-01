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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2019-06-01,
		comment is 'Unit tests for the "object_aliases" example.'
	]).

	cover(experiments).
	cover(simple(_)).

	test(object_aliases_experiments_01, true(TotalLess + TotalEqual + TotalGreater =:= 42)) :-
		experiments::stats(TotalLess, TotalEqual, TotalGreater).

	test(object_aliases_simple_01, true(Top == 1-a)) :-
		simple(<)::insert_top([3-c,1-a,2-b], Top).

	test(object_aliases_simple_02, true(Top == 3-c)) :-
		simple(>)::insert_top([3-c,1-a,2-b], Top).

:- end_object.
