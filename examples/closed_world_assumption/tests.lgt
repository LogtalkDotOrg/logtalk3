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
		date is 2020-02-27,
		comment is 'Unit tests for the "closed_world_assumption" example.'
	]).

	cover(house).

	test(closed_world_assumption_01, true) :-
		house::attic.

	test(closed_world_assumption_02, fail) :-
		house::cellar.

	test(closed_world_assumption_03, error(existence_error(predicate_declaration,pool/0))) :-
		house::pool.

	test(closed_world_assumption_04, true) :-
		house::pleasant.

	test(closed_world_assumption_05, fail) :-
		house::practical.

	test(closed_world_assumption_06, error(existence_error(procedure,_))) :-
		house::fun.

:- end_object.
