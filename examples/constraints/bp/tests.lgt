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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2021-08-20,
		comment is 'Unit tests for the "constraints/bp" example.'
	]).

	test(constraints_bp_1, true) :-
		clique::go.

	test(constraints_bp_2, true) :-
		magic::go.

	test(constraints_bp_3, true(V == [9,5,6,7,1,0,8,2])) :-
		puzzle::solve(V).

	test(constraints_bp_4, true) :-
		steiner::go.

	- test(constraints_bp_5, true) :-
		queens3::top.

	- test(constraints_bp_6, true) :-
		srq::q.

	- test(constraints_bp_7, true) :-
		srq::q_all.

:- end_object.
