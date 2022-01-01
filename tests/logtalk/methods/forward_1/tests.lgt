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
		date is 2019-05-31,
		comment is 'Unit tests for the forward/1 built-in method.'
	]).

	test(forward_1_01, true(X == 1)) :-
		test_object_2::p(X).

	test(forward_1_02, true(X == 2)) :-
		test_object_3::q(X).

	test(forward_1_03, true(X == 1)) :-
		test_object_4::p(X).

	test(forward_1_04, ball(error(existence_error(predicate_declaration,r/3),logtalk(test_object_1::r(_,_,_),_)))) :-
		test_object_4::r(_, _, _).

:- end_object.
