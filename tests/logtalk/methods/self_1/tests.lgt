%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		author is 'Paulo Moura',
		date is 2017-04-09,
		comment is 'Unit tests for the self/1 built-in method.'
	]).

	test(self_1) :-
		self(Self),
		Self == tests.

	test(self_2) :-
		self(tests).

	test(self_3) :-
		\+ self(other).

	test(self_4) :-
		Goal = self(Self),
		call(Goal),
		Self == tests.

	test(self_5) :-
		self_1_test_object_1::p(Self),
		Self == self_1_test_object_1.

:- end_object.
