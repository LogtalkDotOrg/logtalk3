%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		date is 2019-11-18,
		comment is 'Unit tests for the "threads/pmq" example.'
	]).

	cover(pmq).
	cover(split).

	test(pmq_01, true(Messages == [])) :-
		pmq::messages(Messages).

	test(pmq_02, true(Messages == [who, let, the, dogs, out])) :-
		pmq::(send(13-let), send(5-out), send(11-the), send(17-who), send(7-dogs)),
		pmq::messages(Messages).

	test(pmq_03, true(Messages == [])) :-
		pmq::messages(Messages).

	test(pmq_04, true(Messages == [have, fun, ':-)'])) :-
		pmq::(send(8-fun), send(11-have), send(3-':-)')),
		pmq::messages(Messages).

	test(pmq_05, true(Messages == [])) :-
		pmq::messages(Messages).

	test(split_01, true(Messages == [])) :-
		split::messages(Messages).

	test(split_02, true(Messages == [let, the, who, out, dogs])) :-
		split::(send(13-let), send(5-out), send(11-the), send(17-who), send(7-dogs)),
		split::messages(Messages).

	test(split_03, true(Messages == [])) :-
		split::messages(Messages).

	test(split_04, true(Messages == [have, fun, ':-)'])) :-
		split::(send(8-fun), send(11-have), send(3-':-)')),
		split::messages(Messages).

	test(split_05, true(Messages == [])) :-
		split::messages(Messages).

:- end_object.
