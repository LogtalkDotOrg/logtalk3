%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2019/11/18,
		comment is 'Unit tests for the "threads/pmq" example.'
	]).

	cover(pmq).

	test(priority_queue_01, true(Ms == [])) :-
		pmq::important(Ms).

	test(priority_queue_02, true(Ms == [who, let, the, dogs, out])) :-
		pmq::(send(13-let), send(5-out), send(11-the), send(17-who), send(7-dogs)),
		pmq::important(Ms).

	test(priority_queue_03, true(Ms == [])) :-
		pmq::important(Ms).

	test(priority_queue_04, true(Ms == [have, fun, ':-)'])) :-
		pmq::(send(8-fun), send(11-have), send(3-':-)')),
		pmq::important(Ms).

	test(priority_queue_05, true(Ms == [])) :-
		pmq::important(Ms).

:- end_object.
