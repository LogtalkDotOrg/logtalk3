%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2020 Michael T. Richter and Paulo Moura <pmoura@logtalk.org>
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
		author is 'Michael T. Richter and Paulo Moura',
		date is 2020-02-19,
		comment is 'Unit tests for the "pengines" example.'
	]).

	cover(engines).

	test(pengines_01, true(Answers == [q(a), q(b), q(c)])) :-
		engines::ask(Engine),
		engines::answers(Engine, Answers).

	test(pengines_02, true(Answers == [q(a), q(b), q(c)])) :-
		engines::ask(Engine),
		findall(Answer, engines::answer(Engine, Answer), Answers).

:- end_object.
