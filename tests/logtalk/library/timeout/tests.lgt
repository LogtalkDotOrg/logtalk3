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
		version is 0.3,
		author is 'Paulo Moura',
		date is 2019/05/07,
		comment is 'Unit tests for the "timeout" library.'
	]).

	:- uses(timeout, [
		call_with_timeout/2
	]).

	cover(timeout).

	throws(timeout_01, error(timeout((repeat,fail)), _)) :-
		call_with_timeout((repeat,fail), 0.1).

	succeeds(timeout_02) :-
		call_with_timeout(true, 0.1).

	deterministic(timeout_03) :-
		call_with_timeout(repeat, 0.1).

	fails(timeout_04) :-
		call_with_timeout(fail, 0.1).

:- end_object.
