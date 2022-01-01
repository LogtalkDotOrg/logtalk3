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


:- category(engine).

	:- public(prove/1).

	:- public(if/1).
	:- dynamic(if/1).

	:- op(200,  fx, if).
	:- op(150, xfx, then).
	:- op(100, xfy, and).

	prove(true) :-
		!.
	prove(Cond and Conds) :-
		!,
		prove(Cond),
		prove(Conds).
	prove(Fact) :-
		clause(Fact, true).
	prove(Conclusion) :-
		clause(if Conds then Conclusion, true),
		prove(Conds).

:- end_category.


:- object(rules,
	imports(engine)).

	:- public([weather/1, weekday/1, time/1, goto/1]).
	:- dynamic([weather/1, weekday/1, time/1, goto/1]).

	:- dynamic(if/1).

	:- op(200,  fx, if).
	:- op(150, xfx, then).
	:- op(100, xfy, and).

	if weather(sunny) and weekday(weekend) and time(day) then goto(beach).
	if weather(raining) and weekday(weekend) and time(night) then goto(cinema).
	if weekday(workday) and time(day) then goto(work).

	weather(raining).
	weekday(weekend).
	time(night).

:- end_object.
