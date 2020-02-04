%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		date is 2019-04-28,
		comment is 'Unit tests for the "super_calls" example.'
	]).

	cover(parent).
	cover(prototype).
	cover(top).
	cover(middle).
	cover(bottom).

	test(super_calls_01) :-
		parent::get_local(Local),
		Local == parent.

	test(super_calls_02) :-
		prototype::get_local(Local),
		Local == prototype.

	test(super_calls_03) :-
		prototype::correct(Local),
		Local == prototype.

	test(super_calls_04) :-
		prototype::wrong(Local),
		Local == parent.

	test(super_calls_05) :-
		bottom::value(Value),
		Value == parent.

	test(super_calls_06) :-
		middle::assertz(d(middle)),
		bottom::value(Value),
		Value == middle.

	test(super_calls_07) :-
		middle::retractall(d(_)),
		bottom::value(Value),
		Value == parent.

:- end_object.
