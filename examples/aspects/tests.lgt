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
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2020-12-12,
		comment is 'Unit tests for the "aspects" example.'
	]).

	cover(bank).
	cover(security).
	cover(logging).
	cover(account).

	test(aspects_01, true) :-
		^^suppress_text_output,
		bank::transfer(john, jane, 100),
		^^assertion(john, john::balance(900)),
		^^assertion(jane, jane::balance(1100)).

	test(aspects_02, fail) :-
		^^suppress_text_output,
		bank::transfer(jane, john, 400).

:- end_object.
