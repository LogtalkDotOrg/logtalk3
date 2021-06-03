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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2021-06-03,
		comment is 'Unit tests for the "logs" example.'
	]).

	cover(logging).
	cover(object).

	test(logging_01) :-
		object::add_log_entry('something interesting happens'),
		findall(Entry, object::log_entry(_, Entry), Entries),
		Entries == [start, 'something interesting happens'].

	test(logging_02) :-
		object::init_log,
		object::add_log_entry('crickets...'),
		findall(Entry, object::log_entry(_, Entry), Entries),
		Entries == [start, 'crickets...'].

	test(logging_03) :-
		^^suppress_text_output,
		object::init_log,
		object::print_log.

:- end_object.
