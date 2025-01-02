%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		date is 2024-02-06,
		comment is 'Unit tests for the "threads/blackboard" example.'
	]).

	:- threaded.

	:- if(current_logtalk_flag(prolog_dialect, xvm)).

		test(blackboard_1, true) :-
			threaded_call(student::run(10)),
			threaded_call(teacher::run(4)),
			threaded_exit(teacher::run(4)),
			threaded_exit(student::run(10)).

	:- else.

		test(blackboard_1, true) :-
			^^set_text_output(''),
			threaded_call(student::run(10)),
			threaded_call(teacher::run(4)),
			threaded_exit(teacher::run(4)),
			threaded_exit(student::run(10)),
			^^text_output_contents(Chars),
			atom_chars(Atom, Chars),
			atom::split(Atom, '\n', Lines),
			list::msort(Lines, Sorted),
			^^assertion(Sorted == [
				'',
				'student is writing...',
				'student is writing...',
				'student is writing...',
				'student is writing...',
				'student is writing...',
				'student is writing...',
				'student is writing...',
				'student is writing...',
				'student is writing...',
				'student is writing...',
				'teacher is writing...',
				'teacher is writing...',
				'teacher is writing...',
				'teacher is writing...'
			]).

	:- endif.

:- end_object.
