%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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
		date is 2024-02-06,
		comment is 'Unit tests for the "threads/metered_concurrency" example.'
	]).

	cover(metered_concurrency).

	:- if(current_logtalk_flag(prolog_dialect, xvm)).

		test(metered_concurrency_1, true) :-
			metered_concurrency::run.

	:- else.

		test(metered_concurrency_1, true) :-
			^^set_text_output(''),
			metered_concurrency::run,
			^^text_output_contents(Chars),
			atom_chars(Atom, Chars),
			atom::split(Atom, '\n', Lines),
			sort(Lines, Sorted),
			^^assertion(Sorted == [
				'',
				'Worker 1 acquired semaphore',
				'Worker 1 releasing semaphore',
				'Worker 2 acquired semaphore',
				'Worker 2 releasing semaphore',
				'Worker 3 acquired semaphore',
				'Worker 3 releasing semaphore',
				'Worker 4 acquired semaphore',
				'Worker 4 releasing semaphore',
				'Worker 5 acquired semaphore',
				'Worker 5 releasing semaphore',
				'Worker 6 acquired semaphore',
				'Worker 6 releasing semaphore',
				'Worker 7 acquired semaphore',
				'Worker 7 releasing semaphore'
			]).

	:- endif.

:- end_object.
