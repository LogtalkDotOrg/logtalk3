%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		comment is 'Unit tests for the "threads/checkpoint" example.'
	]).

	cover(checkpoint).

	:- if(current_logtalk_flag(prolog_dialect, lvm)).

		test(checkpoint_1, true) :-
			checkpoint::run.

	:- else.

		test(checkpoint_1, true) :-
			^^set_text_output(''),
			checkpoint::run,
			^^text_output_contents(Chars),
			atom_chars(Atom, Chars),
			atom::split(Atom, '\n', Lines),
			list::msort(Lines, Sorted),
			^^assertion(Sorted == [
				'',
				'All assemblies done.',
				'Assembly of item 1 done.',
				'Assembly of item 2 done.',
				'Assembly of item 3 done.',
				'Worker 1 item 1',
				'Worker 1 item 2',
				'Worker 1 item 3',
				'Worker 2 item 1',
				'Worker 2 item 2',
				'Worker 2 item 3',
				'Worker 3 item 1',
				'Worker 3 item 2',
				'Worker 3 item 3',
				'Worker 4 item 1',
				'Worker 4 item 2',
				'Worker 4 item 3',
				'Worker 5 item 1',
				'Worker 5 item 2',
				'Worker 5 item 3'
			]).

	:- endif.

:- end_object.
