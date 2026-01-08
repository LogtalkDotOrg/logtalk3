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
		date is 2024-01-11,
		comment is 'Unit tests for the "haunted_wasteland" example.'
	]).

	cover(haunted_wasteland).

	% Part 1

	test(haunted_wasteland_steps_1_sample_1_file, true(Steps == 2)) :-
		^^file_path('test_files/sample_1', Path),
		haunted_wasteland::steps_1(Path, Steps).

	test(haunted_wasteland_steps_1_sample_2_file, true(Steps == 6)) :-
		^^file_path('test_files/sample_2', Path),
		haunted_wasteland::steps_1(Path, Steps).

	% Part 2

	test(haunted_wasteland_steps_2_sample_3_file, true(Steps == 6)) :-
		^^file_path('test_files/sample_3', Path),
		haunted_wasteland::steps_2(Path, Steps).

:- end_object.
