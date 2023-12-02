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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-02-24,
		comment is 'Unit tests for the "trebuchet" example.'
	]).

	cover(trebuchet).

	test(trebuchet_sample_1_file, true(Calibration == 142)) :-
		^^file_path('test_files/sample_1', Path),
		trebuchet::solution(Path, Calibration).

	test(trebuchet_sample_2_file, true(Calibration == 281)) :-
		^^file_path('test_files/sample_2', Path),
		trebuchet::solution(Path, Calibration).

	test(trebuchet_input_file, true(Calibration == 53894)) :-
		^^file_path('test_files/input', Path),
		trebuchet::solution(Path, Calibration).

:- end_object.
