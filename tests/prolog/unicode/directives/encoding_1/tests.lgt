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
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2024-03-20,
		comment is 'Unit tests for the Prolog encoding/1 directive.'
	]).

	test(encoding_1_us_ascii, true({us_ascii})) :-
		^^file_path('us_ascii.pl', Path),
		% the '$lgt_load_prolog_file'/1 predicate is defined in the backend
		% adapter files and abstracts how to load a Prolog file
		{'$lgt_load_prolog_file'(Path)}.

	test(encoding_1_utf_8_bom, true({utf_8_bom})) :-
		^^file_path('utf_8_bom.pl', Path),
		% the '$lgt_load_prolog_file'/1 predicate is defined in the backend
		% adapter files and abstracts how to load a Prolog file
		{'$lgt_load_prolog_file'(Path)}.

	test(encoding_1_utf_8_no_bom, true({utf_8_no_bom})) :-
		^^file_path('utf_8_no_bom.pl', Path),
		% the '$lgt_load_prolog_file'/1 predicate is defined in the backend
		% adapter files and abstracts how to load a Prolog file
		{'$lgt_load_prolog_file'(Path)}.

:- end_object.
