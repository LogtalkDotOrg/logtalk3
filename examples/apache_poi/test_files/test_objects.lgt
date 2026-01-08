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


:- object(sample_spreadsheet_object).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-03-11,
		comment is 'Expected results when extracting the contents of the test spreadsheets.'
	]).

	:- public([
		db/4, db/5
	]).

	db('Test Sheet #1',    0, 0, 4).
	db('Sheet Testing #2', 1, 0, 4).

	db(0, 0, 0, string, strings).
	db(0, 0, 1, string, numbers).
	db(0, 0, 2, string, booleans).
	db(0, 1, 0, string, hello).
	db(0, 1, 1, numeric, 3.141).
	db(0, 1, 2, boolean, true).
	db(0, 2, 0, string, big).
	db(0, 2, 1, numeric, 4.234).
	db(0, 2, 2, boolean, false).
	db(0, 3, 0, string, beautiful).
	db(0, 3, 1, numeric, 5.13).
	db(0, 3, 2, boolean, true).
	db(0, 4, 0, string, world).
	db(0, 4, 1, numeric, 6.9).
	db(0, 4, 2, boolean, false).
	db(1, 0, 0, string, 'STRINGS').
	db(1, 0, 1, string, 'NUMBERS').
	db(1, 0, 2, string, 'BOOLEANS').
	db(1, 1, 0, string, 'HELLO').
	db(1, 1, 1, numeric, 3.141).
	db(1, 1, 2, boolean, true).
	db(1, 2, 0, string, 'BIG').
	db(1, 2, 1, numeric, 4.234).
	db(1, 2, 2, boolean, false).
	db(1, 3, 0, string, 'BEAUTIFUL').
	db(1, 3, 1, numeric, 5.13).
	db(1, 3, 2, boolean, true).
	db(1, 4, 0, string, 'WORLD').
	db(1, 4, 1, numeric, 6.9).
	db(1, 4, 2, boolean, false).

:- end_object.
