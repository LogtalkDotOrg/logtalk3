:- encoding('UTF-16').
% the encoding/1 directive, when present, must be the
% first term, in the first line, of a source file


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


:- object(tests_utf_16_surrogates,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2021-04-27,
		comment is 'Unit tests for the "encodings" example.'
	]).

	cover(mahjong).

	test(encodings_tests_utf_16_surrogates_01, true(Tiles == ['🀙','🀚','🀛','🀜','🀝','🀞','🀟','🀠'])) :-
		findall(Tile, mahjong::tile(Tile), Tiles).

	test(encodings_tests_utf_16_surrogates_02, true(Codes == [0x1F019,0x1F01A,0x1F01B,0x1F01C,0x1F01D,0x1F01E,0x1F01F,0x1F020])) :-
		findall(Code, (mahjong::tile(Tile), char_code(Tile, Code)), Codes).

:- end_object.
