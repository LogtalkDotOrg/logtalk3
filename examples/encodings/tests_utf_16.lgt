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


:- object(tests_utf_16,
	extends(lgtunit)).

	:- info([
		version is 1:2:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2021-04-24,
		comment is 'Unit tests for the "encodings" example.'
	]).

	cover(asian).

	test(encodings_utf_16_01) :-
		findall(Name-Capital-Country, asian::country(Country, Name, Capital), Solutions),
		^^assertion(Solutions == ['中国'-'北京'-china, '日本'-'東京'-japan, 'Монгол Улс'-'Улаанбатаар'-mongolia, '臺灣'-'臺北'-taiwan, 'Тоҷикистон'-'Душанбе'-tajikistan]).

	test(encodings_utf_16_02) :-
		findall(Length, (asian::country(_, Name, _), atom_length(Name, Length)), Lengths),
		^^assertion(Lengths == [2, 2, 10, 2, 10]).

:- end_object.
