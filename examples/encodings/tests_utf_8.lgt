:- encoding('UTF-8').
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


:- object(tests_utf_8,
	extends(lgtunit)).

	:- info([
		version is 1:3:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2021-04-24,
		comment is 'Unit tests for the "encodings" example.'
	]).

	cover(babel).

	test(encodings_utf_8_01) :-
		findall(Code-Text, babel::hello_world(Code, Text), Solutions),
		^^assertion(Solutions == [el-'Γειά σου κόσμε!', en-'Hello world!', es-'¡Hola mundo!', ja-'こんにちは世界!', ko-'여보세요 세계!', nl-'Hallo wereld!', pt-'Olá mundo!', ru-'Здравствулте! мир!', zh-'你好世界!']).

	test(encodings_utf_8_02) :-
		findall(Length, (babel::hello_world(_, Text), atom_length(Text, Length)), Lengths),
		^^assertion(Lengths == [15, 12, 12, 8, 8, 13, 10, 18, 5]).

:- end_object.
