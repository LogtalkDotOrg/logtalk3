:- encoding('UTF-8').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:5:0,
		author is 'Paulo Moura',
		date is 2021-03-02,
		comment is 'Unit tests for the "cbor" library (UTF-8 text strings).'
	]).

	:- uses(cbor, [
		parse/2, generate/2
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	cover(cbor).

	condition :-
		current_prolog_flag(bounded, false).

	% UTF-8 text string tests

	test(cbor_text_string_utf_8_01, true(Map == {el-'Γειά σου κόσμε!'})) :-
		generate({el-'Γειά σου κόσμε!'}, Encoding),
		parse(Encoding, Map).

	test(cbor_text_string_utf_8_02, true(Map == {en-'Hello world!'})) :-
		generate({en-'Hello world!'}, Encoding),
		parse(Encoding, Map).

	test(cbor_text_string_utf_8_03, true(Map == {es-'¡Hola mundo!'})) :-
		generate({es-'¡Hola mundo!'}, Encoding),
		parse(Encoding, Map).

	test(cbor_text_string_utf_8_04, true(Map == {ja-'こんにちは世界!'})) :-
		generate({ja-'こんにちは世界!'}, Encoding),
		parse(Encoding, Map).

	test(cbor_text_string_utf_8_05, true(Map == {ko-'여보세요 세계!'})) :-
		generate({ko-'여보세요 세계!'}, Encoding),
		parse(Encoding, Map).

	test(cbor_text_string_utf_8_06, true(Map == {nl-'Hallo wereld!'})) :-
		generate({nl-'Hallo wereld!'}, Encoding),
		parse(Encoding, Map).

	test(cbor_text_string_utf_8_07, true(Map == {pt-'Olá mundo!'})) :-
		generate({pt-'Olá mundo!'}, Encoding),
		parse(Encoding, Map).

	test(cbor_text_string_utf_8_08, true(Map == {ru-'Здравствулте! мир!'})) :-
		generate({ru-'Здравствулте! мир!'}, Encoding),
		parse(Encoding, Map).

	test(cbor_text_string_utf_8_09, true(Map == {zh-'你好世界!'})) :-
		generate({zh-'你好世界!'}, Encoding),
		parse(Encoding, Map).

:- end_object.
