:- encoding('UTF-8').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:2:0,
		author is 'Paulo Moura',
		date is 2021-04-28,
		comment is 'Unit tests for Prolog Unicode support.'
	]).

	% UTF-8 with BOM tests

	test(lgt_unicode_utf_8_bom_01, true) :-
		file_path('utf_8_bom.lgt', Path),
		logtalk_compile(Path).

	test(lgt_unicode_utf_8_bom_02, true) :-
		file_path('utf_8_bom.lgt', Path),
		logtalk_load(Path, [clean(on)]).

	test(lgt_unicode_utf_8_bom_03, true(Lengths == [15,12,12,8,8,13,10,18,5,8,10])) :-
		findall(Length, ({utf_8_bom(Atom)}, atom_length(Atom, Length)), Lengths).

	% UTF-8 without BOM tests

	test(lgt_unicode_utf_8_no_bom_01, true, [condition(set_encoding('UTF-8'))]) :-
		file_path('utf_8_no_bom.lgt', Path),
		logtalk_compile(Path).

	test(lgt_unicode_utf_8_no_bom_02, true, [condition(set_encoding('UTF-8'))]) :-
		file_path('utf_8_no_bom.lgt', Path),
		logtalk_load(Path, [clean(on)]).

	test(lgt_unicode_utf_8_no_bom_03, true(Lengths == [15,12,12,8,8,13,10,18,5,8,10]), [condition(set_encoding('UTF-8'))]) :-
		findall(Length, ({utf_8_no_bom(Atom)}, atom_length(Atom, Length)), Lengths).

	% UTF-16BE with BOM tests

	test(lgt_unicode_utf_16_be_bom_01, true) :-
		file_path('utf_16_be_bom.lgt', Path),
		logtalk_compile(Path).

	test(lgt_unicode_utf_16_be_bom_02, true) :-
		file_path('utf_16_be_bom.lgt', Path),
		logtalk_load(Path, [clean(on)]).

	test(lgt_unicode_utf_16_be_bom_03, true(Lengths == [15,12,12,8,8,13,10,18,5,8,10])) :-
		findall(Length, ({utf_16_be_bom(Atom)}, atom_length(Atom, Length)), Lengths).

	% UTF-16BE without BOM tests

	test(lgt_unicode_utf_16_be_no_bom_01, true, [condition(set_encoding('UTF-16BE'))]) :-
		file_path('utf_16_be_no_bom.lgt', Path),
		logtalk_compile(Path).

	test(lgt_unicode_utf_16_be_no_bom_02, true, [condition(set_encoding('UTF-16BE'))]) :-
		file_path('utf_16_be_no_bom.lgt', Path),
		logtalk_load(Path, [clean(on)]).

	test(lgt_unicode_utf_16_be_no_bom_03, true(Lengths == [15,12,12,8,8,13,10,18,5,8,10]), [condition(set_encoding('UTF-16BE'))]) :-
		findall(Length, ({utf_16_be_no_bom(Atom)}, atom_length(Atom, Length)), Lengths).

	% UTF-16LE with BOM tests

	test(lgt_unicode_utf_16_le_bom_01, true) :-
		file_path('utf_16_le_bom.lgt', Path),
		logtalk_compile(Path).

	test(lgt_unicode_utf_16_le_bom_02, true) :-
		file_path('utf_16_le_bom.lgt', Path),
		logtalk_load(Path, [clean(on)]).

	test(lgt_unicode_utf_16_le_bom_03, true(Lengths == [15,12,12,8,8,13,10,18,5,8,10])) :-
		findall(Length, ({utf_16_le_bom(Atom)}, atom_length(Atom, Length)), Lengths).

	% UTF-16LE without BOM tests

	test(lgt_unicode_utf_16_le_no_bom_01, true, [condition(set_encoding('UTF-16LE'))]) :-
		file_path('utf_16_le_no_bom.lgt', Path),
		logtalk_compile(Path).

	test(lgt_unicode_utf_16_le_no_bom_02, true, [condition(set_encoding('UTF-16LE'))]) :-
		file_path('utf_16_le_no_bom.lgt', Path),
		logtalk_load(Path, [clean(on)]).

	test(lgt_unicode_utf_16_le_no_bom_03, true(Lengths == [15,12,12,8,8,13,10,18,5,8,10]), [condition(set_encoding('UTF-16LE'))]) :-
		findall(Length, ({utf_16_le_no_bom(Atom)}, atom_length(Atom, Length)), Lengths).

	% UTF-32BE with BOM tests

	test(lgt_unicode_utf_32_be_bom_01, true) :-
		file_path('utf_32_be_bom.lgt', Path),
		logtalk_compile(Path).

	test(lgt_unicode_utf_32_be_bom_02, true) :-
		file_path('utf_32_be_bom.lgt', Path),
		logtalk_load(Path, [clean(on)]).

	test(lgt_unicode_utf_32_be_bom_03, true(Lengths == [15,12,12,8,8,13,10,18,5,8,10])) :-
		findall(Length, ({utf_32_be_bom(Atom)}, atom_length(Atom, Length)), Lengths).

	% UTF-32BE without BOM tests

	test(lgt_unicode_utf_32_be_no_bom_01, true, [condition(set_encoding('UTF-32BE'))]) :-
		file_path('utf_32_be_no_bom.lgt', Path),
		logtalk_compile(Path).

	test(lgt_unicode_utf_32_be_no_bom_02, true, [condition(set_encoding('UTF-32BE'))]) :-
		file_path('utf_32_be_no_bom.lgt', Path),
		logtalk_load(Path, [clean(on)]).

	test(lgt_unicode_utf_32_be_no_bom_03, true(Lengths == [15,12,12,8,8,13,10,18,5,8,10]), [condition(set_encoding('UTF-32BE'))]) :-
		findall(Length, ({utf_32_be_no_bom(Atom)}, atom_length(Atom, Length)), Lengths).

	% UTF-32LE with BOM tests

	test(lgt_unicode_utf_32_le_bom_01, true) :-
		file_path('utf_32_le_bom.lgt', Path),
		logtalk_compile(Path).

	test(lgt_unicode_utf_32_le_bom_02, true) :-
		file_path('utf_32_le_bom.lgt', Path),
		logtalk_load(Path, [clean(on)]).

	test(lgt_unicode_utf_32_le_bom_03, true(Lengths == [15,12,12,8,8,13,10,18,5,8,10])) :-
		findall(Length, ({utf_32_le_bom(Atom)}, atom_length(Atom, Length)), Lengths).

	% UTF-32LE without BOM tests

	test(lgt_unicode_utf_32_le_no_bom_01, true, [condition(set_encoding('UTF-32LE'))]) :-
		file_path('utf_32_le_no_bom.lgt', Path),
		logtalk_compile(Path).

	test(lgt_unicode_utf_32_le_no_bom_02, true, [condition(set_encoding('UTF-32LE'))]) :-
		file_path('utf_32_le_no_bom.lgt', Path),
		logtalk_load(Path, [clean(on)]).

	test(lgt_unicode_utf_32_le_no_bom_03, true(Lengths == [15,12,12,8,8,13,10,18,5,8,10]), [condition(set_encoding('UTF-32LE'))]) :-
		findall(Length, ({utf_32_le_no_bom(Atom)}, atom_length(Atom, Length)), Lengths).

	% auxiliary predicates

	file_path(File, Path) :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, File, Path).

	set_encoding(Encoding) :-
		(	current_prolog_flag(encoding, Encoding) ->
			true
		;	catch(set_prolog_flag(encoding, Encoding), _, fail)
		).

:- end_object.
