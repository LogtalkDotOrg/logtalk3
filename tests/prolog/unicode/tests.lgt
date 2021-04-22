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
		version is 0:7:0,
		author is 'Paulo Moura',
		date is 2021-04-22,
		comment is 'Unit tests for Prolog Unicode support.'
	]).

	% atom_chars/2 tests

	test(lgt_unicode_atom_chars_2_01a, true(L == ['Γ','ε','ι','ά',' ','σ','ο','υ',' ','κ','ό','σ','μ','ε','!'])) :-
		{atom_chars('Γειά σου κόσμε!', L)}.

	test(lgt_unicode_atom_chars_2_01b, true(A == 'Γειά σου κόσμε!')) :-
		{atom_chars(A, ['Γ','ε','ι','ά',' ','σ','ο','υ',' ','κ','ό','σ','μ','ε','!'])}.

	test(lgt_unicode_atom_chars_2_02a, true(L == ['¡','H','o','l','a',' ','m','u','n','d','o','!'])) :-
		{atom_chars('¡Hola mundo!', L)}.

	test(lgt_unicode_atom_chars_2_02b, true(A == '¡Hola mundo!')) :-
		{atom_chars(A, ['¡','H','o','l','a',' ','m','u','n','d','o','!'])}.

	test(lgt_unicode_atom_chars_2_03a, true(L == ['こ','ん','に','ち','は','世','界','!'])) :-
		{atom_chars('こんにちは世界!', L)}.

	test(lgt_unicode_atom_chars_2_03b, true(A == 'こんにちは世界!')) :-
		{atom_chars(A, ['こ','ん','に','ち','は','世','界','!'])}.

	test(lgt_unicode_atom_chars_2_04a, true(L == ['여','보','세','요',' ','세','계','!'])) :-
		{atom_chars('여보세요 세계!', L)}.

	test(lgt_unicode_atom_chars_2_04b, true(A == '여보세요 세계!')) :-
		{atom_chars(A, ['여','보','세','요',' ','세','계','!'])}.

	test(lgt_unicode_atom_chars_2_05a, true(L == ['O','l','á',' ','m','u','n','d','o','!'])) :-
		{atom_chars('Olá mundo!', L)}.

	test(lgt_unicode_atom_chars_2_05b, true(A == 'Olá mundo!')) :-
		{atom_chars(A, ['O','l','á',' ','m','u','n','d','o','!'])}.

	test(lgt_unicode_atom_chars_2_06a, true(L == ['З','д','р','а','в','с','т','в','у','л','т','е','!',' ','м','и','р','!'])) :-
		{atom_chars('Здравствулте! мир!', L)}.

	test(lgt_unicode_atom_chars_2_06b, true(A == 'Здравствулте! мир!')) :-
		{atom_chars(A, ['З','д','р','а','в','с','т','в','у','л','т','е','!',' ','м','и','р','!'])}.

	test(lgt_unicode_atom_chars_2_07a, true(L == ['你','好','世','界','!'])) :-
		{atom_chars('你好世界!', L)}.

	test(lgt_unicode_atom_chars_2_07b, true(A == '你好世界!')) :-
		{atom_chars(A, ['你','好','世','界','!'])}.

	test(sics_unicode_atom_chars_2_14, true(L == ['P','é','c','s'])) :-
		{atom_chars('Pécs', L)}.

	test(sics_unicode_atom_chars_2_15, true(A == 'Pécs')) :-
		{atom_chars(A, ['P','é','c','s'])}.

	% atom_codes/2 tests

	test(sics_unicode_atom_codes_2_01a, true(L == [0'Γ,0'ε,0'ι,0'ά,32,0'σ,0'ο,0'υ,32,0'κ,0'ό,0'σ,0'μ,0'ε,0'!])) :-
		{atom_codes('Γειά σου κόσμε!', L)}.

	test(sics_unicode_atom_codes_2_01b, true(A == 'Γειά σου κόσμε!')) :-
		{atom_codes(A, [0'Γ,0'ε,0'ι,0'ά,32,0'σ,0'ο,0'υ,32,0'κ,0'ό,0'σ,0'μ,0'ε,0'!])}.

	test(sics_unicode_atom_codes_2_02a, true(L == [0'¡,0'H,0'o,0'l,0'a,32,0'm,0'u,0'n,0'd,0'o,0'!])) :-
		{atom_codes('¡Hola mundo!', L)}.

	test(sics_unicode_atom_codes_2_02b, true(A == '¡Hola mundo!')) :-
		{atom_codes(A, [0'¡,0'H,0'o,0'l,0'a,32,0'm,0'u,0'n,0'd,0'o,0'!])}.

	test(sics_unicode_atom_codes_2_03a, true(L == [0'こ,0'ん,0'に,0'ち,0'は,0'世,0'界,0'!])) :-
		{atom_codes('こんにちは世界!', L)}.

	test(sics_unicode_atom_codes_2_03b, true(A == 'こんにちは世界!')) :-
		{atom_codes(A, [0'こ,0'ん,0'に,0'ち,0'は,0'世,0'界,0'!])}.

	test(sics_unicode_atom_codes_2_04a, true(L == [0'여,0'보,0'세,0'요,32,0'세,0'계,0'!])) :-
		{atom_codes('여보세요 세계!', L)}.

	test(sics_unicode_atom_codes_2_04b, true(A == '여보세요 세계!')) :-
		{atom_codes(A, [0'여,0'보,0'세,0'요,32,0'세,0'계,0'!])}.

	test(sics_unicode_atom_codes_2_05a, true(L == [0'O,0'l,0'á,32,0'm,0'u,0'n,0'd,0'o,0'!])) :-
		{atom_codes('Olá mundo!', L)}.

	test(sics_unicode_atom_codes_2_05b, true(A == 'Olá mundo!')) :-
		{atom_codes(A, [0'O,0'l,0'á,32,0'm,0'u,0'n,0'd,0'o,0'!])}.

	test(sics_unicode_atom_codes_2_06a, true(L == [0'З,0'д,0'р,0'а,0'в,0'с,0'т,0'в,0'у,0'л,0'т,0'е,0'!,32,0'м,0'и,0'р,0'!])) :-
		{atom_codes('Здравствулте! мир!', L)}.

	test(sics_unicode_atom_codes_2_06b, true(A == 'Здравствулте! мир!')) :-
		{atom_codes(A, [0'З,0'д,0'р,0'а,0'в,0'с,0'т,0'в,0'у,0'л,0'т,0'е,0'!,32,0'м,0'и,0'р,0'!])}.

	test(sics_unicode_atom_codes_2_07a, true(L == [0'你,0'好,0'世,0'界,0'!])) :-
		{atom_codes('你好世界!', L)}.

	test(sics_unicode_atom_codes_2_07b, true(A == '你好世界!')) :-
		{atom_codes(A, [0'你,0'好,0'世,0'界,0'!])}.

	test(sics_unicode_atom_codes_2_12, true(C == [0'P,0'é,0'c,0's])) :-
		{atom_codes('Pécs', C)}.

	test(sics_unicode_atom_codes_2_13, true(A == 'Pécs')) :-
		{atom_codes(A, [0'P,0'é,0'c,0's])}.

	% atom_concat/3 tests

	test(sics_unicode_atom_concat_3_11, true(N == 'Bartók Béla')) :-
		{atom_concat('Bartók ', 'Béla', N)}.

	test(sics_unicode_atom_concat_3_12, true(N == 'Bartók ')) :-
		{atom_concat(N, 'Béla', 'Bartók Béla')}.

	test(sics_unicode_atom_concat_3_13, true(N == 'Béla')) :-
		{atom_concat('Bartók ', N, 'Bartók Béla')}.

	test(sics_unicode_atom_concat_3_14, true(L == [''-'Pécs', 'P'-'écs', 'Pé'-'cs', 'Péc'-'s', 'Pécs'-''])) :-
		findall(T1-T2, {atom_concat(T1, T2, 'Pécs')}, L).

	% atom_length/2 tests

	test(lgt_unicode_atom_length_2_01, true(N == 15)) :-
		{atom_length('Γειά σου κόσμε!', N)}.

	test(lgt_unicode_atom_length_2_02, true(N == 12)) :-
		{atom_length('¡Hola mundo!', N)}.

	test(lgt_unicode_atom_length_2_03, true(N == 8)) :-
		{atom_length('こんにちは世界!', N)}.

	test(lgt_unicode_atom_length_2_04, true(N == 8)) :-
		{atom_length('여보세요 세계!', N)}.

	test(lgt_unicode_atom_length_2_05, true(N == 10)) :-
		{atom_length('Olá mundo!', N)}.

	test(lgt_unicode_atom_length_2_06, true(N == 18)) :-
		{atom_length('Здравствулте! мир!', N)}.

	test(lgt_unicode_atom_length_2_07, true(N == 5)) :-
		{atom_length('你好世界!', N)}.

	test(sics_unicode_atom_length_2_09, true(N == 11)) :-
		{atom_length('Bartók Béla', N)}.

	test(lgt_unicode_atom_length_2_10, true(N == 4)) :-
		{atom_length('Pécs', N)}.

	% char_code/2 tests

	test(lgt_unicode_char_code_2_01, true(Code == 243)) :-
		{char_code('ó', Code)}.

	test(lgt_unicode_char_code_2_02, true(Code == 233)) :-
		{char_code('é', Code)}.

	test(lgt_unicode_char_code_2_03, true(Char == 'Γ')) :-
		{char_code(Char, 915)}.

	test(lgt_unicode_char_code_2_04, true(Char == '¡')) :-
		{char_code(Char, 161)}.

	test(lgt_unicode_char_code_2_05, true(Char == 'こ')) :-
		{char_code(Char, 12371)}.

	test(lgt_unicode_char_code_2_06, true(Char == '여')) :-
		{char_code(Char, 50668)}.

	test(lgt_unicode_char_code_2_07, true(Char == 'á')) :-
		{char_code(Char, 225)}.

	test(lgt_unicode_char_code_2_08, true(Char == 'З')) :-
		{char_code(Char, 1047)}.

	test(lgt_unicode_char_code_2_09, true(Char == '你')) :-
		{char_code(Char, 20320)}.

	% current_prolog_flag/2

	test(lgt_unicode_current_prolog_flag_2_01, true(atom(Encoding))) :-
		{current_prolog_flag(encoding, Encoding)}.

	test(lgt_unicode_current_prolog_flag_2_02, true(valid(Encoding))) :-
		{current_prolog_flag(encoding, Encoding)}.

	% get_byte/2 tests

	% check that the BOM is not skipped when opening a binary file for reading
	test(lgt_unicode_get_byte_2_01, true(Byte == 239)) :-
		file_path(sample_utf_8_bom, Path),
		open(Path, write, Output, [type(binary)]),
		% UTF-8 is represented by the bytes 0xEF 0xBB 0xBF
		put_byte(Output, 239), put_byte(Output, 187), put_byte(Output, 191),
		close(Output),
		open(Path, read, Input, [type(binary)]),
		{get_byte(Input, Byte)}.

	% get_char/2 tests

	test(lgt_unicode_get_char_2_01a, true(Char == 'Γ')) :-
		^^set_text_input(st_i, 'Γειά σου κόσμε!', [encoding('UTF-8')]),
		{get_char(st_i, Char)}.

	test(lgt_unicode_get_char_2_01b, true(Assertion)) :-
		^^set_text_input(st_i, 'Γειά σου κόσμε!', [encoding('UTF-8')]),
		{get_char(st_i, _)},
		^^text_input_assertion(st_i, 'ειά σου κόσμε!', Assertion).

	test(lgt_unicode_get_char_2_02a, true(Char == '你')) :-
		^^set_text_input(st_i, '你好世界!', [encoding('UTF-8')]),
		{get_char(st_i, Char)}.

	test(lgt_unicode_get_char_2_02b, true(Assertion)) :-
		^^set_text_input(st_i, '你好世界!', [encoding('UTF-8')]),
		{get_char(st_i, _)},
		^^text_input_assertion(st_i, '好世界!', Assertion).

	% check that the BOM is skipped when opening a text file for reading
	test(lgt_unicode_get_char_2_03, true(Char == a)) :-
		file_path(sample_utf_8_bom, Path),
		open(Path, write, Output, [type(binary)]),
		% UTF-8 is represented by the bytes 0xEF 0xBB 0xBF
		put_byte(Output, 239), put_byte(Output, 187), put_byte(Output, 191),
		% abc
		put_byte(Output, 97), put_byte(Output, 98), put_byte(Output, 99),
		close(Output),
		open(Path, read, Input),
		{get_char(Input, Char)}.

	% get_code/2 tests

	test(lgt_unicode_get_code_2_01a, true(Code == 915)) :-
		^^set_text_input(st_i, 'Γειά σου κόσμε!', [encoding('UTF-8')]),
		{get_code(st_i, Code)}.

	test(lgt_unicode_get_code_2_01b, true(Assertion)) :-
		^^set_text_input(st_i, 'Γειά σου κόσμε!', [encoding('UTF-8')]),
		{get_code(st_i, _)},
		^^text_input_assertion(st_i, 'ειά σου κόσμε!', Assertion).

	test(lgt_unicode_get_code_2_02a, true(Code == 20320)) :-
		^^set_text_input(st_i, '你好世界!', [encoding('UTF-8')]),
		{get_code(st_i, Code)}.

	test(lgt_unicode_get_code_2_02b, true(Assertion)) :-
		^^set_text_input(st_i, '你好世界!', [encoding('UTF-8')]),
		{get_code(st_i, _)},
		^^text_input_assertion(st_i, '好世界!', Assertion).

	% open/4 tests

	% check that the encoding/1 option is accepted
	test(lgt_unicode_open_4_01, true) :-
		file_path(sample_utf_8, Path),
		{open(Path, write, Stream, [encoding('UTF-8')])},
		close(Stream).

	% check that a bom(false) option is accepted
	test(lgt_unicode_open_4_02, true) :-
		file_path(sample_utf_8_no_bom, Path),
		{open(Path, write, Stream, [encoding('UTF-8'), bom(false)])},
		close(Stream).

	% check that a bom(true) option is accepted
	test(lgt_unicode_open_4_03, true) :-
		file_path(sample_utf_8_bom, Path),
		{open(Path, write, Stream, [encoding('UTF-8'), bom(true)])},
		close(Stream).

	% always write a BOM if requested, including for binary files
	test(lgt_unicode_open_4_04, true(Byte == 239)) :-
		file_path(sample_utf_8_bom, Path),
		open(Path, write, Output, [type(binary), bom(true)]),
		close(Output),
		open(Path, read, Input, [type(binary)]),
		{get_byte(Input, Byte)}.

	% don't write a BOM unless explicitly requested
	test(lgt_unicode_open_4_05, true(Byte == 97)) :-
		file_path(sample_utf_8_bom, Path),
		open(Path, write, Output, [type(text), encoding('UTF-8')]),
		write(Output, abc),
		close(Output),
		open(Path, read, Input, [type(binary)]),
		{get_byte(Input, Byte)}.

	% peek_char/2 tests

	test(lgt_unicode_peek_char_2_01, true(Char == 'Γ')) :-
		^^set_text_input(st_i, 'Γειά σου κόσμε', [encoding('UTF-8')]),
		{peek_char(st_i, Char)}.

	test(lgt_unicode_peek_char_2_02, true(Char == '你')) :-
		^^set_text_input(st_i, '你好世界', [encoding('UTF-8')]),
		{peek_char(st_i, Char)}.

	% peek_code/2 tests

	test(lgt_unicode_peek_code_2_01, true(Code == 915)) :-
		^^set_text_input(st_i, 'Γειά σου κόσμε', [encoding('UTF-8')]),
		{peek_code(st_i, Code)}.

	test(lgt_unicode_peek_code_2_02, true(Code == 20320)) :-
		^^set_text_input(st_i, '你好世界', [encoding('UTF-8')]),
		{peek_code(st_i, Code)}.

	% put_char/2 tests

	test(lgt_unicode_put_char_2_01, true(Assertion)) :-
		^^set_text_output(st_o, 'Γειά σου κόσμ', [encoding('UTF-8')]),
		{put_char(st_o, 'ε')},
		^^text_output_assertion(st_o, 'Γειά σου κόσμε', [encoding('UTF-8')], Assertion).

	test(lgt_unicode_put_char_2_02, true(Assertion)) :-
		^^set_text_output(st_o, '你好世', [encoding('UTF-8')]),
		{put_char(st_o, '界')},
		^^text_output_assertion(st_o,  '你好世界', [encoding('UTF-8')], Assertion).

	% put_code/2 tests

	test(lgt_unicode_put_code_2_01, true(Assertion)) :-
		^^set_text_output(st_o, 'Γειά σου κόσμ', [encoding('UTF-8')]),
		{put_code(st_o, 949)},
		^^text_output_assertion(st_o, 'Γειά σου κόσμε', [encoding('UTF-8')], Assertion).

	test(lgt_unicode_put_code_2_02, true(Assertion)) :-
		^^set_text_output(st_o, '你好世', [encoding('UTF-8')]),
		{put_code(st_o, 30028)},
		^^text_output_assertion(st_o,  '你好世界', [encoding('UTF-8')], Assertion).

	% read_term/3 tests

	test(lgt_unicode_read_term_3_01, true(Term == 'Γειά σου κόσμε!')) :-
		^^set_text_input(st_i, '\'Γειά σου κόσμε!\'.', [encoding('UTF-8')]),
		{read_term(st_i, Term, [])}.

	test(lgt_unicode_read_term_3_02, true(Term == '你好世界!')) :-
		^^set_text_input(st_i, '\'你好世界!\'.', [encoding('UTF-8')]),
		{read_term(st_i, Term, [])}.

	% set_prolog_flag/2

	% if the flag is not read-only, setting its value to its current value must succeed
	test(lgt_unicode_set_prolog_flag_2_01, true) :-
		{current_prolog_flag(encoding, Encoding),
		 catch(set_prolog_flag(encoding, Encoding), _, true)}.

	% set_stream_position/2

	% check that BOM detection doesn't break stream position
	test(lgt_unicode_set_stream_position_2_01, true(Term1 == Term3)) :-
		os::absolute_file_name('terms.pl', Path),
		open(Path, read, Stream, [type(text), reposition(true)]),
		stream_property(Stream, position(Position)),
		read_term(Stream, Term1, []),
		read_term(Stream, _, []),
		{set_stream_position(Stream, Position)},
		read_term(Stream, Term3, []).

	% stream_property/2

	test(lgt_unicode_stream_property_2_01, true(Encoding-BOM == 'UTF-8'-false)) :-
		file_path(sample_utf_8, Path),
		open(Path, write, Stream, [encoding('UTF-8')]),
		{stream_property(Stream, encoding(Encoding)),
		 stream_property(Stream, bom(BOM))},
		close(Stream).

	test(lgt_unicode_stream_property_2_02, true(Encoding-BOM == 'UTF-8'-false)) :-
		file_path(sample_utf_8_no_bom, Path),
		open(Path, write, Stream, [encoding('UTF-8'), bom(false)]),
		{stream_property(Stream, encoding(Encoding)),
		 stream_property(Stream, bom(BOM))},
		close(Stream).

	test(lgt_unicode_stream_property_2_03, true(Encoding-BOM == 'UTF-8'-true)) :-
		file_path(sample_utf_8_bom, Path),
		open(Path, write, Stream, [encoding('UTF-8'), bom(true)]),
		{stream_property(Stream, encoding(Encoding)),
		 stream_property(Stream, bom(BOM))},
		close(Stream).

	test(lgt_unicode_stream_property_2_04, true(Encoding == 'UTF-8')) :-
		file_path('tests.lgt', Path),
		open(Path, read, Stream, [encoding('UTF-8')]),
		{stream_property(Stream, encoding(Encoding))},
		close(Stream).

	test(lgt_unicode_stream_property_2_05, true(BOM == false)) :-
		file_path('tests.lgt', Path),
		open(Path, read, Stream, [encoding('UTF-8')]),
		{stream_property(Stream, bom(BOM))},
		close(Stream).

	test(lgt_unicode_stream_property_2_06, true(Encoding-BOM == 'UTF-8'-true)) :-
		file_path(sample_utf_8_bom, Path),
		open(Path, write, Output, [encoding('UTF-8'), bom(true)]),
		close(Output),
		open(Path, read, Input),
		{stream_property(Input, encoding(Encoding)),
		 stream_property(Input, bom(BOM))},
		close(Input).

	% sub_atom/5 tests

	test(lgt_unicode_sub_atom_5_01, true(L == [0-13-'Γε',1-12-'ει',2-11-'ιά',3-10-'ά ',4-9-' σ',5-8-'σο',6-7-'ου',7-6-'υ ',8-5-' κ',9-4-'κό',10-3-'όσ',11-2-'σμ',12-1-'με',13-0-'ε!'])) :-
		findall(B-A-S, {sub_atom('Γειά σου κόσμε!',B,2,A,S)}, L).

	test(lgt_unicode_sub_atom_5_02, true(L == [0-10-'¡H',1-9-'Ho',2-8-'ol',3-7-'la',4-6-'a ',5-5-' m',6-4-'mu',7-3-'un',8-2-'nd',9-1-'do',10-0-'o!'])) :-
		findall(B-A-S, {sub_atom('¡Hola mundo!',B,2,A,S)}, L).

	test(lgt_unicode_sub_atom_5_03, true(L == [0-6-'こん',1-5-'んに',2-4-'にち',3-3-'ちは',4-2-'は世',5-1-'世界',6-0-'界!'])) :-
		findall(B-A-S, {sub_atom('こんにちは世界!',B,2,A,S)}, L).

	test(lgt_unicode_sub_atom_5_04, true(L == [0-6-'여보',1-5-'보세',2-4-'세요',3-3-'요 ',4-2-' 세',5-1-'세계',6-0-'계!'])) :-
		findall(B-A-S, {sub_atom('여보세요 세계!',B,2,A,S)}, L).

	test(lgt_unicode_sub_atom_5_05, true(L == [0-8-'Ol',1-7-'lá',2-6-'á ',3-5-' m',4-4-'mu',5-3-'un',6-2-'nd',7-1-'do',8-0-'o!'])) :-
		findall(B-A-S, {sub_atom('Olá mundo!',B,2,A,S)}, L).

	test(lgt_unicode_sub_atom_5_06, true(L == [0-16-'Зд',1-15-'др',2-14-'ра',3-13-'ав',4-12-'вс',5-11-'ст',6-10-'тв',7-9-'ву',8-8-'ул',9-7-'лт',10-6-'те',11-5-'е!',12-4-'! ',13-3-' м',14-2-'ми',15-1-'ир',16-0-'р!'])) :-
		findall(B-A-S, {sub_atom('Здравствулте! мир!',B,2,A,S)}, L).

	test(lgt_unicode_sub_atom_5_07, true(L == [0-3-'你好',1-2-'好世',2-1-'世界',3-0-'界!'])) :-
		findall(B-A-S, {sub_atom('你好世界!',B,2,A,S)}, L).

	test(sics_unicode_sub_atom_5_31, true(A-S == 5-'ók')) :-
		{sub_atom('Bartók Béla', 4, 2, A, S)}.

	test(sics_unicode_sub_atom_5_32, true(L-S == 2-'ók')) :-
		{sub_atom('Bartók Béla', 4, L, 5, S)}.

	test(sics_unicode_sub_atom_5_33, true(B-S == 4-'ók')) :-
		{sub_atom('Bartók Béla', B, 2, 5, S)}.

	test(sics_unicode_sub_atom_5_34, true(L == [0-2-'Pé', 1-1-'éc', 2-0-'cs'])) :-
		findall(B-A-S, {sub_atom('Pécs',B,2,A,S)}, L).

	test(sics_unicode_sub_atom_5_35, true(L == [0-4-7, 7-4-0])) :-
		findall(B-L-A, {sub_atom(abracadabra,B,L,A,abra)}, L).

	% write_term/3 tests

	test(lgt_unicode_write_term_3_01, true(Assertion)) :-
		^^set_text_output(st_o, '', [encoding('UTF-8')]),
		{write_term(st_o, 'Γειά σου κόσμε!', [])},
		^^text_output_assertion(st_o, 'Γειά σου κόσμε!', [encoding('UTF-8')], Assertion).

	test(lgt_unicode_write_term_3_02, true(Assertion)) :-
		^^set_text_output(st_o, '', [encoding('UTF-8')]),
		{write_term(st_o, '你好世界!', [])},
		^^text_output_assertion(st_o, '你好世界!', [encoding('UTF-8')], Assertion).

	cleanup :-
		^^clean_text_input,
		^^clean_text_output,
		file_path(sample_utf_8, Path1),
		catch(ignore(os::delete_file(Path1)), _, true),
		file_path(sample_utf_8_bom, Path2),
		catch(ignore(os::delete_file(Path2)), _, true),
		file_path(sample_utf_8_no_bom, Path3),
		catch(ignore(os::delete_file(Path3)), _, true).

	% auxiliary predicates

	file_path(File, Path) :-
		this(Object),
		object_property(Object, file(_,Directory)),
		atom_concat(Directory, File, Path).

	% partial list of valid encodings (from http://www.iana.org/assignments/character-sets)
	valid('US-ASCII').
	valid('UTF-8').
	valid('UTF-16BE').
	valid('UTF-16LE').
	valid('UTF-16').
	valid('UTF-32BE').
	valid('UTF-32LE').
	valid('UTF-32').
	valid('ISO-8859-1').
	valid('ISO-8859-2').
	valid('ISO-8859-3').
	valid('ISO-8859-4').
	valid('ISO-8859-5').
	valid('ISO-8859-6').
	valid('ISO-8859-7').
	valid('ISO-8859-8').
	valid('ISO-8859-9').
	valid('ISO-8859-10').

:- end_object.
