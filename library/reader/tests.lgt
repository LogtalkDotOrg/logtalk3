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
		version is 2:3:0,
		author is 'Paulo Moura',
		date is 2022-12-01,
		comment is 'Unit tests for the "reader" library.'
	]).

	cover(reader).

	% file reader predicates

	test(reader_file_to_codes_2_01, true(Codes == [])) :-
		^^file_path('test_files/empty', Path),
		reader::file_to_codes(Path, Codes).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(reader_file_to_codes_2_02, true(Codes == [97,98,99,13,10,100,101,102,13,10,103,104,105,13,10])) :-
			^^file_path('test_files/lines', Path),
			reader::file_to_codes(Path, Codes).

	:- else.

		test(reader_file_to_codes_2_02, true(Codes == [97,98,99,10,100,101,102,10,103,104,105,10])) :-
			^^file_path('test_files/lines', Path),
			reader::file_to_codes(Path, Codes).

	:- endif.

	test(reader_file_to_codes_3_01, true(Codes == [])) :-
		^^file_path('test_files/empty', Path),
		reader::file_to_codes(Path, Codes, []).

	test(reader_file_to_codes_3_02, true(Codes == [65,66,67])) :-
		^^file_path('test_files/empty', Path),
		reader::file_to_codes(Path, Codes, [65,66,67]).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(reader_file_to_codes_3_03, true(Codes == [97,98,99,13,10,100,101,102,13,10,103,104,105,13,10,65,66,67])) :-
			^^file_path('test_files/lines', Path),
			reader::file_to_codes(Path, Codes, [65,66,67]).

	:- else.

		test(reader_file_to_codes_3_03, true(Codes == [97,98,99,10,100,101,102,10,103,104,105,10,65,66,67])) :-
			^^file_path('test_files/lines', Path),
			reader::file_to_codes(Path, Codes, [65,66,67]).

	:- endif.

	test(reader_file_to_chars_2_01, true(Chars == [])) :-
		^^file_path('test_files/empty', Path),
		reader::file_to_chars(Path, Chars).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(reader_file_to_chars_2_02, true(Chars == [a,b,c,'\r','\n',d,e,f,'\r','\n',g,h,i,'\r','\n'])) :-
			^^file_path('test_files/lines', Path),
			reader::file_to_chars(Path, Chars).

	:- else.

		test(reader_file_to_chars_2_02, true(Chars == [a,b,c,'\n',d,e,f,'\n',g,h,i,'\n'])) :-
			^^file_path('test_files/lines', Path),
			reader::file_to_chars(Path, Chars).

	:- endif.

	test(reader_file_to_chars_3_01, true(Chars == [])) :-
		^^file_path('test_files/empty', Path),
		reader::file_to_chars(Path, Chars, []).

	test(reader_file_to_chars_3_02, true(Chars == ['A','B','C'])) :-
		^^file_path('test_files/empty', Path),
		reader::file_to_chars(Path, Chars, ['A','B','C']).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(reader_file_to_chars_3_03, true(Chars == [a,b,c,'\r','\n',d,e,f,'\r','\n',g,h,i,'\r','\n','A','B','C'])) :-
			^^file_path('test_files/lines', Path),
			reader::file_to_chars(Path, Chars, ['A','B','C']).

	:- else.

		test(reader_file_to_chars_3_03, true(Chars == [a,b,c,'\n',d,e,f,'\n',g,h,i,'\n','A','B','C'])) :-
			^^file_path('test_files/lines', Path),
			reader::file_to_chars(Path, Chars, ['A','B','C']).

	:- endif.

	test(reader_file_to_terms_2_01, true(Terms == [])) :-
		^^file_path('test_files/empty', Path),
		reader::file_to_terms(Path, Terms).

	test(reader_file_to_terms_2_02, true(Terms == [a,b,c])) :-
		^^file_path('test_files/terms', Path),
		reader::file_to_terms(Path, Terms).

	test(reader_file_to_terms_3_01, true(Terms == [])) :-
		^^file_path('test_files/empty', Path),
		reader::file_to_terms(Path, Terms, []).

	test(reader_file_to_terms_3_02, true(Terms == [d,e,f])) :-
		^^file_path('test_files/empty', Path),
		reader::file_to_terms(Path, Terms, [d,e,f]).

	test(reader_file_to_terms_3_03, true(Terms == [a,b,c,d,e,f])) :-
		^^file_path('test_files/terms', Path),
		reader::file_to_terms(Path, Terms, [d,e,f]).

	test(reader_file_to_bytes_2_01, true(Bytes == [])) :-
		^^file_path('test_files/empty', Path),
		reader::file_to_bytes(Path, Bytes).

	test(reader_file_to_bytes_2_02, true(Bytes == [65,66,67,68,69,70])) :-
		^^file_path('test_files/bytes', Path),
		reader::file_to_bytes(Path, Bytes).

	test(reader_file_to_bytes_3_01, true(Bytes == [])) :-
		^^file_path('test_files/empty', Path),
		reader::file_to_bytes(Path, Bytes, []).

	test(reader_file_to_bytes_3_02, true(Bytes == [71,72,73])) :-
		^^file_path('test_files/empty', Path),
		reader::file_to_bytes(Path, Bytes, [71,72,73]).

	test(reader_file_to_bytes_3_03, true(Bytes == [65,66,67,68,69,70,71,72,73])) :-
		^^file_path('test_files/bytes', Path),
		reader::file_to_bytes(Path, Bytes, [71,72,73]).

	% stream reader predicates

	test(reader_stream_to_codes_2_01, true(Codes == [])) :-
		text_file_stream('test_files/empty', Stream),
		reader::stream_to_codes(Stream, Codes),
		close(Stream).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(reader_stream_to_codes_2_02, true(Codes == [97,98,99,13,10,100,101,102,13,10,103,104,105,13,10])) :-
			text_file_stream('test_files/lines', Stream),
			reader::stream_to_codes(Stream, Codes),
			close(Stream).

	:- else.

		test(reader_stream_to_codes_2_02, true(Codes == [97,98,99,10,100,101,102,10,103,104,105,10])) :-
			text_file_stream('test_files/lines', Stream),
			reader::stream_to_codes(Stream, Codes),
			close(Stream).

	:- endif.

	test(reader_stream_to_codes_3_01, true(Codes == [])) :-
		text_file_stream('test_files/empty', Stream),
		reader::stream_to_codes(Stream, Codes, []),
		close(Stream).

	test(reader_stream_to_codes_3_02, true(Codes == [65,66,67])) :-
		text_file_stream('test_files/empty', Stream),
		reader::stream_to_codes(Stream, Codes, [65,66,67]),
		close(Stream).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(reader_stream_to_codes_3_03, true(Codes == [97,98,99,13,10,100,101,102,13,10,103,104,105,13,10,65,66,67])) :-
			text_file_stream('test_files/lines', Stream),
			reader::stream_to_codes(Stream, Codes, [65,66,67]),
			close(Stream).

	:- else.

		test(reader_stream_to_codes_3_03, true(Codes == [97,98,99,10,100,101,102,10,103,104,105,10,65,66,67])) :-
			text_file_stream('test_files/lines', Stream),
			reader::stream_to_codes(Stream, Codes, [65,66,67]),
			close(Stream).

	:- endif.

	test(reader_stream_to_chars_2_01, true(Chars == [])) :-
		text_file_stream('test_files/empty', Stream),
		reader::stream_to_chars(Stream, Chars),
		close(Stream).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(reader_stream_to_chars_2_02, true(Chars == [a,b,c,'\r','\n',d,e,f,'\r','\n',g,h,i,'\r','\n'])) :-
			text_file_stream('test_files/lines', Stream),
			reader::stream_to_chars(Stream, Chars),
			close(Stream).

	:- else.

		test(reader_stream_to_chars_2_02, true(Chars == [a,b,c,'\n',d,e,f,'\n',g,h,i,'\n'])) :-
			text_file_stream('test_files/lines', Stream),
			reader::stream_to_chars(Stream, Chars),
			close(Stream).

	:- endif.

	test(reader_stream_to_chars_3_01, true(Chars == [])) :-
		text_file_stream('test_files/empty', Stream),
		reader::stream_to_chars(Stream, Chars, []),
		close(Stream).

	test(reader_stream_to_chars_3_02, true(Chars == ['A','B','C'])) :-
		text_file_stream('test_files/empty', Stream),
		reader::stream_to_chars(Stream, Chars, ['A','B','C']),
		close(Stream).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(reader_stream_to_chars_3_03, true(Chars == [a,b,c,'\r','\n',d,e,f,'\r','\n',g,h,i,'\r','\n','A','B','C'])) :-
			text_file_stream('test_files/lines', Stream),
			reader::stream_to_chars(Stream, Chars, ['A','B','C']),
			close(Stream).

	:- else.

		test(reader_stream_to_chars_3_03, true(Chars == [a,b,c,'\n',d,e,f,'\n',g,h,i,'\n','A','B','C'])) :-
			text_file_stream('test_files/lines', Stream),
			reader::stream_to_chars(Stream, Chars, ['A','B','C']),
			close(Stream).

	:- endif.

	test(reader_stream_to_terms_2_01, true(Terms == [])) :-
		text_file_stream('test_files/empty', Stream),
		reader::stream_to_terms(Stream, Terms),
		close(Stream).

	test(reader_stream_to_terms_2_02, true(Terms == [a,b,c])) :-
		text_file_stream('test_files/terms', Stream),
		reader::stream_to_terms(Stream, Terms),
		close(Stream).

	test(reader_stream_to_terms_3_01, true(Terms == [])) :-
		text_file_stream('test_files/empty', Stream),
		reader::stream_to_terms(Stream, Terms, []),
		close(Stream).

	test(reader_stream_to_terms_3_02, true(Terms == [d,e,f])) :-
		text_file_stream('test_files/empty', Stream),
		reader::stream_to_terms(Stream, Terms, [d,e,f]),
		close(Stream).

	test(reader_stream_to_terms_3_03, true(Terms == [a,b,c,d,e,f])) :-
		text_file_stream('test_files/terms', Stream),
		reader::stream_to_terms(Stream, Terms, [d,e,f]),
		close(Stream).

	test(reader_stream_to_bytes_2_01, true(Bytes == [])) :-
		binary_file_stream('test_files/empty', Stream),
		reader::stream_to_bytes(Stream, Bytes),
		close(Stream).

	test(reader_stream_to_bytes_2_02, true(Bytes == [65,66,67,68,69,70])) :-
		binary_file_stream('test_files/bytes', Stream),
		reader::stream_to_bytes(Stream, Bytes),
		close(Stream).

	test(reader_stream_to_bytes_3_01, true(Bytes == [])) :-
		binary_file_stream('test_files/empty', Stream),
		reader::stream_to_bytes(Stream, Bytes, []),
		close(Stream).

	test(reader_stream_to_bytes_3_02, true(Bytes == [71,72,73])) :-
		binary_file_stream('test_files/empty', Stream),
		reader::stream_to_bytes(Stream, Bytes, [71,72,73]),
		close(Stream).

	test(reader_stream_to_bytes_3_03, true(Bytes == [65,66,67,68,69,70,71,72,73])) :-
		binary_file_stream('test_files/bytes', Stream),
		reader::stream_to_bytes(Stream, Bytes, [71,72,73]),
		close(Stream).

	% line reader predicates

	test(reader_line_to_chars_2_01, true(Chars == end_of_file)) :-
		text_file_stream('test_files/empty', Stream),
		reader::line_to_chars(Stream, Chars),
		close(Stream).

	test(reader_line_to_chars_2_02, true(Chars == [a,b,c])) :-
		text_file_stream('test_files/lines', Stream),
		reader::line_to_chars(Stream, Chars),
		close(Stream).

	test(reader_line_to_chars_2_03, true([Chars1,Chars2,Chars3] == [[a,b,c],[d,e,f],[g,h,i]])) :-
		text_file_stream('test_files/lines', Stream),
		reader::line_to_chars(Stream, Chars1),
		reader::line_to_chars(Stream, Chars2),
		reader::line_to_chars(Stream, Chars3),
		close(Stream).

	test(reader_line_to_chars_2_04, true([Chars1,Chars2,Chars3,Chars4] == [[a,b,c],[d,e,f],[g,h,i],end_of_file])) :-
		text_file_stream('test_files/lines', Stream),
		reader::line_to_chars(Stream, Chars1),
		reader::line_to_chars(Stream, Chars2),
		reader::line_to_chars(Stream, Chars3),
		reader::line_to_chars(Stream, Chars4),
		close(Stream).

	test(reader_line_to_chars_2_05, true([Chars] == [[a,b,c]])) :-
		^^set_text_input(in, 'abc'),
		reader::line_to_chars(in, Chars),
		close(in).

	test(reader_line_to_chars_2_06, true([Chars1,Chars2] == [[a,b,c],end_of_file])) :-
		^^set_text_input(in, 'abc'),
		reader::line_to_chars(in, Chars1),
		reader::line_to_chars(in, Chars2),
		close(in).

	test(reader_line_to_chars_3_01, true(Chars == [])) :-
		text_file_stream('test_files/empty', Stream),
		reader::line_to_chars(Stream, Chars, []),
		close(Stream).

	test(reader_line_to_chars_3_02, true(Chars == [a,b,c,'\n'])) :-
		text_file_stream('test_files/lines', Stream),
		reader::line_to_chars(Stream, Chars, []),
		close(Stream).

	test(reader_line_to_codes_2_01, true(Codes == end_of_file)) :-
		text_file_stream('test_files/empty', Stream),
		reader::line_to_codes(Stream, Codes),
		close(Stream).

	test(reader_line_to_codes_2_02, true(Codes == [97,98,99])) :-
		text_file_stream('test_files/lines', Stream),
		reader::line_to_codes(Stream, Codes),
		close(Stream).

	test(reader_line_to_codes_2_03, true([Codes1,Codes2,Codes3] == [[97,98,99],[100,101,102],[103,104,105]])) :-
		text_file_stream('test_files/lines', Stream),
		reader::line_to_codes(Stream, Codes1),
		reader::line_to_codes(Stream, Codes2),
		reader::line_to_codes(Stream, Codes3),
		close(Stream).

	test(reader_line_to_codes_2_04, true([Codes1,Codes2,Codes3,Codes4] == [[97,98,99],[100,101,102],[103,104,105],end_of_file])) :-
		text_file_stream('test_files/lines', Stream),
		reader::line_to_codes(Stream, Codes1),
		reader::line_to_codes(Stream, Codes2),
		reader::line_to_codes(Stream, Codes3),
		reader::line_to_codes(Stream, Codes4),
		close(Stream).

	test(reader_line_to_codes_2_05, true([Codes] == [[97,98,99]])) :-
		^^set_text_input(in, 'abc'),
		reader::line_to_codes(in, Codes),
		close(in).

	test(reader_line_to_codes_2_06, true([Codes1,Codes2] == [[97,98,99],end_of_file])) :-
		^^set_text_input(in, 'abc'),
		reader::line_to_codes(in, Codes1),
		reader::line_to_codes(in, Codes2),
		close(in).

	test(reader_line_to_codes_3_01, true(Codes == [])) :-
		text_file_stream('test_files/empty', Stream),
		reader::line_to_codes(Stream, Codes, []),
		close(Stream).

	test(reader_line_to_codes_3_02, true(Codes == [97,98,99,10])) :-
		text_file_stream('test_files/lines', Stream),
		reader::line_to_codes(Stream, Codes, []),
		close(Stream).

	cleanup :-
		^^clean_text_input.

	% auxiliary predicates

	text_file_stream(File, Stream) :-
		^^file_path(File, Path),
		open(Path, read, Stream).

	binary_file_stream(File, Stream) :-
		^^file_path(File, Path),
		open(Path, read, Stream, [type(binary)]).

:- end_object.
