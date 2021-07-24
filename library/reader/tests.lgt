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
		version is 2:0:2,
		author is 'Paulo Moura',
		date is 2021-07-24,
		comment is 'Unit tests for the "reader" library.'
	]).

	cover(reader).

	% file reader predicates

	test(reader_file_to_codes_2_01, true(Codes == [])) :-
		file_path(empty, Path),
		reader::file_to_codes(Path, Codes).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(reader_file_to_codes_2_02, true(Codes == [97,98,99,13,10,100,101,102,13,10,103,104,105,13,10])) :-
			file_path(lines, Path),
			reader::file_to_codes(Path, Codes).

	:- else.

		test(reader_file_to_codes_2_02, true(Codes == [97,98,99,10,100,101,102,10,103,104,105,10])) :-
			file_path(lines, Path),
			reader::file_to_codes(Path, Codes).

	:- endif.

	test(reader_file_to_codes_3_01, true(Codes == [])) :-
		file_path(empty, Path),
		reader::file_to_codes(Path, Codes, []).

	test(reader_file_to_codes_3_02, true(Codes == [65,66,67])) :-
		file_path(empty, Path),
		reader::file_to_codes(Path, Codes, [65,66,67]).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(reader_file_to_codes_3_03, true(Codes == [97,98,99,13,10,100,101,102,13,10,103,104,105,13,10,65,66,67])) :-
			file_path(lines, Path),
			reader::file_to_codes(Path, Codes, [65,66,67]).

	:- else.

		test(reader_file_to_codes_3_03, true(Codes == [97,98,99,10,100,101,102,10,103,104,105,10,65,66,67])) :-
			file_path(lines, Path),
			reader::file_to_codes(Path, Codes, [65,66,67]).

	:- endif.

	test(reader_file_to_chars_2_01, true(Chars == [])) :-
		file_path(empty, Path),
		reader::file_to_chars(Path, Chars).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(reader_file_to_chars_2_02, true(Chars == [a,b,c,'\r','\n',d,e,f,'\r','\n',g,h,i,'\r','\n'])) :-
			file_path(lines, Path),
			reader::file_to_chars(Path, Chars).

	:- else.

		test(reader_file_to_chars_2_02, true(Chars == [a,b,c,'\n',d,e,f,'\n',g,h,i,'\n'])) :-
			file_path(lines, Path),
			reader::file_to_chars(Path, Chars).

	:- endif.

	test(reader_file_to_chars_3_01, true(Chars == [])) :-
		file_path(empty, Path),
		reader::file_to_chars(Path, Chars, []).

	test(reader_file_to_chars_3_02, true(Chars == ['A','B','C'])) :-
		file_path(empty, Path),
		reader::file_to_chars(Path, Chars, ['A','B','C']).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(reader_file_to_chars_3_03, true(Chars == [a,b,c,'\r','\n',d,e,f,'\r','\n',g,h,i,'\r','\n','A','B','C'])) :-
			file_path(lines, Path),
			reader::file_to_chars(Path, Chars, ['A','B','C']).

	:- else.

		test(reader_file_to_chars_3_03, true(Chars == [a,b,c,'\n',d,e,f,'\n',g,h,i,'\n','A','B','C'])) :-
			file_path(lines, Path),
			reader::file_to_chars(Path, Chars, ['A','B','C']).

	:- endif.

	test(reader_file_to_terms_2_01, true(Terms == [])) :-
		file_path(empty, Path),
		reader::file_to_terms(Path, Terms).

	test(reader_file_to_terms_2_02, true(Terms == [a,b,c])) :-
		file_path(terms, Path),
		reader::file_to_terms(Path, Terms).

	test(reader_file_to_terms_3_01, true(Terms == [])) :-
		file_path(empty, Path),
		reader::file_to_terms(Path, Terms, []).

	test(reader_file_to_terms_3_02, true(Terms == [d,e,f])) :-
		file_path(empty, Path),
		reader::file_to_terms(Path, Terms, [d,e,f]).

	test(reader_file_to_terms_3_03, true(Terms == [a,b,c,d,e,f])) :-
		file_path(terms, Path),
		reader::file_to_terms(Path, Terms, [d,e,f]).

	test(reader_file_to_bytes_2_01, true(Bytes == [])) :-
		file_path(empty, Path),
		reader::file_to_bytes(Path, Bytes).

	test(reader_file_to_bytes_2_02, true(Bytes == [65,66,67,68,69,70])) :-
		file_path(bytes, Path),
		reader::file_to_bytes(Path, Bytes).

	test(reader_file_to_bytes_3_01, true(Bytes == [])) :-
		file_path(empty, Path),
		reader::file_to_bytes(Path, Bytes, []).

	test(reader_file_to_bytes_3_02, true(Bytes == [71,72,73])) :-
		file_path(empty, Path),
		reader::file_to_bytes(Path, Bytes, [71,72,73]).

	test(reader_file_to_bytes_3_03, true(Bytes == [65,66,67,68,69,70,71,72,73])) :-
		file_path(bytes, Path),
		reader::file_to_bytes(Path, Bytes, [71,72,73]).

	% stream reader predicates

	test(reader_stream_to_codes_2_01, true(Codes == [])) :-
		text_file_stream(empty, Stream),
		reader::stream_to_codes(Stream, Codes),
		close(Stream).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(reader_stream_to_codes_2_02, true(Codes == [97,98,99,13,10,100,101,102,13,10,103,104,105,13,10])) :-
			text_file_stream(lines, Stream),
			reader::stream_to_codes(Stream, Codes),
			close(Stream).

	:- else.

		test(reader_stream_to_codes_2_02, true(Codes == [97,98,99,10,100,101,102,10,103,104,105,10])) :-
			text_file_stream(lines, Stream),
			reader::stream_to_codes(Stream, Codes),
			close(Stream).

	:- endif.

	test(reader_stream_to_codes_3_01, true(Codes == [])) :-
		text_file_stream(empty, Stream),
		reader::stream_to_codes(Stream, Codes, []),
		close(Stream).

	test(reader_stream_to_codes_3_02, true(Codes == [65,66,67])) :-
		text_file_stream(empty, Stream),
		reader::stream_to_codes(Stream, Codes, [65,66,67]),
		close(Stream).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(reader_stream_to_codes_3_03, true(Codes == [97,98,99,13,10,100,101,102,13,10,103,104,105,13,10,65,66,67])) :-
			text_file_stream(lines, Stream),
			reader::stream_to_codes(Stream, Codes, [65,66,67]),
			close(Stream).

	:- else.

		test(reader_stream_to_codes_3_03, true(Codes == [97,98,99,10,100,101,102,10,103,104,105,10,65,66,67])) :-
			text_file_stream(lines, Stream),
			reader::stream_to_codes(Stream, Codes, [65,66,67]),
			close(Stream).

	:- endif.

	test(reader_stream_to_chars_2_01, true(Chars == [])) :-
		text_file_stream(empty, Stream),
		reader::stream_to_chars(Stream, Chars),
		close(Stream).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(reader_stream_to_chars_2_02, true(Chars == [a,b,c,'\r','\n',d,e,f,'\r','\n',g,h,i,'\r','\n'])) :-
			text_file_stream(lines, Stream),
			reader::stream_to_chars(Stream, Chars),
			close(Stream).

	:- else.

		test(reader_stream_to_chars_2_02, true(Chars == [a,b,c,'\n',d,e,f,'\n',g,h,i,'\n'])) :-
			text_file_stream(lines, Stream),
			reader::stream_to_chars(Stream, Chars),
			close(Stream).

	:- endif.

	test(reader_stream_to_chars_3_01, true(Chars == [])) :-
		text_file_stream(empty, Stream),
		reader::stream_to_chars(Stream, Chars, []),
		close(Stream).

	test(reader_stream_to_chars_3_02, true(Chars == ['A','B','C'])) :-
		text_file_stream(empty, Stream),
		reader::stream_to_chars(Stream, Chars, ['A','B','C']),
		close(Stream).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(reader_stream_to_chars_3_03, true(Chars == [a,b,c,'\r','\n',d,e,f,'\r','\n',g,h,i,'\r','\n','A','B','C'])) :-
			text_file_stream(lines, Stream),
			reader::stream_to_chars(Stream, Chars, ['A','B','C']),
			close(Stream).

	:- else.

		test(reader_stream_to_chars_3_03, true(Chars == [a,b,c,'\n',d,e,f,'\n',g,h,i,'\n','A','B','C'])) :-
			text_file_stream(lines, Stream),
			reader::stream_to_chars(Stream, Chars, ['A','B','C']),
			close(Stream).

	:- endif.

	test(reader_stream_to_terms_2_01, true(Terms == [])) :-
		text_file_stream(empty, Stream),
		reader::stream_to_terms(Stream, Terms),
		close(Stream).

	test(reader_stream_to_terms_2_02, true(Terms == [a,b,c])) :-
		text_file_stream(terms, Stream),
		reader::stream_to_terms(Stream, Terms),
		close(Stream).

	test(reader_stream_to_terms_3_01, true(Terms == [])) :-
		text_file_stream(empty, Stream),
		reader::stream_to_terms(Stream, Terms, []),
		close(Stream).

	test(reader_stream_to_terms_3_02, true(Terms == [d,e,f])) :-
		text_file_stream(empty, Stream),
		reader::stream_to_terms(Stream, Terms, [d,e,f]),
		close(Stream).

	test(reader_stream_to_terms_3_03, true(Terms == [a,b,c,d,e,f])) :-
		text_file_stream(terms, Stream),
		reader::stream_to_terms(Stream, Terms, [d,e,f]),
		close(Stream).

	test(reader_stream_to_bytes_2_01, true(Bytes == [])) :-
		binary_file_stream(empty, Stream),
		reader::stream_to_bytes(Stream, Bytes),
		close(Stream).

	test(reader_stream_to_bytes_2_02, true(Bytes == [65,66,67,68,69,70])) :-
		binary_file_stream(bytes, Stream),
		reader::stream_to_bytes(Stream, Bytes),
		close(Stream).

	test(reader_stream_to_bytes_3_01, true(Bytes == [])) :-
		binary_file_stream(empty, Stream),
		reader::stream_to_bytes(Stream, Bytes, []),
		close(Stream).

	test(reader_stream_to_bytes_3_02, true(Bytes == [71,72,73])) :-
		binary_file_stream(empty, Stream),
		reader::stream_to_bytes(Stream, Bytes, [71,72,73]),
		close(Stream).

	test(reader_stream_to_bytes_3_03, true(Bytes == [65,66,67,68,69,70,71,72,73])) :-
		binary_file_stream(bytes, Stream),
		reader::stream_to_bytes(Stream, Bytes, [71,72,73]),
		close(Stream).

	% line reader predicates

	test(reader_line_to_codes_2_01, true(Codes == end_of_file)) :-
		text_file_stream(empty, Stream),
		reader::line_to_codes(Stream, Codes),
		close(Stream).

	test(reader_line_to_codes_2_02, true(Codes == [97,98,99])) :-
		text_file_stream(lines, Stream),
		reader::line_to_codes(Stream, Codes),
		close(Stream).

	test(reader_line_to_codes_2_03, true([Codes1,Codes2,Codes3] == [[97,98,99],[100,101,102],[103,104,105]])) :-
		text_file_stream(lines, Stream),
		reader::line_to_codes(Stream, Codes1),
		reader::line_to_codes(Stream, Codes2),
		reader::line_to_codes(Stream, Codes3),
		close(Stream).

	test(reader_line_to_codes_2_04, true([Codes1,Codes2,Codes3,Codes4] == [[97,98,99],[100,101,102],[103,104,105],end_of_file])) :-
		text_file_stream(lines, Stream),
		reader::line_to_codes(Stream, Codes1),
		reader::line_to_codes(Stream, Codes2),
		reader::line_to_codes(Stream, Codes3),
		reader::line_to_codes(Stream, Codes4),
		close(Stream).

	test(reader_line_to_codes_3_01, true(Codes == [])) :-
		text_file_stream(empty, Stream),
		reader::line_to_codes(Stream, Codes, []),
		close(Stream).

	test(reader_line_to_codes_3_02, true(Codes == [97,98,99,10])) :-
		text_file_stream(lines, Stream),
		reader::line_to_codes(Stream, Codes, []),
		close(Stream).

	% auxiliary predicates

	file_path(File, Path) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::path_concat(Directory, test_files, Path0),
		os::path_concat(Path0, File, Path).

	text_file_stream(File, Stream) :-
		file_path(File, Path),
		open(Path, read, Stream).

	binary_file_stream(File, Stream) :-
		file_path(File, Path),
		open(Path, read, Stream, [type(binary)]).

:- end_object.
