%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2019/02/26,
		comment is 'Unit tests for the "reader" library.'
	]).

	cover(reader).

	% file reader predicates

	test(reader_file_to_codes_2_01, true(Codes == [])) :-
		file_path(empty, Path),
		reader::file_to_codes(Path, Codes).

	test(reader_file_to_codes_2_02, true(Codes == [97,98,99,10,100,101,102,10,103,104,105,10])) :-
		file_path(lines, Path),
		reader::file_to_codes(Path, Codes).

	test(reader_file_to_codes_3_01, true(Codes == [])) :-
		file_path(empty, Path),
		reader::file_to_codes(Path, Codes, []).

	test(reader_file_to_codes_3_02, true(Codes == [65,66,67])) :-
		file_path(empty, Path),
		reader::file_to_codes(Path, Codes, [65,66,67]).

	test(reader_file_to_codes_3_03, true(Codes == [97,98,99,10,100,101,102,10,103,104,105,10,65,66,67])) :-
		file_path(lines, Path),
		reader::file_to_codes(Path, Codes, [65,66,67]).

	test(reader_file_to_chars_2_01, true(Chars == [])) :-
		file_path(empty, Path),
		reader::file_to_chars(Path, Chars).

	test(reader_file_to_chars_2_02, true(Chars == [a,b,c,'\n',d,e,f,'\n',g,h,i,'\n'])) :-
		file_path(lines, Path),
		reader::file_to_chars(Path, Chars).

	test(reader_file_to_chars_3_01, true(Chars == [])) :-
		file_path(empty, Path),
		reader::file_to_chars(Path, Chars, []).

	test(reader_file_to_chars_3_02, true(Chars == ['A','B','C'])) :-
		file_path(empty, Path),
		reader::file_to_chars(Path, Chars, ['A','B','C']).

	test(reader_file_to_chars_3_03, true(Chars == [a,b,c,'\n',d,e,f,'\n',g,h,i,'\n','A','B','C'])) :-
		file_path(lines, Path),
		reader::file_to_chars(Path, Chars, ['A','B','C']).

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

	% stream reader predicates

	test(reader_stream_to_codes_2_01, true(Codes == [])) :-
		file_stream(empty, Stream),
		reader::stream_to_codes(Stream, Codes),
		close(Stream).

	test(reader_stream_to_codes_2_02, true(Codes == [97,98,99,10,100,101,102,10,103,104,105,10])) :-
		file_stream(lines, Stream),
		reader::stream_to_codes(Stream, Codes),
		close(Stream).

	test(reader_stream_to_codes_3_01, true(Codes == [])) :-
		file_stream(empty, Stream),
		reader::stream_to_codes(Stream, Codes, []),
		close(Stream).

	test(reader_stream_to_codes_3_02, true(Codes == [65,66,67])) :-
		file_stream(empty, Stream),
		reader::stream_to_codes(Stream, Codes, [65,66,67]),
		close(Stream).

	test(reader_stream_to_codes_3_03, true(Codes == [97,98,99,10,100,101,102,10,103,104,105,10,65,66,67])) :-
		file_stream(lines, Stream),
		reader::stream_to_codes(Stream, Codes, [65,66,67]),
		close(Stream).

	test(reader_stream_to_chars_2_01, true(Chars == [])) :-
		file_stream(empty, Stream),
		reader::stream_to_chars(Stream, Chars),
		close(Stream).

	test(reader_stream_to_chars_2_02, true(Chars == [a,b,c,'\n',d,e,f,'\n',g,h,i,'\n'])) :-
		file_stream(lines, Stream),
		reader::stream_to_chars(Stream, Chars),
		close(Stream).

	test(reader_stream_to_chars_3_01, true(Chars == [])) :-
		file_stream(empty, Stream),
		reader::stream_to_chars(Stream, Chars, []),
		close(Stream).

	test(reader_stream_to_chars_3_02, true(Chars == ['A','B','C'])) :-
		file_stream(empty, Stream),
		reader::stream_to_chars(Stream, Chars, ['A','B','C']),
		close(Stream).

	test(reader_stream_to_chars_3_03, true(Chars == [a,b,c,'\n',d,e,f,'\n',g,h,i,'\n','A','B','C'])) :-
		file_stream(lines, Stream),
		reader::stream_to_chars(Stream, Chars, ['A','B','C']),
		close(Stream).

	test(reader_stream_to_terms_2_01, true(Terms == [])) :-
		file_stream(empty, Stream),
		reader::stream_to_terms(Stream, Terms),
		close(Stream).

	test(reader_stream_to_terms_2_02, true(Terms == [a,b,c])) :-
		file_stream(terms, Stream),
		reader::stream_to_terms(Stream, Terms),
		close(Stream).

	test(reader_stream_to_terms_3_01, true(Terms == [])) :-
		file_stream(empty, Stream),
		reader::stream_to_terms(Stream, Terms, []),
		close(Stream).

	test(reader_stream_to_terms_3_02, true(Terms == [d,e,f])) :-
		file_stream(empty, Stream),
		reader::stream_to_terms(Stream, Terms, [d,e,f]),
		close(Stream).

	test(reader_stream_to_terms_3_03, true(Terms == [a,b,c,d,e,f])) :-
		file_stream(terms, Stream),
		reader::stream_to_terms(Stream, Terms, [d,e,f]),
		close(Stream).

	% line reader predicates

	test(reader_line_to_codes_2_01, true(Codes == end_of_file)) :-
		file_stream(empty, Stream),
		reader::line_to_codes(Stream, Codes),
		close(Stream).

	test(reader_line_to_codes_2_02, true(Codes == [97,98,99])) :-
		file_stream(lines, Stream),
		reader::line_to_codes(Stream, Codes),
		close(Stream).

	test(reader_line_to_codes_2_03, true([Codes1,Codes2,Codes3] == [[97,98,99],[100,101,102],[103,104,105]])) :-
		file_stream(lines, Stream),
		reader::line_to_codes(Stream, Codes1),
		reader::line_to_codes(Stream, Codes2),
		reader::line_to_codes(Stream, Codes3),
		close(Stream).

	test(reader_line_to_codes_2_04, true([Codes1,Codes2,Codes3,Codes4] == [[97,98,99],[100,101,102],[103,104,105],end_of_file])) :-
		file_stream(lines, Stream),
		reader::line_to_codes(Stream, Codes1),
		reader::line_to_codes(Stream, Codes2),
		reader::line_to_codes(Stream, Codes3),
		reader::line_to_codes(Stream, Codes4),
		close(Stream).

	test(reader_line_to_codes_3_01, true(Codes == [])) :-
		file_stream(empty, Stream),
		reader::line_to_codes(Stream, Codes, []),
		close(Stream).

	test(reader_line_to_codes_3_02, true(Codes == [97,98,99,10])) :-
		file_stream(lines, Stream),
		reader::line_to_codes(Stream, Codes, []),
		close(Stream).

	% auxiliary predicates

	file_path(File, Path) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, File, Path).

	file_stream(File, Stream) :-
		file_path(File, Path),
		open(Path, read, Stream).

:- end_object.
