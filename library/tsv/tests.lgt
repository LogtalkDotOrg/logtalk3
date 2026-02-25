%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2021 Jacinto Dávila <jdavila@optimusprime.ai>
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


:- dynamic(p/3).
:- dynamic(r/3).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 2:1:0,
		author is 'Paulo Moura',
		date is 2026-02-25,
		comment is 'Tests for the ``tsv`` library.'
	]).

	cover(tsv(_, _)).
	cover(tsv(_)).
	cover(tsv).

	setup :-
		^^clean_file('test_files/output00.tsv'),
		^^clean_file('test_files/output01.tsv'),
		^^clean_file('test_files/output02.tsv').

	cleanup :-
		setup.

	% read_file/2 tests

	test(tsv_read_file_2_header_only_file_keep, true(Rows == [['Name','Age','Address']])) :-
		^^suppress_text_output,
		^^file_path('test_files/header_only.tsv', Path),
		tsv(keep)::read_file(Path, Rows).

	test(tsv_read_file_2_header_only_file_skip, true(Rows == [])) :-
		^^suppress_text_output,
		^^file_path('test_files/header_only.tsv', Path),
		tsv(skip)::read_file(Path, Rows).

	test(tsv_read_file_2_lf_ending, true(Rows == [['Name','Age','Address'], ['Paul',58,'Sunset Street'], ['Anna',52,'Central Plaza'], ['Zeke',45,'Main Street'], ['Lucy',42,'Old Oak']])) :-
		^^suppress_text_output,
		^^file_path('test_files/lf_ending.tsv', Path),
		tsv(keep)::read_file(Path, Rows).

	test(tsv_read_file_2_crlf_ending, true(Rows == [['Name','Age','Address'], ['Paul',58,'Sunset Street'], ['Anna',52,'Central Plaza'], ['Zeke',45,'Main Street'], ['Lucy',42,'Old Oak']])) :-
		^^suppress_text_output,
		^^file_path('test_files/crlf_ending.tsv', Path),
		tsv(keep)::read_file(Path, Rows).

	test(tsv_read_sample_no_lf_at_end, true(Rows == [['Name','Age','Address'], ['Paul',58,'Sunset Street'], ['Anna',52,'Central Plaza'], ['Zeke',45,'Main Street'], ['Lucy',42,'Old Oak']])) :-
		^^suppress_text_output,
		^^file_path('test_files/no_lf_at_end.tsv', Path),
		tsv(keep)::read_file(Path, Rows).

	test(tsv_read_file_2_no_crlf_at_end, true(Rows == [['Name','Age','Address'], ['Paul',58,'Sunset Street'], ['Anna',52,'Central Plaza'], ['Zeke',45,'Main Street'], ['Lucy',42,'Old Oak']])) :-
		^^suppress_text_output,
		^^file_path('test_files/no_crlf_at_end.tsv', Path),
		tsv(keep)::read_file(Path, Rows).

	test(tsv_read_file_2_escapes, true(Rows == [['Name','Age','Address'], ['Paul',58,'Sunset\\tStreet'], ['Anna',52,'Central\\rPlaza'], ['Zeke',45,'Main\\nStreet'], ['Lucy',42,'Old\\\\Oak']])) :-
		^^suppress_text_output,
		^^file_path('test_files/escapes.tsv', Path),
		tsv(keep)::read_file(Path, Rows).

	test(tsv_read_file_2_comments_default_false, true(Rows == [['# persons table with comments'], ['Name','Age','Address'], ['# data rows'], ['Paul',58,'Sunset Street'], ['# another comment'], ['Anna',52,'Central Plaza'], ['Zeke',45,'Main Street'], ['Lucy',42,'Old Oak'], ['# end marker']])) :-
		^^suppress_text_output,
		^^file_path('test_files/comments.tsv', Path),
		tsv(keep)::read_file(Path, Rows).

	test(tsv_read_file_2_comments_true_keep_header, true(Rows == [['Name','Age','Address'], ['Paul',58,'Sunset Street'], ['Anna',52,'Central Plaza'], ['Zeke',45,'Main Street'], ['Lucy',42,'Old Oak']])) :-
		^^suppress_text_output,
		^^file_path('test_files/comments.tsv', Path),
		tsv(keep, true)::read_file(Path, Rows).

	test(tsv_read_file_2_comments_true_skip_header, true(Rows == [['Paul',58,'Sunset Street'], ['Anna',52,'Central Plaza'], ['Zeke',45,'Main Street'], ['Lucy',42,'Old Oak']])) :-
		^^suppress_text_output,
		^^file_path('test_files/comments.tsv', Path),
		tsv(skip, true)::read_file(Path, Rows).

	% read_file_by_line/2 tests

	test(tsv_read_file_by_line_2_lf_ending, true(Rows == [['Name','Age','Address'], ['Paul',58,'Sunset Street'], ['Anna',52,'Central Plaza'], ['Zeke',45,'Main Street'], ['Lucy',42,'Old Oak']])) :-
		^^suppress_text_output,
		^^file_path('test_files/lf_ending.tsv', Path),
		tsv(keep)::read_file_by_line(Path, Rows).

	test(tsv_read_file_by_line_2_comments_true_skip_header, true(Rows == [['Paul',58,'Sunset Street'], ['Anna',52,'Central Plaza'], ['Zeke',45,'Main Street'], ['Lucy',42,'Old Oak']])) :-
		^^suppress_text_output,
		^^file_path('test_files/comments.tsv', Path),
		tsv(skip, true)::read_file_by_line(Path, Rows).

	% read_stream/2 tests

	test(tsv_read_stream_2_lf_crlf_ending, true(Rows == [['Paul',58,'Sunset Street'], ['Anna',52,'Central Plaza'], ['Zeke',45,'Main Street'], ['Lucy',42,'Old Oak']])) :-
		^^suppress_text_output,
		^^file_path('test_files/crlf_ending.tsv', Path),
		open(Path, read, Stream),
		tsv(skip)::read_stream(Stream, Rows),
		close(Stream).

	test(tsv_read_stream_2_comments_true_skip_header, true(Rows == [['Paul',58,'Sunset Street'], ['Anna',52,'Central Plaza'], ['Zeke',45,'Main Street'], ['Lucy',42,'Old Oak']])) :-
		^^suppress_text_output,
		^^file_path('test_files/comments.tsv', Path),
		open(Path, read, Stream),
		tsv(skip, true)::read_stream(Stream, Rows),
		close(Stream).

	% read_stream_by_line/2 tests

	test(tsv_read_stream_by_line_2_crlf_ending, true(Rows == [['Paul',58,'Sunset Street'], ['Anna',52,'Central Plaza'], ['Zeke',45,'Main Street'], ['Lucy',42,'Old Oak']])) :-
		^^suppress_text_output,
		^^file_path('test_files/crlf_ending.tsv', Path),
		open(Path, read, Stream),
		tsv(skip)::read_stream_by_line(Stream, Rows),
		close(Stream).

	test(tsv_read_stream_by_line_2_comments_true_skip_header, true(Rows == [['Paul',58,'Sunset Street'], ['Anna',52,'Central Plaza'], ['Zeke',45,'Main Street'], ['Lucy',42,'Old Oak']])) :-
		^^suppress_text_output,
		^^file_path('test_files/comments.tsv', Path),
		open(Path, read, Stream),
		tsv(skip, true)::read_stream_by_line(Stream, Rows),
		close(Stream).

	test(tsv_read_file_2_empty_beginning, true(Rows == [['',58,'Sunset Street'], ['Anna',52,'Central Plaza'], ['',45,'Main Street'], ['Lucy',42,'Old Oak']])) :-
		^^suppress_text_output,
		^^file_path('test_files/empty_beginning.tsv', Path),
		tsv(skip)::read_file(Path, Rows).

	test(tsv_read_file_2_empty_middle, true(Rows == [['Paul',58,'Sunset Street'], ['Anna','','Central Plaza'], ['Zeke',45,'Main Street'], ['Lucy','','Old Oak']])) :-
		^^suppress_text_output,
		^^file_path('test_files/empty_middle.tsv', Path),
		tsv(skip)::read_file(Path, Rows).

	test(tsv_read_file_2_empty_end, true(Rows == [['Paul', 58,''], ['Anna',52,'Central Plaza'], ['Zeke', 45,''], ['Lucy',42,'Old Oak']])) :-
		^^suppress_text_output,
		^^file_path('test_files/empty_end.tsv', Path),
		tsv(skip)::read_file(Path, Rows).

	% write_file/2 tests

	test(tsv_write_file_2_quotes, true(Rows == [['quote alone " in here','quote doubled ""','"no quote at the end']])) :-
		^^suppress_text_output,
		user::retractall(p(_, _, _)),
		user::assertz(p('quote alone " in here', 'quote doubled ""', '"no quote at the end')),
		^^file_path('test_files/output00.tsv', Path),
		tsv::write_file(Path, user, p/3),
		tsv::read_file(Path, Rows).

	% roundtrip tests

	test(tsv_round_trip_read_file, true) :-
		^^suppress_text_output,
		^^file_path('test_files/escapes.tsv', Input),
		^^file_path('test_files/output01.tsv', Output),
		roundtrip_file(read_file, Input, Output, p/3, r/3).

	test(tsv_round_trip_read_file_by_line, true) :-
		^^suppress_text_output,
		^^file_path('test_files/escapes.tsv', Input),
		^^file_path('test_files/output02.tsv', Output),
		roundtrip_file(read_file_by_line, Input, Output, p/3, r/3).

	test(tsv_round_trip_read_stream, true) :-
		^^suppress_text_output,
		^^file_path('test_files/escapes.tsv', Input),
		^^file_path('test_files/output01.tsv', Output),
		roundtrip_stream(read_stream, Input, Output, p/3, r/3).

	test(tsv_round_trip_read_stream_by_line, true) :-
		^^suppress_text_output,
		^^file_path('test_files/escapes.tsv', Input),
		^^file_path('test_files/output02.tsv', Output),
		roundtrip_stream(read_stream_by_line, Input, Output, p/3, r/3).

	% auxiliary predicates

	roundtrip_file(Read, Input, Output, Functor1/Arity1, Functor2/Arity2) :-
		functor(Goal1, Functor1, Arity1),
		user::retractall(Goal1),
		functor(Goal2, Functor2, Arity2),
		user::retractall(Goal2),
		ReadMessage1 =.. [Read, Input, user, Functor1/Arity1],
		tsv::ReadMessage1,
		tsv::write_file(Output, user, Functor1/Arity1),
		ReadMessage2 =.. [Read, Output, user, Functor2/Arity2],
		tsv::ReadMessage2,
		forall(user::Goal1, user::Goal2),
		forall(user::Goal2, user::Goal1).

	roundtrip_stream(Read, Input, Output, Functor1/Arity1, Functor2/Arity2) :-
		functor(Goal1, Functor1, Arity1),
		user::retractall(Goal1),
		functor(Goal2, Functor2, Arity2),
		user::retractall(Goal2),
		open(Input, read, InputStream),
		ReadMessage1 =.. [Read, InputStream, user, Functor1/Arity1],
		tsv::ReadMessage1,
		close(InputStream),
		open(Output, write, OutputStream1),
		tsv::write_stream(OutputStream1, user, Functor1/Arity1),
		close(OutputStream1),
		open(Output, read, OutputStream2),
		ReadMessage2 =.. [Read, OutputStream2, user, Functor2/Arity2],
		tsv::ReadMessage2,
		close(OutputStream2),
		forall(user::Goal1, user::Goal2),
		forall(user::Goal2, user::Goal1).

:- end_object.
