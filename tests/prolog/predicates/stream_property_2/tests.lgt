%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:7:0,
		author is 'Paulo Moura',
		date is 2021-09-14,
		comment is 'Unit tests for the ISO Prolog standard stream_property/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.8.4

	test(iso_stream_property_2_01, deterministic) :-
		os::absolute_file_name(foo, FooPath),
		os::absolute_file_name(bar, BarPath),
		^^create_text_file(FooPath, ''),
		open(FooPath, read, S1),
		open(BarPath, write, S2),
		findall(S-F, {stream_property(S, file_name(F))}, L),
		memberchk(S1-FooPath, L),
		memberchk(S2-BarPath, L).

	test(iso_stream_property_2_02, deterministic) :-
		os::absolute_file_name(bar, BarPath),
		open(BarPath, write, FOut),
		current_output(COut),
		findall(S, {stream_property(S, output)}, L),
		memberchk(FOut, L),
		memberchk(COut, L).

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(sics_stream_property_2_03, error(domain_error(stream,foo))) :-
		{stream_property(foo, _S)}.

	test(sics_stream_property_2_04, error(domain_error(stream_property,foo))) :-
		{stream_property(_S, foo)}.

	test(sics_stream_property_2_05a, deterministic) :-
		current_input(S),
		findall(P, {stream_property(S, P)}, L),
		memberchk(input, L).

	test(sics_stream_property_2_05b, deterministic) :-
		current_input(S),
		findall(A, {stream_property(S, alias(A))}, L),
		memberchk(user_input, L).

	test(sics_stream_property_2_05c, deterministic(Action == reset)) :-
		current_input(S),
		{stream_property(S, eof_action(Action))}.

	test(sics_stream_property_2_05d, deterministic(Mode == read)) :-
		current_input(S),
		{stream_property(S, mode(Mode))}.

	test(sics_stream_property_2_05e, deterministic(Reposition == false)) :-
		current_input(S),
		{stream_property(S, reposition(Reposition))}.

	test(sics_stream_property_2_05f, deterministic(Type == text)) :-
		current_input(S),
		{stream_property(S, type(Type))}.

	test(sics_stream_property_2_06a, deterministic) :-
		current_output(S),
		findall(P, {stream_property(S, P)}, L),
		memberchk(output, L).

	test(sics_stream_property_2_06b, deterministic) :-
		current_output(S),
		findall(A, {stream_property(S, alias(A))}, L),
		memberchk(user_output, L).

	test(sics_stream_property_2_06c, deterministic(Action == reset)) :-
		current_output(S),
		{stream_property(S, eof_action(Action))}.

	test(sics_stream_property_2_06d, deterministic(Mode == append)) :-
		current_output(S),
		{stream_property(S, mode(Mode))}.

	test(sics_stream_property_2_06e, deterministic(Reposition == false)) :-
		current_output(S),
		{stream_property(S, reposition(Reposition))}.

	test(sics_stream_property_2_06f, deterministic(Type == text)) :-
		current_output(S),
		{stream_property(S, type(Type))}.

	test(sics_stream_property_2_07, fail) :-
		{stream_property(_S, type(binary))}.

	% tests from the Logtalk portability work

	test(lgt_stream_property_2_08a, true) :-
		stream_property(S, alias(user_input)),
		findall(P, {stream_property(S, P)}, L),
		memberchk(input, L).

	test(lgt_stream_property_2_08b, true(Action == reset)) :-
		stream_property(S, alias(user_input)),
		{stream_property(S, eof_action(Action))}.

	test(lgt_stream_property_2_08c, true(Mode == read)) :-
		stream_property(S, alias(user_input)),
		{stream_property(S, mode(Mode))}.

	test(lgt_stream_property_2_08d, true(Reposition == false)) :-
		stream_property(S, alias(user_input)),
		{stream_property(S, reposition(Reposition))}.

	test(lgt_stream_property_2_08e, true(Type == text)) :-
		stream_property(S, alias(user_input)),
		{stream_property(S, type(Type))}.

	test(lgt_stream_property_2_09a, true) :-
		stream_property(S, alias(user_output)),
		findall(P, {stream_property(S, P)}, L),
		memberchk(output, L).

	test(lgt_stream_property_2_09b, true(Action == reset)) :-
		stream_property(S, alias(user_output)),
		{stream_property(S, eof_action(Action))}.

	test(lgt_stream_property_2_09c, true(Mode == append)) :-
		stream_property(S, alias(user_output)),
		{stream_property(S, mode(Mode))}.

	test(lgt_stream_property_2_09d, true(Reposition == false)) :-
		stream_property(S, alias(user_output)),
		{stream_property(S, reposition(Reposition))}.

	test(lgt_stream_property_2_09e, true(Type == text)) :-
		stream_property(S, alias(user_output)),
		{stream_property(S, type(Type))}.

	test(lgt_stream_property_2_10a, true) :-
		stream_property(S, alias(user_error)),
		findall(P, {stream_property(S, P)}, L),
		memberchk(output, L).

	test(lgt_stream_property_2_10b, true(Action == reset)) :-
		stream_property(S, alias(user_error)),
		{stream_property(S, eof_action(Action))}.

	test(lgt_stream_property_2_10c, true(Mode == append)) :-
		stream_property(S, alias(user_error)),
		{stream_property(S, mode(Mode))}.

	test(lgt_stream_property_2_10d, true(Reposition == false)) :-
		stream_property(S, alias(user_error)),
		{stream_property(S, reposition(Reposition))}.

	test(lgt_stream_property_2_10e, true(Type == text)) :-
		stream_property(S, alias(user_error)),
		{stream_property(S, type(Type))}.

	test(lgt_stream_property_2_11, error(existence_error(stream,_))) :-
		^^closed_input_stream(S, [alias(foo)]),
		{stream_property(S, type(_))}.

	test(lgt_stream_property_2_12, error(existence_error(stream,_))) :-
		^^closed_output_stream(S, [alias(bar)]),
		{stream_property(S, type(_))}.

	test(lgt_stream_property_2_13, true) :-
		{stream_property(_, _)}.

	% tests for the file_name/1 stream property

	test(lgt_stream_property_2_14, true(File == Path)) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, write, Stream),
		{stream_property(Stream, file_name(File))},
		close(Stream).

	test(lgt_stream_property_2_15, true(File == Path)) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, append, Stream),
		{stream_property(Stream, file_name(File))},
		close(Stream).

	test(lgt_stream_property_2_16, true(File == Path)) :-
		os::absolute_file_name(input_text_file, Path),
		^^create_text_file(Path, ''),
		open(Path, read, Stream),
		{stream_property(Stream, file_name(File))},
		close(Stream).

	% tests for the mode/1 stream property

	test(lgt_stream_property_2_17, true(Mode == write)) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, write, Stream),
		{stream_property(Stream, mode(Mode))},
		close(Stream).

	test(lgt_stream_property_2_18, true(Mode == append)) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, append, Stream),
		{stream_property(Stream, mode(Mode))},
		close(Stream).

	test(lgt_stream_property_2_19, true(Mode == read)) :-
		os::absolute_file_name(input_text_file, Path),
		^^create_text_file(Path, ''),
		open(Path, read, Stream),
		{stream_property(Stream, mode(Mode))},
		close(Stream).

	% tests for the input/0 stream property

	test(lgt_stream_property_2_20, false, [cleanup(close(out))]) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, write, Stream, [alias(out)]),
		{stream_property(Stream, input)}.

	test(lgt_stream_property_2_21, false, [cleanup(close(out))]) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, append, Stream, [alias(out)]),
		{stream_property(Stream, input)}.

	test(lgt_stream_property_2_22, true, [cleanup(close(in))]) :-
		os::absolute_file_name(input_text_file, Path),
		^^create_text_file(Path, ''),
		open(Path, read, Stream, [alias(in)]),
		{stream_property(Stream, input)}.

	% tests for the output/0 stream property

	test(lgt_stream_property_2_23, true, [cleanup(close(out))]) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, write, Stream, [alias(out)]),
		{stream_property(Stream, output)}.

	test(lgt_stream_property_2_24, true, [cleanup(close(out))]) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, append, Stream, [alias(out)]),
		{stream_property(Stream, output)}.

	test(lgt_stream_property_2_25, false, [cleanup(close(in))]) :-
		os::absolute_file_name(input_text_file, Path),
		^^create_text_file(Path, ''),
		open(Path, read, Stream, [alias(in)]),
		{stream_property(Stream, output)}.

	% tests for the alias/1 stream property

	test(lgt_stream_property_2_26, true(Alias == w)) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, write, Stream, [alias(w)]),
		{stream_property(Stream, alias(Alias))},
		close(Stream).

	test(lgt_stream_property_2_27, true(Alias == a)) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, append, Stream, [alias(a)]),
		{stream_property(Stream, alias(Alias))},
		close(Stream).

	test(lgt_stream_property_2_28, true(Alias == r)) :-
		os::absolute_file_name(input_text_file, Path),
		^^create_text_file(Path, ''),
		open(Path, read, Stream, [alias(r)]),
		{stream_property(Stream, alias(Alias))},
		close(Stream).

	% tests for the position/1 stream property

	test(lgt_stream_property_2_29, true(ground(Position)), [cleanup(close(out))]) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, write, Stream, [reposition(true), alias(out)]),
		{stream_property(Stream, position(Position))}.

	test(lgt_stream_property_2_30, true(ground(Position)), [cleanup(close(out))]) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, append, Stream, [reposition(true), alias(out)]),
		{stream_property(Stream, position(Position))}.

	test(lgt_stream_property_2_31, true(ground(Position)), [cleanup(close(in))]) :-
		os::absolute_file_name(input_text_file, Path),
		^^create_text_file(Path, ''),
		open(Path, read, Stream, [reposition(true), alias(in)]),
		{stream_property(Stream, position(Position))}.

	% tests for the end_of_stream/1 stream property

	test(lgt_stream_property_2_32, true((EndOfStream == not; EndOfStream == at))) :-
		os::absolute_file_name(input_text_file, Path),
		^^create_text_file(Path, ''),
		open(Path, read, Stream),
		{stream_property(Stream, end_of_stream(EndOfStream))},
		close(Stream).

	test(lgt_stream_property_2_33, true((EndOfStream == not; EndOfStream == at))) :-
		os::absolute_file_name(input_binary_file, Path),
		^^create_binary_file(Path, []),
		open(Path, read, Stream, [type(binary)]),
		{stream_property(Stream, end_of_stream(EndOfStream))},
		close(Stream).

	% tests for the eof_action/1 stream property

	test(lgt_stream_property_2_34, true(EOFAction == error)) :-
		os::absolute_file_name(input_text_file, Path),
		^^create_text_file(Path, ''),
		open(Path, read, Stream, [eof_action(error)]),
		{stream_property(Stream, eof_action(EOFAction))},
		close(Stream).

	test(lgt_stream_property_2_35, true(EOFAction == eof_code)) :-
		os::absolute_file_name(input_text_file, Path),
		^^create_text_file(Path, ''),
		open(Path, read, Stream, [eof_action(eof_code)]),
		{stream_property(Stream, eof_action(EOFAction))},
		close(Stream).

	test(lgt_stream_property_2_36, true(EOFAction == reset)) :-
		os::absolute_file_name(input_text_file, Path),
		^^create_text_file(Path, ''),
		open(Path, read, Stream, [eof_action(reset)]),
		{stream_property(Stream, eof_action(EOFAction))},
		close(Stream).

	test(lgt_stream_property_2_37, true((EOFAction == error; EOFAction == eof_code; EOFAction == reset))) :-
		os::absolute_file_name(input_text_file, Path),
		^^create_text_file(Path, ''),
		open(Path, read, Stream),
		{stream_property(Stream, eof_action(EOFAction))},
		close(Stream).

	test(lgt_stream_property_2_38, true(EOFAction == error)) :-
		os::absolute_file_name(input_binary_file, Path),
		^^create_binary_file(Path, []),
		open(Path, read, Stream, [eof_action(error), type(binary)]),
		{stream_property(Stream, eof_action(EOFAction))},
		close(Stream).

	test(lgt_stream_property_2_39, true(EOFAction == eof_code)) :-
		os::absolute_file_name(input_binary_file, Path),
		^^create_binary_file(Path, []),
		open(Path, read, Stream, [eof_action(eof_code), type(binary)]),
		{stream_property(Stream, eof_action(EOFAction))},
		close(Stream).

	test(lgt_stream_property_2_40, true(EOFAction == reset)) :-
		os::absolute_file_name(input_binary_file, Path),
		^^create_binary_file(Path, []),
		open(Path, read, Stream, [eof_action(reset), type(binary)]),
		{stream_property(Stream, eof_action(EOFAction))},
		close(Stream).

	test(lgt_stream_property_2_41, true((EOFAction == error; EOFAction == eof_code; EOFAction == reset))) :-
		os::absolute_file_name(input_binary_file, Path),
		^^create_binary_file(Path, []),
		open(Path, read, Stream, [type(binary)]),
		{stream_property(Stream, eof_action(EOFAction))},
		close(Stream).

	% tests for the reposition/1 stream property

	test(lgt_stream_property_2_42, true(Reposition == true)) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, write, Stream, [reposition(true)]),
		{stream_property(Stream, reposition(Reposition))},
		close(Stream).

	test(lgt_stream_property_2_43, true((Reposition == false; Reposition == true))) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, write, Stream, [reposition(false)]),
		{stream_property(Stream, reposition(Reposition))},
		close(Stream).

	test(lgt_stream_property_2_44, true(Reposition == true)) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, append, Stream, [reposition(true)]),
		{stream_property(Stream, reposition(Reposition))},
		close(Stream).

	test(lgt_stream_property_2_45, true((Reposition == false; Reposition == true))) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, append, Stream, [reposition(false)]),
		{stream_property(Stream, reposition(Reposition))},
		close(Stream).

	test(lgt_stream_property_2_46, true(Reposition == true)) :-
		os::absolute_file_name(input_text_file, Path),
		^^create_text_file(Path, ''),
		open(Path, read, Stream, [reposition(true)]),
		{stream_property(Stream, reposition(Reposition))},
		close(Stream).

	test(lgt_stream_property_2_47, true((Reposition == false; Reposition == true))) :-
		os::absolute_file_name(input_text_file, Path),
		^^create_text_file(Path, ''),
		open(Path, read, Stream, [reposition(false)]),
		{stream_property(Stream, reposition(Reposition))},
		close(Stream).

	% tests for the type/1 stream property

	test(lgt_stream_property_2_48, true(Type == text)) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, write, Stream, [type(text)]),
		{stream_property(Stream, type(Type))},
		close(Stream).

	test(lgt_stream_property_2_49, true(Type == binary)) :-
		os::absolute_file_name(output_binary_file, Path),
		open(Path, write, Stream, [type(binary)]),
		{stream_property(Stream, type(Type))},
		close(Stream).

	test(lgt_stream_property_2_50, true(Type == text)) :-
		os::absolute_file_name(output_text_file, Path),
		open(Path, append, Stream, [type(text)]),
		{stream_property(Stream, type(Type))},
		close(Stream).

	test(lgt_stream_property_2_51, true(Type == binary)) :-
		os::absolute_file_name(output_binary_file, Path),
		open(Path, append, Stream, [type(binary)]),
		{stream_property(Stream, type(Type))},
		close(Stream).

	test(lgt_stream_property_2_52, true(Type == text)) :-
		os::absolute_file_name(input_text_file, Path),
		^^create_text_file(Path, ''),
		open(Path, read, Stream, [type(text)]),
		{stream_property(Stream, type(Type))},
		close(Stream).

	test(lgt_stream_property_2_53, true(Type == binary)) :-
		os::absolute_file_name(input_binary_file, Path),
		^^create_text_file(Path, ''),
		open(Path, read, Stream, [type(binary)]),
		{stream_property(Stream, type(Type))},
		close(Stream).

	cleanup :-
		^^clean_file(foo),
		^^clean_file(bar),
		^^clean_file(output_text_file),
		^^clean_file(input_text_file),
		^^clean_file(output_binary_file),
		^^clean_file(input_binary_file).

	% auxiliary predicates

	memberchk(Element, [Head| _]) :-
		Element == Head,
		!.
	memberchk(Element, [_| Tail]) :-
		memberchk(Element, Tail).

:- end_object.
