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
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2020-12-14,
		comment is 'Unit tests for the ISO Prolog standard set_stream_position/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.9

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(sics_set_stream_position_2_01, error(instantiation_error)) :-
		^^stream_position(Position),
		{set_stream_position(_Stream, Position)}.

	test(sics_set_stream_position_2_02, error(instantiation_error)) :-
		% the original test used the current input stream but this results in a test that
		% can trigger two different errors depending on the order of argument checking
		% {current_input(S)},
		^^set_text_input(st_i, '', [reposition(true)]),
		{set_stream_position(st_i, _Position)}.

	test(sics_set_stream_position_2_03, errors([domain_error(stream_or_alias,foo), existence_error(stream,foo)])) :-
		% both exception terms seem to be acceptable in the ISO spec
		^^stream_position(Position),
		{set_stream_position(foo, Position)}.

	test(sics_set_stream_position_2_04, error(existence_error(stream,Stream))) :-
		^^stream_position(Position),
		^^closed_output_stream(Stream, []),
		{set_stream_position(Stream, Position)}.

	test(sics_set_stream_position_2_05, error(domain_error(stream_position,foo))) :-
		% the original test used the current input stream but this results in a test that
		% can trigger two different errors depending on the order of argument checking
		% {current_input(S)},
		^^set_text_input(st_i, '', [reposition(true)]),
		{set_stream_position(st_i, foo)}.

	test(sics_set_stream_position_2_06, error(permission_error(reposition,stream,Stream))) :-
		os::absolute_file_name('terms.pl', Path),
		open(Path, read, Stream, [reposition(false)]),
		stream_property(Stream, position(Position)),
		{set_stream_position(Stream, Position)}.

	% tests from the Logtalk portability work

	test(lgt_set_stream_position_2_07, true(Term1 == Term3)) :-
		os::absolute_file_name('terms.pl', Path),
		open(Path, read, Stream, [type(text), reposition(true)]),
		stream_property(Stream, position(Position)),
		read_term(Stream, Term1, []),
		read_term(Stream, _, []),
		{set_stream_position(Stream, Position)},
		read_term(Stream, Term3, []).		

	test(lgt_set_stream_position_2_08, true(Byte1 == Byte3)) :-
		os::absolute_file_name('bytes.pl', Path),
		open(Path, read, Stream, [type(binary), reposition(true)]),
		stream_property(Stream, position(Position)),
		get_byte(Stream, Byte1),
		get_byte(Stream, _),
		{set_stream_position(Stream, Position)},
		get_byte(Stream, Byte3).		

	cleanup :-
		^^clean_text_input,
		^^clean_file(foo).

:- end_object.
