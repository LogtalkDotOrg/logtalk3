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


:- object(tests_io_predicates,
	extends(lgtunit)).

	:- info([
		version is 0:5:0,
		author is 'Paulo Moura',
		date is 2021-05-17,
		comment is 'Unit tests for the "lgtunit" tool input/output testing predicates.'
	]).

	% suppress_text_output/0 tests

	test(suppress_text_output_0_01, deterministic) :-
		^^suppress_text_output.

	% suppress_binary_output/0 tests

	test(suppress_binary_output_0_01, deterministic) :-
		^^suppress_binary_output.

	% set_text_output/1 + check_text_output/1 + clean_text_output/0 tests

	test(text_output_01, deterministic) :-
		^^set_text_output(foo),
		^^check_text_output(foo),
		^^clean_text_output.

	% set_text_output/2 + check_text_output/2 + clean_text_output/0 tests

	test(text_output_02, deterministic) :-
		^^set_text_output(alias, foo),
		^^check_text_output(alias, foo),
		^^clean_text_output.

	% set_text_output/1 + text_output_assertion/2 + clean_text_output/0 tests

	test(text_output_03, deterministic(Assertion)) :-
		^^set_text_output(foo),
		^^text_output_assertion(foo, Assertion),
		^^clean_text_output.

	% set_text_output/2 + text_output_assertion/3 + clean_text_output/0 tests

	test(text_output_04, deterministic(Assertion)) :-
		^^set_text_output(alias, foo),
		^^text_output_assertion(alias, foo, Assertion),
		^^clean_text_output.

	% set_binary_output/1 + check_binary_output/1 + clean_text_output/0 tests

	test(binary_output_01, deterministic) :-
		^^set_binary_output([65,66,67]),
		^^check_binary_output([65,66,67]),
		^^clean_binary_output.

	% set_binary_output/2 + check_binary_output/2 + clean_binary_output/0 tests

	test(binary_output_02, deterministic) :-
		^^set_binary_output(alias, [65,66,67]),
		^^check_binary_output(alias, [65,66,67]),
		^^clean_binary_output.

	% set_binary_output/1 + binary_output_assertion/2 + clean_binary_output/0 tests

	test(binary_output_03, deterministic(Assertion)) :-
		^^set_binary_output([65,66,67]),
		^^binary_output_assertion([65,66,67], Assertion),
		^^clean_binary_output.

	% set_binary_output/2 + binary_output_assertion/3 + clean_text_output/0 tests

	test(binary_output_04, deterministic(Assertion)) :-
		^^set_binary_output(alias, [65,66,67]),
		^^binary_output_assertion(alias, [65,66,67], Assertion),
		^^clean_binary_output.

	% set_text_input/1 + check_text_input/1 + clean_text_input/0 tests

	test(text_input_01, deterministic) :-
		^^set_text_input(foo),
		^^check_text_input(foo),
		^^clean_text_input.

	% set_text_input/2 + check_text_input/2 + clean_text_input/0 tests

	test(text_input_02, deterministic) :-
		^^set_text_input(alias, foo),
		^^check_text_input(alias, foo),
		^^clean_text_input.

	% set_text_input/1 + text_input_assertion/2 + clean_text_input/0 tests

	test(text_input_03, deterministic(Assertion)) :-
		^^set_text_input(foo),
		^^text_input_assertion(foo, Assertion),
		^^clean_text_input.

	% set_text_input/2 + text_input_assertion/3 + clean_text_input/0 tests

	test(text_input_04, deterministic(Assertion)) :-
		^^set_text_input(alias, foo),
		^^text_input_assertion(alias, foo, Assertion),
		^^clean_text_input.

	% set_binary_input/1 + check_binary_input/1 + clean_binary_input/0 tests

	test(binary_input_01, deterministic) :-
		^^set_binary_input([65,66,67]),
		^^check_binary_input([65,66,67]),
		^^clean_binary_input.

	% set_binary_input/2 + check_binary_input/2 + clean_binary_input/0 tests

	test(binary_input_02, deterministic) :-
		^^set_binary_input(alias, [65,66,67]),
		^^check_binary_input(alias, [65,66,67]),
		^^clean_binary_input.

	% set_binary_input/1 + binary_input_assertion/2 + clean_binary_input/0 tests

	test(binary_input_03, deterministic(Assertion)) :-
		^^set_binary_input([65,66,67]),
		^^binary_input_assertion([65,66,67], Assertion),
		^^clean_binary_input.

	% set_binary_input/2 + binary_input_assertion/3 + clean_binary_input/0 tests

	test(binary_input_04, deterministic(Assertion)) :-
		^^set_binary_input(alias, [65,66,67]),
		^^binary_input_assertion(alias, [65,66,67], Assertion),
		^^clean_binary_input.

	% create_text_file/2 tests

	test(create_text_file_2_01, deterministic(Term == foo42)) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, foo42, File),
		^^create_text_file(File, 'foo42.'),
		open(File, read, Stream, [type(text)]),
		read_term(Stream, Term, []),
		^^clean_file(File).

	% create_binary_file/2 tests

	test(create_binary_file_2_01, deterministic([Byte1,Byte2,Byte3] == [65,66,67])) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, foo42, File),
		^^create_binary_file(File, [65,66,67]),
		open(File, read, Stream, [type(binary)]),
		get_byte(Stream, Byte1),
		get_byte(Stream, Byte2),
		get_byte(Stream, Byte3),
		^^clean_file(File).

	% check_text_file/2 tests

	test(check_text_file_2_01, deterministic) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, foo42, File),
		^^create_text_file(File, 'foo42.'),
		^^check_text_file(File, 'foo42.'),
		^^clean_file(File).

	test(check_text_file_2_02, false) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, foo42, File),
		^^create_text_file(File, 'foo42.'),
		^^check_text_file(File, 'foo24.').

	% text_file_assertion/3

	test(text_file_assertion_3_01, deterministic(Assertion)) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, foo42, File),
		^^create_text_file(File, 'foo42.'),
		^^text_file_assertion(File, 'foo42.', Assertion),
		^^clean_file(File).

	test(text_file_assertion_3_02, true(\+ Assertion)) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, foo42, File),
		^^create_text_file(File, 'foo42.'),
		^^text_file_assertion(File, 'foo24.', Assertion),
		^^clean_file(File).

	% check_binary_file/2 tests

	test(check_binary_file_2_01, deterministic) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, foo42, File),
		^^create_binary_file(File, [65,66,67]),
		^^check_binary_file(File, [65,66,67]),
		^^clean_file(File).

	test(check_binary_file_2_02, false) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, foo42, File),
		^^create_binary_file(File, [65,66,67]),
		^^check_binary_file(File, [68,69,70]).

	% binary_file_assertion/3

	test(binary_file_assertion_3_01, deterministic(Assertion)) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, foo42, File),
		^^create_binary_file(File, [65,66,67]),
		^^binary_file_assertion(File, [65,66,67], Assertion),
		^^clean_file(File).

	test(binary_file_assertion_3_02, true(\+ Assertion)) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, foo42, File),
		^^create_binary_file(File, [65,66,67]),
		^^binary_file_assertion(File, [68,69,70], Assertion),
		^^clean_file(File).

	% clean_file/1 tests

	test(clean_file_1_01, deterministic(\+ os::file_exists(File))) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, foo42, File),
		open(File, write, _),
		^^clean_file(File).

	% closed_input_stream/2 tests

	test(closed_input_stream_2_01, deterministic(ground(Handle))) :-
		^^closed_input_stream(Handle, []).

	% closed_output_stream/2 tests

	test(closed_output_stream_2_01, deterministic(ground(Handle))) :-
		^^closed_output_stream(Handle, []).

	% stream_position/1 tests

	test(stream_position_1_01, deterministic(ground(Position))) :-
		^^stream_position(Position).

	cleanup :-
		catch(ignore(os::delete_file(foo42)), _, true).

:- end_object.
