%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 0.1,
		author is 'Paulo Moura',
		date is 2018/03/04,
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

	% closed_input_stream/2 tests

	test(closed_input_stream_2_01, deterministic) :-
		^^closed_input_stream(Handle, []),
		ground(Handle).

	% closed_output_stream/2 tests

	test(closed_output_stream_2_01, deterministic) :-
		^^closed_output_stream(Handle, []),
		ground(Handle).

	% stream_position/1 tests

	test(stream_position_1_01, deterministic) :-
		^^stream_position(Position),
		ground(Position).

:- end_object.
