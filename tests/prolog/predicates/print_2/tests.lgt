%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2024-10-28,
		comment is 'Unit tests for the Prolog the facto standard print/2 built-in predicate.'
	]).

	condition :-
		predicate_property(print(_), built_in).

	test(lgt_print_2_01, true(Assertion)) :-
		^^set_text_output(out, ''),
		{print(out, 42)},
		^^text_output_assertion(out, '42', Assertion).

	test(lgt_print_2_02, true(Assertion)) :-
		^^set_text_output(out, ''),
		{print(out, foo)},
		^^text_output_assertion(out, foofoo, Assertion).

	test(lgt_print_2_03, true(Assertion)) :-
		^^set_text_output(out, ''),
		{print(out, a(foo))},
		^^text_output_assertion(out, 'a(foofoo)', Assertion).

	test(lgt_print_2_04, true(Assertion)) :-
		^^set_text_output(out, ''),
		{print(out, a(foo,b(c(foo))))},
		^^text_output_assertion(out, 'a(foofoo,b(c(foofoo)))', Assertion).

	test(lgt_print_2_05, error(instantiation_error)) :-
		{print(_, foo)}.

	test(lgt_print_2_06, errors([domain_error(stream_or_alias,foo), existence_error(stream,foo)])) :-
		{print(foo, 42)}.

	test(lgt_print_2_07, error(existence_error(stream,S))) :-
		^^closed_input_stream(S, []),
		{print(S, 42)}.

	test(lgt_print_2_08, error(existence_error(stream,S))) :-
		^^closed_output_stream(S, []),
		{print(S, 42)}.

	test(lgt_print_2_09, error(permission_error(output,stream,S))) :-
		current_input(S),
		{print(S, 42)}.

	test(lgt_print_2_10, error(permission_error(output,binary_stream,S))) :-
		^^set_binary_output([]),
		current_output(S),
		{print(S, 42)}.

	test(lgt_print_2_11, error(permission_error(output,stream,s))) :-
		^^set_text_input(s, ''),
		{print(s, a)}.

	test(lgt_print_2_12, error(permission_error(output,binary_stream,_))) :-
		^^set_binary_output(s, []),
		{print(s, a)}.

:- end_object.
