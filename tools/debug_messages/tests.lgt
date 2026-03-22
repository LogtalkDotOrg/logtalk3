%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


% compiling in optimal mode would suppress all
% logtalk::print_message/2 goals from the code
:- set_logtalk_flag(optimize, off).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2026-03-21,
		comment is 'Unit tests for the "debug_messages" tool.'
	]).

	:- uses(debug_messages, [
		enable/1, disable/1, enabled/1,
		enable/2, disable/2, enabled/2
	]).

	:- uses(logtalk, [
		print_message/3
	]).

	cover(debug_messages).

	setup :-
		logtalk::asserta(message_prefix_stream(debug,    _, '', test_output)),
		logtalk::asserta(message_prefix_stream(debug(_), _, '', test_output)).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(debug_messages_enable_1_01, true(Assertion)) :-
			enable(foo),
			^^set_text_output(test_output, ''),
			print_message(debug, foo, @debug_messages_enable_1_01),
			^^text_output_assertion(test_output, 'debug_messages_enable_1_01\r\n', Assertion).

		test(debug_messages_enable_1_02, true(Assertion)) :-
			enable(foo),
			^^set_text_output(test_output, ''),
			print_message(debug(group), foo, @debug_messages_enable_1_02),
			^^text_output_assertion(test_output, 'debug_messages_enable_1_02\r\n', Assertion).

	:- else.

		test(debug_messages_enable_1_01, true(Assertion)) :-
			enable(foo),
			^^set_text_output(test_output, ''),
			print_message(debug, foo, @debug_messages_enable_1_01),
			^^text_output_assertion(test_output, 'debug_messages_enable_1_01\n', Assertion).

		test(debug_messages_enable_1_02, true(Assertion)) :-
			enable(foo),
			^^set_text_output(test_output, ''),
			print_message(debug(group), foo, @debug_messages_enable_1_02),
			^^text_output_assertion(test_output, 'debug_messages_enable_1_02\n', Assertion).

	:- endif.

	test(debug_messages_disable_1_01, true(Assertion)) :-
		disable(foo),
		^^set_text_output(test_output, ''),
		print_message(debug, foo, @debug_messages_disable_1_01),
		^^text_output_assertion(test_output, '', Assertion).

	test(debug_messages_enabled_1_01, true(Components == [])) :-
		disable(_),
		findall(Component, enabled(Component), Components).

	test(debug_messages_enabled_1_02, true(Components == [bar,foo])) :-
		disable(_),
		enable(bar),
		enable(foo),
		findall(Component, enabled(Component), Components).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(debug_messages_enable_2_01, true(Assertion)) :-
			enable(baz, group),
			^^set_text_output(test_output, ''),
			print_message(debug(group), baz, @debug_messages_enable_2_01),
			^^text_output_assertion(test_output, 'debug_messages_enable_2_01\r\n', Assertion).

	:- else.

		test(debug_messages_enable_2_01, true(Assertion)) :-
			enable(baz, group),
			^^set_text_output(test_output, ''),
			print_message(debug(group), baz, @debug_messages_enable_2_01),
			^^text_output_assertion(test_output, 'debug_messages_enable_2_01\n', Assertion).

	:- endif.

	test(debug_messages_enable_2_02, true(Assertion)) :-
		enable(baz, group),
		^^set_text_output(test_output, ''),
		print_message(debug(other), baz, @debug_messages_enable_2_02),
		^^text_output_assertion(test_output, '', Assertion).

	test(debug_messages_disable_2_01, true(Assertion)) :-
		disable(baz, group),
		^^set_text_output(test_output, ''),
		print_message(debug(group), baz, @debug_messages_disable_2_01),
		^^text_output_assertion(test_output, '', Assertion).

	test(debug_messages_enabled_2_01, true(List == [])) :-
		disable(_, _),
		findall(Component-Group, enabled(Component, Group), List).

	test(debug_messages_enabled_2_02, true(List == [baz-group,qux-group])) :-
		disable(_, _),
		enable(baz, group),
		enable(qux, group),
		findall(Component-Group, enabled(Component, Group), List).

:- end_object.
