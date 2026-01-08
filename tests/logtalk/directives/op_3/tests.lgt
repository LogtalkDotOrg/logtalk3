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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2023-08-17,
		comment is 'Unit tests for the op/3 built-in directive.'
	]).

	:- private(op(501, xfx, foo)).
	:- private(op(601, xfy, [bar, baz])).

	test(op_3_01, true(Priority-Specifier == 501-xfx)) :-
		current_op(Priority, Specifier, foo).

	test(op_3_02, true(Priority-Specifier == 601-xfy)) :-
		current_op(Priority, Specifier, bar).

	test(op_3_03, true(Priority-Specifier == 601-xfy)) :-
		current_op(Priority, Specifier, baz).

	test(op_3_04, true(Assertion)) :-
		^^set_text_output(''),
		obj::wt(<=>(1,2)),
		^^text_output_assertion('1<=>2', Assertion).

	test(op_3_05, true(Assertion)) :-
		^^set_text_output(''),
		obj::wq(<=>(1,2)),
		^^text_output_assertion('1<=>2', Assertion).

	test(op_3_06, true(Assertion)) :-
		^^set_text_output(''),
		obj::w(<=>(1,2)),
		^^text_output_assertion('1<=>2', Assertion).

	test(op_3_07, true(Term == <=>(1,2))) :-
		^^set_text_input('1<=>2. '),
		obj::rt(Term).

	test(op_3_08, true(Term == <=>(1,2))) :-
		^^set_text_input('1<=>2. '),
		obj::r(Term).

	cleanup :-
		^^clean_text_input,
		^^clean_text_output.

:- end_object.
