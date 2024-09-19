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


% database for tests

o(1).
o(2).
o(3).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:8:0,
		author is 'Paulo Moura',
		date is 2024-09-19,
		comment is 'Unit tests for the ISO Prolog standard op/3 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.14.3.4

	test(iso_op_3_01, true) :-
		{op(30, xfy, ++)},
		{current_op(30, xfy, ++), op(0, xfy, ++)}.

	test(iso_op_3_02, true) :-
		{op(30, xfy, ++), op(0, xfy, ++)},
		{\+ current_op(_, xfy, ++)}.

	test(iso_op_3_03, error(type_error(integer,max))) :-
		{op(max, xfy, ++)}.

	test(iso_op_3_04, error(domain_error(operator_priority,-30))) :-
		{op(-30, xfy, ++)}.

	test(iso_op_3_05, error(domain_error(operator_priority,1201))) :-
		{op(1201, xfy, ++)}.

	test(iso_op_3_06, error(instantiation_error)) :-
		{op(30, _XFY, ++)}.

	test(iso_op_3_07, error(domain_error(operator_specifier,yfy))) :-
		{op(30, yfy, ++)}.

	test(iso_op_3_08, error(type_error(list,0))) :-
		{op(30, xfy, 0)}.

	test(iso_op_3_09, true) :-
		{(op(30, xfy, ++), op(40, xfx, ++))},
		{current_op(40, xfx, ++), op(0, xfx, ++)}.

	:- if((
		current_logtalk_flag(prolog_dialect, Dialect),
		(Dialect == eclipse; Dialect == sicstus; Dialect == swi; Dialect == trealla; Dialect == yap; Dialect == xvm)
	)).
		% these Prolog systems support the definition of an atom as both an infix and a postfix operator
		test(iso_op_3_10, true) :-
			true.
	:- else.
		test(iso_op_3_10, error(permission_error(create,operator,++))) :-
			{op(30, xfy, ++), op(50, yf, ++)}.
	:- endif.

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.14.3.4

	test(iso_op_3_11, error(permission_error(create,operator,[]))) :-
		{op(500, xfy, [])}.

	test(iso_op_3_12, error(permission_error(create,operator,[]))) :-
		{op(500, xfy, [[]])}.

	test(iso_op_3_13, error(permission_error(create,operator,{}))) :-
		{op(500, xfy, {})}.

	test(iso_op_3_14, error(permission_error(create,operator,{}))) :-
		{op(500, xfy, [{}])}.

	test(iso_op_3_15, error(permission_error(create,operator,'|'))) :-
		{op(1000, xfy, '|')}.

	test(iso_op_3_16, error(permission_error(create,operator,'|'))) :-
		{op(1000, xfy, ['|'])}.

	test(iso_op_3_17, error(permission_error(create,operator,'|'))) :-
		{op(1000, fx, '|')}.

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.14.3.1 NOTES

	test(iso_op_3_18, true) :-
		{op(333, xfy, [abc, abc, abc])},
		{current_op(333, xfy, abc)}.

	test(iso_op_3_19, error(permission_error(modify,operator,(',')))) :-
		{op(0, xfx, (','))}.

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.14.4.1 NOTES 1

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(iso_op_3_20, true(Assertion)) :-
			^^set_text_output(''),
			(	{	op(555, fx, o),
					o(X),
					write(o(X)), nl,
					op(555, xf, o),
					fail
				}
			;	^^text_output_assertion('o 1\r\no 2\r\no 3\r\n', Assertion)
			).

	:- else.

		test(iso_op_3_20, true(Assertion)) :-
			^^set_text_output(''),
			(	{	op(555, fx, o),
					o(X),
					write(o(X)), nl,
					op(555, xf, o),
					fail
				}
			;	^^text_output_assertion('o 1\no 2\no 3\n', Assertion)
			).

	:- endif.

	% next test updated for the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 6.3.4.3

	:- if(\+ current_op(_, _, '|')).

		test(iso_op_3_21, true) :-
			{op(1105, xfy, '|')},
			{current_op(Priority, Specifier, '|')},
			Priority == 1105,
			Specifier == xfy.

	:- else.

		test(iso_op_3_21, true) :-
			{current_op(Priority, Specifier, '|')},
			Priority >= 1001,
			(	Specifier == xfy
			;	Specifier == xfx
			;	Specifier == yfx
			).

	:- endif.

	test(iso_op_3_22, true) :-
		{op(0, xfy, '|')},
		{\+ current_op(_, xfy, '|')}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(sics_op_3_23, error(instantiation_error)) :-
		{op(_, xfx, ++)}.

	test(sics_op_3_24, error(instantiation_error)) :-
		{op(100, xfx, _)}.

	test(sics_op_3_25, error(instantiation_error)) :-
		{op(100, xfx, [a|_])}.

	test(sics_op_3_26, error(instantiation_error)) :-
		{op(100, xfx, [a,_])}.

	test(sics_op_3_27, error(type_error(atom,200))) :-
		{op(100, 200, [a])}.

	test(sics_op_3_28, error(type_error(atom,f(1)))) :-
		{op(100, f(1), [a])}.

	test(sics_op_3_29, error(type_error(atom,a+b))) :-
		{op(100, xfx, [a,a+b])}.

	test(sics_op_3_30, error(permission_error(modify,operator,(',')))) :-
		{op(100, xfx, (','))}.

	test(sics_op_3_31, error(permission_error(modify,operator,(',')))) :-
		{op(100, xfx, [a,(',')])}.

	% tests from the Logtalk portability work

	test(lgt_op_3_32, true) :-
		{op(0, fx, foo)}.

	test(lgt_op_3_33, true) :-
		{op(0, fx, bar)},
		{\+ current_op(_, _, bar)}.

	test(lgt_op_3_34, true) :-
		{op(700, xfx, xyz)},
		^^set_text_input('1 xyz 2. '),
		read(T),
		^^assertion(T == xyz(1,2)),
		{op(0, xfx, xyz)},
		^^set_text_input('3 xyz 4. '),
		catch(read(T), Error, true),
		^^assertion(subsumes_term(error(syntax_error(_),_), Error)).

	cleanup :-
		^^clean_text_input.

:- end_object.
