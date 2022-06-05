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
		version is 1:8:0,
		author is 'Paulo Moura',
		date is 2022-06-05,
		comment is 'Unit tests for the ISO Prolog standard current_op/3 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.14.4.4

	test(iso_current_op_3_01, exists(P-T == 1200-xfx)) :-
		{current_op(P, T, ':-')}.

	test(iso_current_op_3_02, exists(P-T == 1200-xfx)) :-
		{current_op(P, T, '-->')}.

	test(iso_current_op_3_03, exists(P-T == 1200-fx)) :-
		{current_op(P, T, ':-')}.

	test(iso_current_op_3_04, exists(P-T == 1200-fx)) :-
		{current_op(P, T, '?-')}.

	test(iso_current_op_3_05, exists(P-T == 1100-xfy)) :-
		{current_op(P, T, ';')}.

	test(iso_current_op_3_06, exists(P-T == 1050-xfy)) :-
		{current_op(P, T, '->')}.

	test(iso_current_op_3_07, exists(P-T == 1000-xfy)) :-
		{current_op(P, T, ',')}.

	test(iso_current_op_3_08, exists(P-T == 900-fy)) :-
		{current_op(P, T, '\\+')}.

	test(iso_current_op_3_09, exists(P-T == 700-xfx)) :-
		{current_op(P, T, '=')}.

	test(iso_current_op_3_10, exists(P-T == 700-xfx)) :-
		{current_op(P, T, '\\=')}.

	test(iso_current_op_3_11, exists(P-T == 700-xfx)) :-
		{current_op(P, T, '==')}.

	test(iso_current_op_3_12, exists(P-T == 700-xfx)) :-
		{current_op(P, T, '\\==')}.

	test(iso_current_op_3_13, exists(P-T == 700-xfx)) :-
		{current_op(P, T, '@<')}.

	test(iso_current_op_3_14, exists(P-T == 700-xfx)) :-
		{current_op(P, T, '@=<')}.

	test(iso_current_op_3_15, exists(P-T == 700-xfx)) :-
		{current_op(P, T, '@>')}.

	test(iso_current_op_3_16, exists(P-T == 700-xfx)) :-
		{current_op(P, T, '@>=')}.

	test(iso_current_op_3_17, exists(P-T == 700-xfx)) :-
		{current_op(P, T, '=..')}.

	test(iso_current_op_3_18, exists(P-T == 700-xfx)) :-
		{current_op(P, T, 'is')}.

	test(iso_current_op_3_19, exists(P-T == 700-xfx)) :-
		{current_op(P, T, '=:=')}.

	test(iso_current_op_3_20, exists(P-T == 700-xfx)) :-
		{current_op(P, T, '=\\=')}.

	test(iso_current_op_3_21, exists(P-T == 700-xfx)) :-
		{current_op(P, T, '<')}.

	test(iso_current_op_3_22, exists(P-T == 700-xfx)) :-
		{current_op(P, T, '=<')}.

	test(iso_current_op_3_23, exists(P-T == 700-xfx)) :-
		{current_op(P, T, '>')}.

	test(iso_current_op_3_24, exists(P-T == 700-xfx)) :-
		{current_op(P, T, '>=')}.

	test(iso_current_op_3_25, exists(P-T == 500-yfx)) :-
		{current_op(P, T, '+')}.

	test(iso_current_op_3_26, exists(P-T == 500-yfx)) :-
		{current_op(P, T, '-')}.

	test(iso_current_op_3_27, exists(P-T == 500-yfx)) :-
		{current_op(P, T, '/\\')}.

	test(iso_current_op_3_28, exists(P-T == 500-yfx)) :-
		{current_op(P, T, '\\/')}.

	test(iso_current_op_3_29, exists(P-T == 400-yfx)) :-
		{current_op(P, T, '*')}.

	test(iso_current_op_3_30, exists(P-T == 400-yfx)) :-
		{current_op(P, T, '/')}.

	test(iso_current_op_3_31, exists(P-T == 400-yfx)) :-
		{current_op(P, T, '//')}.

	test(iso_current_op_3_32, exists(P-T == 400-yfx)) :-
		{current_op(P, T, 'rem')}.

	test(iso_current_op_3_33, exists(P-T == 400-yfx)) :-
		{current_op(P, T, 'mod')}.

	test(iso_current_op_3_34, exists(P-T == 400-yfx)) :-
		{current_op(P, T, '<<')}.

	test(iso_current_op_3_35, exists(P-T == 400-yfx)) :-
		{current_op(P, T, '>>')}.

	test(iso_current_op_3_36, exists(P-T == 200-xfx)) :-
		{current_op(P, T, '**')}.

	test(iso_current_op_3_37, exists(P-T == 200-xfy)) :-
		{current_op(P, T, '^')}.

	test(iso_current_op_3_38, exists(P-T == 200-fy)) :-
		{current_op(P, T, '-')}.

	test(iso_current_op_3_39, exists(P-T == 200-fy)) :-
		{current_op(P, T, '\\')}.

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.14.4.1 NOTES

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(iso_current_op_3_40, true(Assertion)) :-
			^^set_text_output(''),
			{	op(333, fx, foo),
				(T = 1; T = 2; T = 3),
				write(foo(T)), nl,
				op(0, fx, foo),
				fail
			;	true
			},
			^^text_output_assertion('foo 1\r\nfoo 2\r\nfoo 3\r\n', Assertion).

	:- else.

		test(iso_current_op_3_40, true(Assertion)) :-
			^^set_text_output(''),
			{	op(333, fx, foo),
				(T = 1; T = 2; T = 3),
				write(foo(T)), nl,
				op(0, fx, foo),
				fail
			;	true
			},
			^^text_output_assertion('foo 1\nfoo 2\nfoo 3\n', Assertion).

	:- endif.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(sics_current_op_3_41, error(domain_error(operator_priority,1201))) :-
		{current_op(1201, _, _)}.

	test(sics_current_op_3_42, error(domain_error(operator_specifier,yfy))) :-
		{current_op(_, yfy, _)}.

	test(sics_current_op_3_43, errors([type_error(atom,0), domain_error(operator_specifier,0)])) :-
		% the standard specifies a domain_error(operator_specifier,0) for this case
		% but domain errors imply that the type is correct, which is not the case here
		{current_op(_, 0, _)}.

	test(sics_current_op_3_44, error(type_error(atom,5))) :-
		{current_op(_, _, 5)}.

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.14.4.4

	test(iso_current_op_3_45, true) :-
		(	{current_op(P, T, '|')} ->
			P >= 1001, infix(T)
		;	true
		).

	test(iso_current_op_3_46, exists(P-T == 400-yfx)) :-
		{current_op(P, T, div)}.

	test(iso_current_op_3_47, exists(P-T == 200-fy)) :-
		{current_op(P, T, '+')}.

	% tests from the Logtalk portability work

	test(lgt_current_op_3_48, true(PTs == [200-fy, 500-yfx])) :-
		setof(P-T, {current_op(P, T, '-')}, PTs).

	test(lgt_current_op_3_49, errors([type_error(integer,a), domain_error(operator_priority,a)])) :-
		% the standard specifies a domain_error(operator_priority,a) for this case
		% but domain errors imply that the type is correct, which is not the case here
		{current_op(a, _, _)}.

	test(lgt_current_op_3_50, false) :-
		{current_op(0, _, _)}.

	test(lgt_current_op_3_51, false) :-
		{	op(0, xfx, foo),
			current_op(0, _, _)
		}.

	test(lgt_current_op_3_52, false) :-
		{	op(0, xfx, foo),
			current_op(0, xfx, foo)
		}.

	infix(yfx).
	infix(xfx).
	infix(xfy).

:- end_object.
