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
		date is 2024-02-11,
		comment is 'Unit tests for the ISO Prolog standard logical update semantics.'
	]).

	:- uses(user, [
		aa/1, az/1, r/1, ra/1, o/1
	]).
	:- dynamic([
		user::aa/1, user::az/1, user::r/1, user::ra/1, user::o/1
	]).

	% tests for the ISO/IEC 13211-1:1995(E) standard section 7.5.4

	setup :-
		retractall(aa(_)), assertz(aa(1)), assertz(aa(2)), assertz(aa(3)),
		retractall(az(_)), assertz(az(1)), assertz(az(2)), assertz(az(3)),
		retractall(r(_)),  assertz(r(1)),  assertz(r(2)),  assertz(r(3)),
		retractall(ra(_)), assertz(ra(1)), assertz(ra(2)), assertz(ra(3)),
		retractall(o(_)),  assertz(o(1)),  assertz(o(2)),  assertz(o(3)).

	test(logical_update_semantics_asserta, true(Assertion)) :-
		^^set_text_output(''),
		(	aa(X),
			write(X),
			asserta(aa(4)),
			asserta(aa(5)),
			fail
		;	^^text_output_assertion('123', Assertion)
		).

	test(logical_update_semantics_assertz, true(Assertion)) :-
		^^set_text_output(''),
		(	az(X),
			write(X),
			assertz(az(4)),
			assertz(az(5)),
			fail
		;	^^text_output_assertion('123', Assertion)
		).

	test(logical_update_semantics_retract, true(Assertion)) :-
		^^set_text_output(''),
		(	r(X),
			write(X),
			retract(r(_)),
			fail
		;	^^text_output_assertion('123', Assertion)
		).

	test(logical_update_semantics_retractall, true(Assertion)) :-
		^^set_text_output(''),
		(	ra(X),
			write(X),
			retractall(ra(_)),
			fail
		;	^^text_output_assertion('123', Assertion)
		).

	% for operators, see also the ISO/IEC 13211-1:1995(E) standard section 8.14.4.1 NOTES 1

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(logical_update_semantics_operators, true(Assertion)) :-
			^^set_text_output(''),
			(	op(555, fx, o),
				o(X),
				write(o(X)), nl,
				op(555, xf, o),
				fail
			;	^^text_output_assertion('o 1\r\no 2\r\no 3\r\n', Assertion)
			).

	:- else.

		test(logical_update_semantics_operators, true(Assertion)) :-
			^^set_text_output(''),
			(	op(555, fx, o),
				o(X),
				write(o(X)), nl,
				op(555, xf, o),
				fail
			;	^^text_output_assertion('o 1\no 2\no 3\n', Assertion)
			).

	:- endif.

:- end_object.
