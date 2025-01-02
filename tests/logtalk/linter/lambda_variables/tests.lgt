%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		date is 2024-08-15,
		comment is 'Unit tests for the ``lambda_variables`` linter flag.'
	]).

	:- uses(lgtunit, [
		variant/2
	]).

	:- private(unclassified_variables_in_lambda_expression/6).
	:- dynamic(unclassified_variables_in_lambda_expression/6).

	:- private(variables_with_dual_role_in_lambda_expression/6).
	:- dynamic(variables_with_dual_role_in_lambda_expression/6).

	:- private(parameter_variable_used_elsewhere/6).
	:- dynamic(parameter_variable_used_elsewhere/6).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [lambda_variables(warning)]).

	cleanup :-
		retractall(unclassified_variables_in_lambda_expression(_, _, _, _, _, _)),
		retractall(variables_with_dual_role_in_lambda_expression(_, _, _, _, _, _)),
		retractall(parameter_variable_used_elsewhere(_, _, _, _, _, _)).

	test(lambda_variables_linter_flag_01, variant(UnclassifiedVars-Lambda, [B,C]-{D}/qux(D,B,C))) :-
		unclassified_variables_in_lambda_expression(_, _, object, lambdas(_), UnclassifiedVars, Lambda).

	test(lambda_variables_linter_flag_02, variant(MixedUpVars-Lambda, [B]-{B}/[B]>>qux(B))) :-
		variables_with_dual_role_in_lambda_expression(_, _, object, lambdas(_), MixedUpVars, Lambda).

	test(lambda_variables_linter_flag_03, variant(Lambda-Variable, [B]>>qux(B)-B)) :-
		parameter_variable_used_elsewhere(_, _, object, lambdas(_), Lambda, Variable).

	test(lambda_variables_linter_flag_04, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(unclassified_variables_in_lambda_expression(file, 1-2, object, disjunctions, [A], [B]>>foo(A,B)), core), Tokens).

	test(lambda_variables_linter_flag_05, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(variables_with_dual_role_in_lambda_expression(file, 1-2, object, disjunctions, [A], {A}/[A]>>foo(A)), core), Tokens).

	test(lambda_variables_linter_flag_06, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(parameter_variable_used_elsewhere(file, 1-2, object, disjunctions, [B]>>foo(B), B), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(unclassified_variables_in_lambda_expression(File, Lines, Type, Entity, UnclassifiedVars, Lambda), warning(lambda_variables), core, _) :-
		assertz(unclassified_variables_in_lambda_expression(File, Lines, Type, Entity, UnclassifiedVars, Lambda)).
	logtalk::message_hook(variables_with_dual_role_in_lambda_expression(File, Lines, Type, Entity, MixedUpVars, Lambda), warning(lambda_variables), core, _) :-
		assertz(variables_with_dual_role_in_lambda_expression(File, Lines, Type, Entity, MixedUpVars, Lambda)).
	logtalk::message_hook(parameter_variable_used_elsewhere(File, Lines, Type, Entity, Lambda, Variable), warning(lambda_variables), core, _) :-
		assertz(parameter_variable_used_elsewhere(File, Lines, Type, Entity, Lambda, Variable)).

:- end_object.
