%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- if(\+ current_logtalk_flag(prolog_dialect, quintus)).

	:- if(current_logtalk_flag(prolog_dialect, sicstus)).
		:- use_module(library(process), []).
	:- endif.

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(basic_types(loader)),
		logtalk_load(json(loader)),
		logtalk_load(json_schema(loader)),
		logtalk_load(mutations(loader)),
		logtalk_load(options(loader)),
		logtalk_load(os(loader)),
		logtalk_load(random(loader)),
		logtalk_load(term_io(loader)),
		logtalk_load([
			subprocess_mutation_hook,
			mutation_testing,
			mutation_testing_messages,
			mutator_protocol,
			clause_mutator_protocol,
			directive_mutator_protocol,
			mutator_common,
			'mutators/fail_insertion',
			'mutators/body_goal_negation',
			'mutators/relational_operator_replacement',
			'mutators/arithmetic_operator_replacement',
			'mutators/truth_literal_flip',
			'mutators/head_arguments_mutation',
			'mutators/head_arguments_reordering',
			'mutators/clauses_reordering',
			'mutators/scope_directive_replacement',
			'mutators/predicate_directive_suppression',
			'mutators/uses_directive_resource_deletion'
		], [
			source_data(on),
			debug(on)
		]),
		logtalk_load(lgtunit(loader)),
		logtalk_load('test_files/test_entities', [source_data(on)]),
		logtalk_load('test_files/tests', [hook(lgtunit), optimize(on)]),
		tests::run
	)).

:- else.

	:- initialization((write('(not applicable)'), nl)).

:- endif.
