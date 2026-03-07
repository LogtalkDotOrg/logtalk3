%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if(\+ current_logtalk_flag(prolog_dialect, quintus)).

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(basic_types(loader)),
		logtalk_load(mutations(loader)),
		logtalk_load(options(loader)),
		logtalk_load(os(loader)),
		logtalk_load(random(loader)),
		logtalk_load([
			subprocess_mutation_hook,
			mutation_testing,
			mutation_testing_messages,
			mutator_protocol,
			mutator_common,
			'mutators/fail_insertion',
			'mutators/predicate_negation',
			'mutators/relational_operator_replacement',
			'mutators/arithmetic_operator_replacement',
			'mutators/truth_literal_flip',
			'mutators/head_arguments_mutation',
			'mutators/head_arguments_reordering',
			'mutators/clause_order_reordering'
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
