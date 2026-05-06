%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(types(loader)),
	logtalk_load(format(loader)),
	logtalk_load(ranking_protocols(loader)),
	logtalk_load([
		ranking_protocols('test_datasets/regular_head_to_head'),
		ranking_protocols('test_datasets/head_to_head'),
		ranking_protocols('test_datasets/cyclic_pairwise'),
		ranking_protocols('test_datasets/condorcet_divergence_pairwise'),
		ranking_protocols('test_datasets/malformed_pairwise'),
		ranking_protocols('test_datasets/disconnected_pairwise'),
		ranking_protocols('test_datasets/malformed_duplicate_items'),
		ranking_protocols('test_datasets/malformed_self_preference'),
		ranking_protocols('test_datasets/malformed_non_positive_weight')
	], [
		source_data(on),
		debug(on)
	]),
	logtalk_load(kemeny_young_ranker, [source_data(on), debug(on)]),
	logtalk_load(lgtunit(loader)),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
