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
		ranking_protocols('test_datasets/temporal_two_period_chain'),
		ranking_protocols('test_datasets/temporal_draws'),
		ranking_protocols('test_datasets/temporal_idle_periods'),
		ranking_protocols('test_datasets/temporal_late_activity'),
		ranking_protocols('test_datasets/disconnected_temporal_pairwise'),
		ranking_protocols('test_datasets/malformed_duplicate_periods'),
		ranking_protocols('test_datasets/malformed_temporal_unknown_period'),
		ranking_protocols('test_datasets/malformed_temporal_unknown_item'),
		ranking_protocols('test_datasets/malformed_temporal_self_game'),
		ranking_protocols('test_datasets/malformed_temporal_illegal_score')
	], [
		source_data(on),
		debug(on)
	]),
	logtalk_load(glicko2_periodic, [source_data(on), debug(on)]),
	logtalk_load(lgtunit(loader)),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
