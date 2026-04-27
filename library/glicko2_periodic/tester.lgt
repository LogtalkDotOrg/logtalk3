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
