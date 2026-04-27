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
	logtalk_load(types(loader)),
	logtalk_load(format(loader)),
	logtalk_load(dictionaries(loader)),
	logtalk_load(options(loader)),
	logtalk_load([
		ranker_protocol,
		ranker_common,
		score_ranker_common,
		score_ranker_model_common,
		glicko2_common,
		condorcet_victory_common,
		grouped_strength_ranker_common,
		plackett_luce_common,
		pairwise_strength_ranker_common,
		ranking_dataset_common,
		ranking_dataset_protocol,
		pairwise_ranking_dataset_protocol,
		pairwise_measurement_dataset_protocol,
		temporal_pairwise_ranking_dataset_protocol
	], [
		optimize(on)
	])
)).
