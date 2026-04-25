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
	logtalk_load(dictionaries(loader)),
	logtalk_load(format(loader)),
	logtalk_load(options(loader)),
	logtalk_load(ranking_protocols(loader)),
	logtalk_load([
		ranking_protocols('test_datasets/ranked_ballots'),
		ranking_protocols('test_datasets/tied_grouped'),
		ranking_protocols('test_datasets/sparse_grouped_relevance'),
		ranking_protocols('test_datasets/reordered_grouped_items'),
		ranking_protocols('test_datasets/search_results'),
		ranking_protocols('test_datasets/malformed_grouped')
	], [
		source_data(on),
		debug(on)
	]),
	logtalk_load(plackett_luce_last, [source_data(on), debug(on)]),
	logtalk_load(lgtunit(loader)),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
