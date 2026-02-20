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
	logtalk_load(basic_types(loader)),
	logtalk_load(dictionaries(loader)),
	logtalk_load(sets(loader)),
	logtalk_load(lgtunit(loader)),
	logtalk_load([
		graph_types,
		graph_protocol,
		unweighted_graph_protocol,
		weighted_graph_protocol,
		directed_graph_protocol,
		graph_common,
		directed_graph_common,
		undirected_graph_common,
		unweighted_directed_graph,
		unweighted_undirected_graph,
		weighted_directed_graph,
		weighted_undirected_graph
	], [
		debug(on), source_data(on)
	]),
	logtalk_load([
		tests_unweighted_directed_graph,
		tests_unweighted_undirected_graph,
		tests_weighted_directed_graph,
		tests_weighted_undirected_graph
	], [
		hook(lgtunit)
	]),
	lgtunit::run_test_sets([
		tests_unweighted_directed_graph(avltree),
		tests_unweighted_undirected_graph(avltree),
		tests_weighted_directed_graph(avltree),
		tests_weighted_undirected_graph(avltree)
	])
)).
