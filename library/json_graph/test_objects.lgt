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


:- object(test_graph_store,
	implements(json_graph_data_protocol)).

	:- dynamic([
		graph/2,
		node/3,
		edge/5,
		hyperedge/4,
		hyperedge/5
	]).

:- end_object.


:- object(source_graph,
	implements(json_graph_data_protocol)).

	graph(g1, [label('Example'), directed(true)]).
	node(g1, n1, [label('one')]).
	edge(g1, e1, n1, n1, [directed(true)]).

:- end_object.


:- object(source_hypergraphs,
	implements(json_graph_data_protocol)).

	graph(g2, [label('Directed hypergraph'), directed(true)]).
	graph(g3, [label('Undirected hypergraph'), directed(false)]).

	node(g2, a, []).
	node(g2, b, []).
	node(g2, c, []).
	node(g3, x, []).
	node(g3, y, []).

	hyperedge(g2, he1, [a], [b, c], [relation(links)]).
	hyperedge(g3, he1, [x, y], [metadata({weight-1})]).

:- end_object.


:- object(non_graph_source).

:- end_object.
