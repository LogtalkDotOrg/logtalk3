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


:- category(directed_graph_common,
	implements(directed_graph_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Common directed graph predicates shared by directed graph objects. Uses self-dispatch to call object-specific predicates and avltree for internal bookkeeping.'
	]).

	:- uses(avltree, [
		new/1 as scc_new/1,
		insert/4 as scc_insert/4,
		lookup/3 as scc_lookup/3
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	% === Degree queries ===

	in_degree(Vertex, Graph, Degree) :-
		::neighbors(Vertex, Graph, _),
		::vertices(Graph, Vertices),
		count_incoming(Vertices, Vertex, Graph, 0, Degree).

	out_degree(Vertex, Graph, Degree) :-
		::neighbors(Vertex, Graph, Ns),
		length(Ns, Degree).

	% === DAG test ===

	is_dag(Graph) :-
		::topological_sort(Graph, Sorted),
		::vertices(Graph, Vertices),
		length(Sorted, N1),
		length(Vertices, N2),
		N1 =:= N2.

	% === Strongly Connected Components (Tarjan's algorithm) ===

	strongly_connected_components(Graph, SCCs) :-
		::vertices(Graph, Vertices),
		scc_new(Info0),
		tarjan_all(Vertices, Graph, Info0, 0, [], [], _Info, _Index, _Stack, SCCs).

	% ===========================================================
	% Auxiliary predicates
	% ===========================================================

	% --- In-degree helpers ---

	count_incoming([], _, _, D, D).
	count_incoming([V|Vs], Target, Graph, Acc, D) :-
		(	::neighbors(V, Graph, Ns),
			memberchk(Target, Ns) ->
			Acc1 is Acc + 1
		;	Acc1 = Acc
		),
		count_incoming(Vs, Target, Graph, Acc1, D).

	% --- Tarjan's SCC ---

	tarjan_all([], _, Info, Index, Stack, SCCs, Info, Index, Stack, SCCs).
	tarjan_all([V|Vs], Graph, Info0, Index0, Stack0, SCCs0, Info, Index, Stack, SCCs) :-
		(	scc_lookup(V, _, Info0) ->
			tarjan_all(Vs, Graph, Info0, Index0, Stack0, SCCs0, Info, Index, Stack, SCCs)
		;	strongconnect(V, Graph, Info0, Index0, Stack0, SCCs0, Info1, Index1, Stack1, SCCs1),
			tarjan_all(Vs, Graph, Info1, Index1, Stack1, SCCs1, Info, Index, Stack, SCCs)
		).

	strongconnect(V, Graph, Info0, Index0, Stack0, SCCs0, Info, Index, Stack, SCCs) :-
		scc_insert(Info0, V, scc(Index0, Index0, true), Info1),
		Index1 is Index0 + 1,
		Stack1 = [V|Stack0],
		::neighbors(V, Graph, Neighbors),
		process_scc_neighbors(Neighbors, V, Graph, Info1, Index1, Stack1, SCCs0, Info2, Index2, Stack2, SCCs1),
		scc_lookup(V, scc(VIndex, VLowlink, _), Info2),
		(	VIndex =:= VLowlink ->
			pop_scc(Stack2, V, [], SCC0, Stack3),
			sort(SCC0, SCC),
			mark_off_stack(SCC, Info2, Info3),
			Info = Info3, Index = Index2, Stack = Stack3, SCCs = [SCC|SCCs1]
		;	Info = Info2, Index = Index2, Stack = Stack2, SCCs = SCCs1
		).

	process_scc_neighbors([], _, _, Info, Index, Stack, SCCs, Info, Index, Stack, SCCs).
	process_scc_neighbors([W|Ws], V, Graph, Info0, Index0, Stack0, SCCs0, Info, Index, Stack, SCCs) :-
		(	scc_lookup(W, scc(WIndex, _, WOnStack), Info0) ->
			(	WOnStack == true ->
				scc_lookup(V, scc(VIndex, VLowlink, VOnStack), Info0),
				NewVLowlink is min(VLowlink, WIndex),
				scc_insert(Info0, V, scc(VIndex, NewVLowlink, VOnStack), Info1),
				process_scc_neighbors(Ws, V, Graph, Info1, Index0, Stack0, SCCs0, Info, Index, Stack, SCCs)
			;	process_scc_neighbors(Ws, V, Graph, Info0, Index0, Stack0, SCCs0, Info, Index, Stack, SCCs)
			)
		;	strongconnect(W, Graph, Info0, Index0, Stack0, SCCs0, Info1, Index1, Stack1, SCCs1),
			scc_lookup(V, scc(VIndex, VLowlink, VOnStack), Info1),
			scc_lookup(W, scc(_, WLowlink, _), Info1),
			NewVLowlink is min(VLowlink, WLowlink),
			scc_insert(Info1, V, scc(VIndex, NewVLowlink, VOnStack), Info2),
			process_scc_neighbors(Ws, V, Graph, Info2, Index1, Stack1, SCCs1, Info, Index, Stack, SCCs)
		).

	pop_scc([W|Stack], Target, Acc, SCC, RestStack) :-
		(	W == Target ->
			SCC = [W|Acc],
			RestStack = Stack
		;	pop_scc(Stack, Target, [W|Acc], SCC, RestStack)
		).

	mark_off_stack([], Info, Info).
	mark_off_stack([V|Vs], Info0, Info) :-
		scc_lookup(V, scc(VIndex, VLowlink, _), Info0),
		scc_insert(Info0, V, scc(VIndex, VLowlink, false), Info1),
		mark_off_stack(Vs, Info1, Info).

:- end_category.
