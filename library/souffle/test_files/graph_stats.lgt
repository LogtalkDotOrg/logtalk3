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


:- object(souffle_graph_stats,
	implements(souffle_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-10,
		comment is 'Fixture exercising typed columns, negation, comparisons, string concatenation, and the count/sum/max/min/mean aggregates.'
	]).

	:- souffle_decl(edge, [x-number, y-number]).
	edge(1, 2).
	edge(2, 3).
	edge(3, 4).

	:- souffle_decl(node, [n-number]).
	:- souffle_output(node).
	node(X) :- edge(X, _).
	node(X) :- edge(_, X).

	:- souffle_decl(isolated, [n-number]).
	:- souffle_output(isolated).
	isolated(X) :- node(X), \+ edge(X, _), \+ edge(_, X).

	:- souffle_decl(edge_count, [cnt-number]).
	:- souffle_output(edge_count).
	edge_count(X) :- X = aggregate(count, none, [edge(_, _)]).

	:- souffle_decl(max_x, [m-number]).
	:- souffle_output(max_x).
	max_x(X) :- X = aggregate(max, Y, [edge(Y, _)]).

	:- souffle_decl(min_x, [m-number]).
	:- souffle_output(min_x).
	min_x(X) :- X = aggregate(min, Y, [edge(Y, _)]).

	:- souffle_decl(sum_x, [s-number]).
	:- souffle_output(sum_x).
	sum_x(X) :- X = aggregate(sum, Y, [edge(Y, _)]).

	:- souffle_decl(weight, [w-float]).
	weight(1.0).
	weight(2.0).
	weight(3.0).

	:- souffle_decl(mean_weight, [m-float]).
	:- souffle_output(mean_weight).
	mean_weight(X) :- X = aggregate(mean, Y, [weight(Y)]).

	:- souffle_decl(greeting, [s-symbol]).
	:- souffle_output(greeting).
	greeting(X) :- X = cat(string([104,101,108,108,111,32]), string([119,111,114,108,100])).

	:- souffle_decl(big_x, [n-number]).
	:- souffle_output(big_x).
	big_x(X) :- node(X), X > 2.

:- end_object.
