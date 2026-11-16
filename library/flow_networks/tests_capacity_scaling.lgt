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


:- object(tests_capacity_scaling(_Dictionary_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Unit tests for capacity-scaling maximum flow.',
		parnames is ['Dictionary']
	]).

	:- uses(flow_network(_Dictionary_), [
		new/2, new/3, valid_st_flow/5, valid_cut/5
	]).

	:- uses(capacity_scaling(_Dictionary_), [
		max_flow/5, min_cut/5, multi_source_max_flow/5, node_capacity_max_flow/6, sink_closest_min_cut/5,
		unique_min_cut/4, min_cuts/5
	]).

	cover(capacity_scaling(_Dictionary_)).

	test(power_boundaries, deterministic(Value == 24)) :-
		new([arc(s,a,7),arc(a,t,7),arc(s,b,8),arc(b,t,8),arc(s,t,9),arc(s,s,32)], Network),
		max_flow(Network, s, t, Value, Flow),
		valid_st_flow(Network, s, t, Value, Flow),
		min_cut(Network, s, t, Value, Cut),
		valid_cut(Network, s, t, Value, Cut).

	test(zero_capacity, deterministic(Flow == [])) :-
		new([arc(s,t,0)], Network),
		max_flow(Network, s, t, 0, Flow).

	test(isolated_terminals, deterministic(Value == 0)) :-
		new([s,t], [], Network),
		max_flow(Network, s, t, Value, []).

	test(large_capacity, deterministic(Value == 100000001)) :-
		new([arc(s,a,100000000),arc(a,t,100000000),arc(s,t,1)], Network),
		max_flow(Network, s, t, Value, Flow),
		valid_st_flow(Network, s, t, Value, Flow).

	test(reverse_rerouting, deterministic(Value == 16)) :-
		new([arc(s,a,8),arc(a,b,8),arc(b,t,8),arc(s,c,7),arc(c,b,7),
			arc(a,d,7),arc(d,t,7),arc(s,t,1)], Network),
		max_flow(Network, s, t, Value, Flow),
		valid_st_flow(Network, s, t, Value, Flow).

	test(multi_terminal_wrapper, deterministic(Value == 5)) :-
		new([arc(a,x,2),arc(b,x,3),arc(x,t,5)], Network),
		multi_source_max_flow(Network, [a,b], [t], Value, _).

	test(node_capacity_wrapper, deterministic(Value == 2)) :-
		new([arc(s,x,5),arc(x,t,5)], Network),
		node_capacity_max_flow(Network, s, t, [x-2], Value, _).

	test(cut_variants, deterministic(Cuts == [cut([s],[t],[arc(s,t,3)])])) :-
		new([arc(s,t,3)], Network),
		sink_closest_min_cut(Network, s, t, 3, cut([s],[t],[arc(s,t,3)])),
		unique_min_cut(Network, s, t, true),
		findall(Cut, min_cuts(Network, s, t, 3, Cut), Cuts).

	test(lower_bound_rejection, error(domain_error(zero_lower_bounds, _))) :-
		new([arc(s,t,1,3,0)], Network),
		max_flow(Network, s, t, _, _).

:- end_object.
