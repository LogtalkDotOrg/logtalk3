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


:- object(tests_time_expanded_network(_DictionaryObject_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Unit tests for time-expanded flow network construction.',
		parnames is ['DictionaryObject']
	]).

	:- uses(integer, [
		between/3
	]).

	:- uses(list, [
		append/3
	]).

	:- uses(flow_network(_DictionaryObject_), [
		new/3 as network_new/3, vertices/2, arcs/2,
		valid_min_cost_flow/4 as network_valid_min_cost_flow/4,
		valid_st_flow/5 as network_valid_st_flow/5
	]).

	:- uses(dinic(_DictionaryObject_), [
		max_flow/5
	]).

	:- uses(successive_shortest_paths(_DictionaryObject_), [
		min_cost_flow/4
	]).

	cover(time_expanded_network(_DictionaryObject_)).
	cover(time_expanded_network).
	cover(flow_network(_DictionaryObject_)).
	cover(dinic(_DictionaryObject_)).
	cover(successive_shortest_paths(_DictionaryObject_)).

	test(time_expanded_horizon_zero_empty, deterministic(Vertices-Mapping == [time(a,0),time(b,0)]-mapping([],[]))) :-
		time_expanded_network(_DictionaryObject_)::build([b,a], 0, [], [], Network, Mapping),
		vertices(Network, Vertices).

	test(time_expanded_isolated_copies, deterministic(Vertices == [time(a,0),time(a,1),time(a,2),time(z,0),time(z,1),time(z,2)])) :-
		time_expanded_network(_DictionaryObject_)::build([z,a,z,a], 2, [], [], Network, _),
		vertices(Network, Vertices).

	test(time_expanded_mapping_and_parallel_schedules, deterministic(Arcs-Mapping == [arc(time(a,0),time(b,2),0,2,4),arc(time(a,0),time(b,2),1,3,5),arc(time(a,1),time(b,2),0,1,6),arc(time(b,1),time(a,2),0,1,7)]-mapping([travel(1,1,a,b,0,2),travel(2,2,a,b,0,2),travel(3,3,a,b,1,2),travel(4,4,b,a,1,2)],[]))) :-
		time_expanded_network(_DictionaryObject_)::build(
			[a,b], 2,
			[travel(a,b,0,2,0,2,4), travel(a,b,0,2,1,3,5), travel(a,b,1,1,0,1,6), travel(b,a,1,1,0,1,7)],
			[], Network, Mapping),
		arcs(Network, Arcs).

	test(time_expanded_storage_one_period, deterministic(Arcs-Mapping == [arc(time(a,0),time(a,1),1,2,-3)]-mapping([],[store(1,1,a,0,1)]))) :-
		time_expanded_network(_DictionaryObject_)::build([a], 1, [], [store(a,0,1,2,-3)], Network, Mapping),
		arcs(Network, Arcs).

	test(time_expanded_time_dependent_storage, deterministic(Arcs-Mapping == [arc(time(a,0),time(a,1),0,1,2),arc(time(a,1),time(a,2),0,3,-1)]-mapping([],[store(1,1,a,0,1),store(2,2,a,1,2)]))) :-
		time_expanded_network(_DictionaryObject_)::build([a], 2, [], [store(a,0,0,1,2),store(a,1,0,3,-1)], Network, Mapping),
		arcs(Network, Arcs).

	test(time_expanded_lower_bounds_min_cost, deterministic(Cost == 3)) :-
		time_expanded_network(_DictionaryObject_)::build(
			[a,b], 2,
			[travel(a,b,0,1,1,2,3)],
			[store(b,1,1,2,0)], Network,
			mapping([travel(1,1,a,b,0,1)], [store(2,1,b,1,2)])),
		min_cost_flow(Network, [time(a,0)-1, time(b,2)-(-1)], Cost, Flow),
		network_valid_min_cost_flow(Network, [time(a,0)-1, time(b,2)-(-1)], Flow, Cost).

	test(time_expanded_inventory_optimum, deterministic(Cost == Expected)) :-
		time_expanded_network(_DictionaryObject_)::build(
			[depot, market], 2,
			[travel(depot,market,0,1,0,2,5), travel(depot,market,1,1,0,2,1)],
			[store(depot,0,0,2,0), store(market,1,0,2,0)], Network, _),
		min_cost_flow(Network, [time(depot,0)-2, time(market,2)-(-2)], Cost, Flow),
		network_valid_min_cost_flow(Network, [time(depot,0)-2, time(market,2)-(-2)], Flow, Cost),
		inventory_oracle(Expected).

	test(time_expanded_deadline_flow, deterministic(Value == Expected)) :-
		time_expanded_network(_DictionaryObject_)::build(
			[origin, near, far], 3,
			[travel(origin,near,0,1,0,2,0), travel(origin,far,0,2,0,1,0), travel(origin,far,0,3,0,5,0)],
			[], ExpandedNetwork, _),
		arcs(ExpandedNetwork, ExpandedArcs),
		append(ExpandedArcs,
			[arc(time(near,1),deadline_sink,0,2,0), arc(time(far,2),deadline_sink,0,2,0)],
			Arcs),
		network_new([deadline_sink], Arcs, Network),
		max_flow(Network, time(origin,0), deadline_sink, Value, Flow),
		network_valid_st_flow(Network, time(origin,0), deadline_sink, Value, Flow),
		deadline_oracle(Expected).

	test(time_expanded_invalid_horizon_type, error(type_error(integer, 1.5))) :-
		time_expanded_network(_DictionaryObject_)::build([], 1.5, [], [], _, _).

	test(time_expanded_negative_horizon, error(domain_error(non_negative_integer, -1))) :-
		time_expanded_network(_DictionaryObject_)::build([], -1, [], [], _, _).

	test(time_expanded_nonground_vertex, error(instantiation_error)) :-
		time_expanded_network(_DictionaryObject_)::build([vertex(_)], 0, [], [], _, _).

	test(time_expanded_partial_travel_list, error(instantiation_error)) :-
		time_expanded_network(_DictionaryObject_)::build([a,b], 1, [travel(a,b,0,1,0,1,0)|_], [], _, _).

	test(time_expanded_bad_travel_shape, error(domain_error(time_expanded_schedule, edge(a,b)))) :-
		time_expanded_network(_DictionaryObject_)::build([a,b], 1, [edge(a,b)], [], _, _).

	test(time_expanded_undeclared_endpoint, error(existence_error(vertex, c))) :-
		time_expanded_network(_DictionaryObject_)::build([a,b], 1, [travel(a,c,0,1,0,1,0)], [], _, _).

	test(time_expanded_nonground_endpoint, error(instantiation_error)) :-
		time_expanded_network(_DictionaryObject_)::build([a,b], 1, [travel(a,f(_),0,1,0,1,0)], [], _, _).

	test(time_expanded_out_of_range_departure, error(consistency_error(time_expanded_schedule_and_horizon, travel(a,b,2,1,0,1,0), 1))) :-
		time_expanded_network(_DictionaryObject_)::build([a,b], 1, [travel(a,b,2,1,0,1,0)], [], _, _).

	test(time_expanded_arrival_after_horizon, error(consistency_error(time_expanded_schedule_and_horizon, travel(a,b,0,2,0,1,0), 1))) :-
		time_expanded_network(_DictionaryObject_)::build([a,b], 1, [travel(a,b,0,2,0,1,0)], [], _, _).

	test(time_expanded_duration_over_remaining_horizon, error(consistency_error(time_expanded_schedule_and_horizon, travel(a,b,0,1000000000,0,1,0), 1))) :-
		time_expanded_network(_DictionaryObject_)::build([a,b], 1, [travel(a,b,0,1000000000,0,1,0)], [], _, _).

	test(time_expanded_zero_duration, error(domain_error(positive_integer, 0))) :-
		time_expanded_network(_DictionaryObject_)::build([a,b], 1, [travel(a,b,0,0,0,1,0)], [], _, _).

	test(time_expanded_noninteger_duration, error(type_error(integer, 1.5))) :-
		time_expanded_network(_DictionaryObject_)::build([a,b], 1, [travel(a,b,0,1.5,0,1,0)], [], _, _).

	test(time_expanded_negative_capacity, error(domain_error(non_negative_integer, -1))) :-
		time_expanded_network(_DictionaryObject_)::build([a,b], 1, [travel(a,b,0,1,0,-1,0)], [], _, _).

	test(time_expanded_lower_exceeds_capacity, error(consistency_error(arc_lower_bound_and_capacity, 2, 1))) :-
		time_expanded_network(_DictionaryObject_)::build([a,b], 1, [travel(a,b,0,1,2,1,0)], [], _, _).

	test(time_expanded_noninteger_cost, error(type_error(integer, 1.5))) :-
		time_expanded_network(_DictionaryObject_)::build([a,b], 1, [travel(a,b,0,1,0,1,1.5)], [], _, _).

	test(time_expanded_storage_out_of_range, error(consistency_error(time_expanded_schedule_and_horizon, store(a,0,0,1,0), 0))) :-
		time_expanded_network(_DictionaryObject_)::build([a], 0, [], [store(a,0,0,1,0)], _, _).

	test(time_expanded_bad_storage_shape, error(domain_error(time_expanded_schedule, foo))) :-
		time_expanded_network(_DictionaryObject_)::build([a], 1, [], [foo], _, _).

	test(time_expanded_error_reports_travel, deterministic) :-
		Entry = travel(a,b,0,2,0,1,0),
		catch(time_expanded_network(_DictionaryObject_)::build([a,b], 1, [Entry], [], _, _),
			error(consistency_error(time_expanded_schedule_and_horizon, Reported, 1), _), true),
		Reported == Entry.

	test(time_expanded_error_reports_storage, deterministic) :-
		Entry = store(a,0,0,1,0),
		catch(time_expanded_network(_DictionaryObject_)::build([a], 0, [], [Entry], _, _),
			error(consistency_error(time_expanded_schedule_and_horizon, Reported, 0), _), true),
		Reported == Entry.

	test(time_expanded_default_wrapper, deterministic(Mapping == mapping([travel(1,1,a,b,0,1)],[]))) :-
		time_expanded_network::build([a,b], 1, [travel(a,b,0,1,0,1,0)], [], _, Mapping).

	inventory_oracle(2) :-
		forall(
			between(0, 2, EarlyDepartures),
			(	LateDepartures is 2 - EarlyDepartures,
				CandidateCost is 5 * EarlyDepartures + LateDepartures,
				CandidateCost >= 2
			)
		).

	deadline_oracle(3) :-
		% Enumerate the two independently bounded arrivals available by the deadline.
		between(0, 2, Near),
		between(0, 1, Far),
		Near + Far =:= 3,
		!.

:- end_object.
