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

:- object(tabu_search_fixture(_Mode_, _States_, _Stop_),
	implements(tabu_search_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-10,
		comment is 'Deterministic tabu search regression problems.'
	]).

	:- public([clear_log/0, reports/1, calls/2, numbers/2, sampling_benchmark/4, candidate_benchmark/4]).
	:- mode(clear_log, one).
	:- mode(reports(-list), one).
	:- mode(calls(+atom, -non_negative_integer), one).
	:- mode(numbers(+non_negative_integer, -list(integer)), one).
	:- mode(sampling_benchmark(+positive_integer, +positive_integer, +positive_integer, -number), one).
	:- mode(candidate_benchmark(+positive_integer, +positive_integer, +positive_integer, -number), one).
	:- dynamic(report_/5).
	:- dynamic(event_/1).

	initial_state(State) :-
		parameter(2, [State-_| _]).

	state_energy(State, Energy) :-
		record_event(energy),
		parameter(2, States),
		energy(State, States, Energy).

	neighbor_state(State, Neighbor) :-
		parameter(1, Mode),
		Mode \== no_neighbor,
		record_event(neighbor),
		parameter(2, States),
		States = [First-_| _],
		next(State, States, First, Neighbor).

	neighbor_state(State, Neighbor, Delta) :-
		parameter(1, Mode),
		Mode == delta,
		neighbor_state(State, Neighbor),
		parameter(2, States),
		energy(State, States, Current),
		energy(Neighbor, States, Next),
		Delta is Next - Current.

	neighbors(State, Neighbors) :-
		parameter(1, Mode),
		( Mode == empty ->
			Neighbors = []
		; ( Mode == enumerated ->
				parameter(2, States),
				findall(Candidate, (
					list::member(Candidate-_, States),
					Candidate \== State
				), Neighbors)
			; Mode == full,
				neighbor_state(State, Neighbor),
				Neighbors = [Neighbor]
			)
		).

	stop_condition(Step, _, _) :-
		parameter(3, Stop),
		integer(Stop),
		Step >= Stop.

	progress(Step, Best, Current, Acceptance, Improvement) :-
		parameter(1, Mode),
		Mode \== no_progress,
		assertz(report_(Step, Best, Current, Acceptance, Improvement)).

	clear_log :-
		retractall(report_(_, _, _, _, _)),
		retractall(event_(_)).

	reports(Reports) :-
		findall(report(Step, Best, Current, Acceptance, Improvement), report_(Step, Best, Current, Acceptance, Improvement), Reports).

	calls(Kind, Count) :-
		findall(1, event_(Kind), Events),
		list::length(Events, Count).

	numbers(0, []) :-
		!.
	numbers(Count, [Count| Rest]) :-
		Next is Count - 1,
		numbers(Next, Rest).

	sampling_benchmark(Size, Count, Repeats, Seconds) :-
		numbers(Size, List),
		fast_random(xoshiro128pp)::randomize(42),
		tabu_search(quadratic, xoshiro128pp)<<sample_list(List, Size, Count, _),
		os::cpu_time(Start),
		repeat_sample(Repeats, List, Size, Count),
		os::cpu_time(End),
		Seconds is End - Start.

	candidate_benchmark(Tenure, Count, Repeats, Seconds) :-
		numbers(Tenure, Numbers),
		findall(Number-1000000, list::member(Number, Numbers), Tabu),
		copies(Count, a, Candidates),
		os::cpu_time(Start),
		repeat_evaluation(Repeats, Candidates, Tabu),
		os::cpu_time(End),
		Seconds is End - Start.

	record_event(Kind) :-
		parameter(1, Mode),
		( (Mode == counted; Mode == delta) ->
			assertz(event_(Kind))
		; true
		).

	energy(State, [Candidate-Energy| _], Energy) :-
		State == Candidate,
		!.
	energy(State, [_| Rest], Energy) :-
		energy(State, Rest, Energy).

	next(State, [Candidate-_| Rest], First, Neighbor) :-
		State == Candidate,
		!,
		( Rest = [Neighbor-_| _] ->
			true
		; Neighbor = First
		).
	next(State, [_| Rest], First, Neighbor) :-
		next(State, Rest, First, Neighbor).

	repeat_sample(0, _, _, _) :-
		!.
	repeat_sample(Repeats, List, Size, Count) :-
		tabu_search(quadratic, xoshiro128pp)<<sample_list(List, Size, Count, _),
		Next is Repeats - 1,
		repeat_sample(Next, List, Size, Count).

	copies(0, _, []) :-
		!.
	copies(Count, Element, [Element| Rest]) :-
		Next is Count - 1,
		copies(Next, Element, Rest).

	repeat_evaluation(0, _, _) :-
		!.
	repeat_evaluation(Repeats, Candidates, Tabu) :-
		tabu_search(tabu_search_fixture(full, [a-1], none), xoshiro128pp)<<evaluate_candidates(policy(false, false, false, false, false), Candidates, 0, Tabu, 1, _, _, _),
		Next is Repeats - 1,
		repeat_evaluation(Next, Candidates, Tabu).

:- end_object.


:- object(tabu_search_key_fixture(_KeyMode_, _Mode_, _States_, _Stop_),
	extends(tabu_search_fixture(_Mode_, _States_, _Stop_))).

	tabu_key(State, Key) :-
		parameter(1, Mode),
		( Mode == fail ->
			fail
		; ( Mode == invalid ->
				true
			; ( Mode == throw ->
					throw(key_error)
				; ( Mode == identity ->
						Key = State
					; arg(1, State, Value),
						Key = key(Value)
					)
				)
			)
		).

:- end_object.


:- object(tabu_search_inherited_key_fixture,
	extends(tabu_search_key_fixture(canonical, full, [s(a,1)-0,s(b,1)-1,s(a,2)-0], none))).
:- end_object.


:- object(tabu_search_restart_fixture(_RestartMode_, _States_, _Stop_),
	extends(tabu_search_fixture(counted, _States_, _Stop_))).

	:- public(restart_inputs/1).
	:- mode(restart_inputs(-list), one).
	:- dynamic(restart_/1).

	restart_state(Best, State) :-
		assertz(restart_(Best)),
		parameter(1, Mode),
		( Mode == fail ->
			fail
		; ( Mode == invalid ->
				true
			; ( Mode == throw ->
					throw(restart_error)
				; ( Mode == identity ->
						State = Best
					; ( Mode == random ->
							fast_random(as183)::between(1, 2, Index),
							parameter(2, States),
							list::nth1(Index, States, State-_)
						; Mode = value(State)
						)
					)
				)
			)
		).

	clear_log :-
		^^clear_log,
		retractall(restart_(_)).

	restart_inputs(Inputs) :-
		findall(State, restart_(State), Inputs).

:- end_object.


:- object(tabu_search_inherited_restart_fixture,
	extends(tabu_search_restart_fixture(value(b), [a-2,b-1], 0))).
:- end_object.


:- object(tabu_search_tenure_fixture(_TenureMode_, _States_, _Stop_),
	extends(tabu_search_fixture(full, _States_, _Stop_))).

	:- public(tenure_inputs/1).
	:- mode(tenure_inputs(-list), one).
	:- dynamic(tenure_/3).

	tabu_tenure(Step, Best, Current, Tenure) :-
		assertz(tenure_(Step, Best, Current)),
		parameter(1, Mode),
		( Mode == fail ->
			fail
		; ( Mode == throw ->
				throw(tenure_error)
			; ( Mode = schedule(Tenures) ->
					Index is Step + 1,
					list::nth1(Index, Tenures, Tenure)
				; Mode = value(Tenure)
				)
			)
		).

	clear_log :-
		^^clear_log,
		retractall(tenure_(_, _, _)).

	tenure_inputs(Inputs) :-
		findall(tenure(Step, Best, Current), tenure_(Step, Best, Current), Inputs).

:- end_object.


:- object(tabu_search_inherited_tenure_fixture,
	extends(tabu_search_tenure_fixture(value(1), [a-0,b-1], none))).
:- end_object.


:- object(tabu_search_aspiration_fixture(_AspirationMode_, _Mode_, _States_, _Stop_),
	extends(tabu_search_fixture(_Mode_, _States_, _Stop_))).

	:- public(aspiration_inputs/1).
	:- mode(aspiration_inputs(-list), one).
	:- dynamic(aspiration_/3).

	aspiration(State, Energy, Best) :-
		assertz(aspiration_(State, Energy, Best)),
		parameter(1, Mode),
		( Mode == throw ->
			throw(aspiration_error)
		; Mode == permit
		).

	clear_log :-
		^^clear_log,
		retractall(aspiration_(_, _, _)).

	aspiration_inputs(Inputs) :-
		findall(aspiration(State, Energy, Best), aspiration_(State, Energy, Best), Inputs).

:- end_object.


:- object(tabu_search_inherited_aspiration_fixture,
	extends(tabu_search_aspiration_fixture(permit, full, [a-0,b-1], none))).
:- end_object.


:- object(tabu_search_keyed_aspiration_fixture,
	extends(tabu_search_aspiration_fixture(permit, full, [s(a,1)-0,s(b,1)-1,s(a,2)-0], none))).

	tabu_key(State, key(Value)) :-
		arg(1, State, Value).

:- end_object.


:- object(tabu_search_missing_neighborhood_fixture,
	implements(tabu_search_problem_protocol)).

	initial_state(_) :-
		throw(initial_state_called).

:- end_object.


:- object(tabu_search_failing_neighborhood_fixture,
	extends(tabu_search_fixture(full, [a-0,b-1], none))).

	neighbors(a, [b]).

:- end_object.


:- object(tabu_search_throwing_neighborhood_fixture,
	extends(tabu_search_fixture(full, [a-0,b-1], none))).

	neighbors(_, _) :-
		throw(neighborhood_error).

:- end_object.


:- object(tabu_search_enumerated_delta_fixture,
	extends(tabu_search_fixture(delta, [a-3,b-2,c-1], none))).

	neighbors(_, [b,c]).

:- end_object.


:- object(tabu_search_combined_fixture(_Mode_),
	extends(tabu_search_aspiration_fixture(permit, _Mode_, [s(a,1)-2,s(b,1)-1,s(a,2)-0], none))).

	tabu_key(State, key(Value)) :-
		arg(1, State, Value).

	tabu_tenure(Step, _, _, Tenure) :-
		( Step mod 2 =:= 0 ->
			Tenure = 3
		; Tenure = 0
		).

	restart_state(_, s(b,1)).

:- end_object.


:- object(tabu_search_restart_energy_error_fixture,
	extends(tabu_search_restart_fixture(value(b), [a-0,b-1], 0))).

	state_energy(State, Energy) :-
		( State == b ->
			throw(restart_energy_error)
		; ^^state_energy(State, Energy)
		).

:- end_object.


:- category(tabu_search_key_fixture_category,
	implements(tabu_search_problem_protocol)).

	tabu_key(State, State).

:- end_category.


:- object(tabu_search_category_key_fixture,
	imports(tabu_search_key_fixture_category),
	extends(tabu_search_fixture(full, [a-0,b-1], none))).
:- end_object.
