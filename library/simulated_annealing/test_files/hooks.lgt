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


:- object(simulated_annealing_failing_hooks,
	extends(quadratic)).

	% Reuse the quadratic problem but define failing hooks so that we can
	% query the defined_in/1 predicate property to detect the definitions,
	% therefore exercising fallbacks rather than absent-hook paths.

	neighbor_state(_, _, _) :-
		fail.

	cooling_schedule(_, _, _) :-
		fail.

	stop_condition(_, _, _) :-
		fail.

:- end_object.


:- object(simulated_annealing_inherited_hooks,
	extends(quadratic_delta)).

:- end_object.


:- object(simulated_annealing_dynamic_hooks,
	implements(simulated_annealing_protocol)).

	:- public([
		neighbor_state/3, cooling_schedule/3, stop_condition/3
	]).
	:- dynamic([
		neighbor_state/3, cooling_schedule/3, stop_condition/3
	]).

	initial_state(2) :-
		retractall(neighbor_state(_, _, _)),
		retractall(cooling_schedule(_, _, _)),
		retractall(stop_condition(_, _, _)).

	state_energy(State, State).

	initial_temperature(10.0).

	neighbor_state(State, Neighbor) :-
		Neighbor is State - 1,
		assertz(neighbor_state(_, 0, -1)),
		assertz(cooling_schedule(_, _, 5.0)),
		assertz(stop_condition(2, _, _)).

:- end_object.
