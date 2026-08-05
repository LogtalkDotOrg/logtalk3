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


:- object(supplied_velocity(_Case_),
	implements(particle_swarm_optimization_protocol)).

	initial_positions([[0.2]]).

	position_bounds([0.0-1.0]).

	initial_velocities(Velocities) :-
		initial_velocities(_Case_, Velocities).

	initial_velocities(movement, [[0.3]]).
	initial_velocities(lower_limit, [[-1.0]]).
	initial_velocities(upper_limit, [[1.0]]).
	initial_velocities(empty, []).
	initial_velocities(extra, [[0.0], [0.0]]).
	initial_velocities(dimension, [[0.0, 0.0]]).
	initial_velocities(nonnumeric, [[a]]).
	initial_velocities(below_limit, [[-1.1]]).
	initial_velocities(above_limit, [[1.1]]).
	initial_velocities(nonground, [[_]]).

	fitness([X], X).

:- end_object.


:- object(random_velocity_fallback,
	implements(particle_swarm_optimization_protocol)).

	initial_positions([[0.2]]).

	position_bounds([0.0-1.0]).

	fitness([X], X).

:- end_object.


:- object(failing_velocity_fallback,
	extends(random_velocity_fallback)).

	initial_velocities(_Velocities) :-
		fail.

:- end_object.
