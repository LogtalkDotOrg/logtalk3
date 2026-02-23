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


% Quadratic minimization with progress reporting.
% Defines progress/5 to record progress updates using assertz.

:- object(quadratic_progress,
	implements(simulated_annealing_protocol)).

	:- public(progress_log/5).
	:- dynamic(progress_log/5).

	:- public(clear_log/0).

	:- public(reset_seed/0).

	:- uses(fast_random(xoshiro128pp), [
		random/3, reset_seed/0
	]).

	initial_state(50.0).

	neighbor_state(X, Y) :-
		random(-2.0, 2.0, Delta),
		Y is X + Delta.

	state_energy(X, E) :-
		E is (X - 3.0) * (X - 3.0).

	initial_temperature(100.0).

	progress(Step, Temp, BestEnergy, AccRate, ImpRate) :-
		assertz(progress_log(Step, Temp, BestEnergy, AccRate, ImpRate)).

	clear_log :-
		retractall(progress_log(_, _, _, _, _)).

:- end_object.
