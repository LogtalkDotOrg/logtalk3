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


% Traveling Salesman Problem (TSP) for a set of 6 cities arranged as
% vertices of a regular hexagon centered at (5,5) with radius 5.
%
% Cities:
%   a = (10.0, 5.0)
%   b = (7.5, 9.330)
%   c = (2.5, 9.330)
%   d = (0.0, 5.0)
%   e = (2.5, 0.670)
%   f = (7.5, 0.670)
%
% The optimal tour visits them in order around the hexagon.
% Each side has length 5.0, so the optimal tour length is 30.0.
%
% A state is a list of city names representing the tour order.
% The energy is the total Euclidean distance of the round trip.
%
% Uses a custom cooling schedule (slower geometric cooling).

:- object(tsp,
	implements(simulated_annealing_protocol)).

	:- uses(fast_random(xoshiro128pp), [
		between/3
	]).

	:- uses(list, [
		length/2
	]).

	% City coordinates
	city(a, 10.0,  5.000).
	city(b,  7.5,  9.330).
	city(c,  2.5,  9.330).
	city(d,  0.0,  5.000).
	city(e,  2.5,  0.670).
	city(f,  7.5,  0.670).

	initial_state([a, b, c, d, e, f]).

	% Generate a neighbor by swapping two random cities in the tour
	neighbor_state(Tour, Neighbor) :-
		length(Tour, N),
		between(1, N, I),
		N1 is N - 1,
		between(1, N1, J0),
		(	J0 >= I ->
			J is J0 + 1
		;	J = J0
		),
		swap(Tour, I, J, Neighbor).

	% Compute the total tour distance (round trip)
	state_energy(Tour, Energy) :-
		tour_distance(Tour, Energy).

	initial_temperature(1000.0).

	% Custom slower geometric cooling for better TSP exploration
	cooling_schedule(Temp, _Step, NewTemp) :-
		NewTemp is Temp * 0.999.

	% Swap elements at positions I and J in a list
	swap(List, I, J, Result) :-
		swap_elements(List, 1, I, J, _ElemI, _ElemJ, Result).

	swap_elements([], _, _, _, _, _, []).
	swap_elements([ElemI| Tail0], I, I, J, ElemI, ElemJ, [ElemJ| Tail]) :-
		!,
		Pos is I + 1,
		swap_elements(Tail0, Pos, I, J, ElemI, ElemJ, Tail).
	swap_elements([ElemJ| Tail0], J, I, J, ElemI, ElemJ, [ElemI| Tail]) :-
		!,
		Pos is J + 1,
		swap_elements(Tail0, Pos, I, J, ElemI, ElemJ, Tail).
	swap_elements([Head| Tail0], Pos, I, J, ElemI, ElemJ, [Head| Tail]) :-
		Pos1 is Pos + 1,
		swap_elements(Tail0, Pos1, I, J, ElemI, ElemJ, Tail).

	% Compute total tour distance including return to start
	tour_distance([City| Rest], Distance) :-
		tour_distance_loop(Rest, City, City, 0.0, Distance).

	tour_distance_loop([], LastCity, FirstCity, Distance0, Distance) :-
		city_distance(LastCity, FirstCity, D),
		Distance is Distance0 + D.
	tour_distance_loop([City| Rest], PrevCity, FirstCity, Distance0, Distance) :-
		city_distance(PrevCity, City, CityDistance),
		Distance1 is Distance0 + CityDistance,
		tour_distance_loop(Rest, City, FirstCity, Distance1, Distance).

	% Euclidean distance between two cities
	city_distance(City1, City2, Distance) :-
		city(City1, X1, Y1),
		city(City2, X2, Y2),
		DX is X1 - X2,
		DY is Y1 - Y2,
		Distance is sqrt(DX * DX + DY * DY).

:- end_object.
