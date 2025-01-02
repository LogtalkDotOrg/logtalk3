%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(farmer,
	instantiates(state_space)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2024-11-06,
		comment is 'Farmer, cabbage, goat, and wolf state space search problem.'
	]).

	initial_state(start, s(north, north, north, north)).

	goal_state(end, s(south, south, south, south)).

	next_state(s(Cabbage, Goat, Wolf, Farmer), s(FCabbage, Goat, Wolf, FFarmer)) :-
		same_side(Farmer, Cabbage),
		\+ same_side(Goat, Wolf),
		flip(Farmer, FFarmer),
		flip(Cabbage, FCabbage).
	next_state(s(Cabbage, Goat, Wolf, Farmer), s(Cabbage, FGloat, Wolf, FFarmer)) :-
		same_side(Farmer, Goat),
		flip(Farmer, FFarmer),
		flip(Goat, FGloat).
	next_state(s(Cabbage, Goat, Wolf, Farmer), s(Cabbage, Goat, FWolf, FFarmer)) :-
		same_side(Farmer, Wolf),
		\+ same_side(Cabbage, Goat),
		flip(Farmer, FFarmer),
		flip(Wolf, FWolf).
	next_state(s(Cabbage, Goat, Wolf, Farmer), s(Cabbage, Goat, Wolf, FFarmer)) :-
		\+ same_side(Cabbage, Goat),
		\+ same_side(Goat, Wolf),
		flip(Farmer, FFarmer).

	flip(north, south).
	flip(south, north).

	same_side(north, north).
	same_side(south, south).

	print_state(s(Cabbage, Goat, Wolf, Farmer)) :-
		(Cabbage == north -> write(c); write('_')),
		(Goat == north -> write(g); write('_')),
		(Wolf == north -> write(w); write('_')),
		(Farmer == north -> write('f.<__>.........._'); write('_..........<__>.f')),
		(Cabbage == north -> write('_'); write(c)),
		(Goat == north -> write('_'); write(g)),
		(Wolf == north -> write('_'); write(w)),
		nl.

:- end_object.
