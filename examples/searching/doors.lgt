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


:- object(doors,
	instantiates(state_space)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-12-14,
		comment is 'Locked doors state space search problem.'
	]).

	:- uses(format, [
		format/2
	]).

	initial_state(start, s(Room, false, false)) :-
		initial(Room).

	goal_state(end, s(Room, _, _)) :-
		treasure(Room).

	% moving without picking up a key or using a key
	next_state(s(A, Red, Blue), s(B, Red, Blue)) :-
		door(A, B).
	next_state(s(A, Red, Blue), s(B, Red, Blue)) :-
		door(B, A).
	% picking up a red key
	next_state(s(A, false, Blue), s(B, true, Blue)) :-
		door(A, B),
		key(B, red).
	% picking up a blue key
	next_state(s(A, Red, false), s(B, Red, true)) :-
		door(A, B),
		key(B, blue).
	% open locked doors
	next_state(s(A, Red, true), s(B, Red, true)) :-
	    locked_door(A, B, blue).
	next_state(s(A, Red, true), s(B, Red, true)) :-
	    locked_door(B, A, blue).
	next_state(s(A, true, Blue), s(B, true, Blue)) :-
	    locked_door(A, B, red).
	next_state(s(A, true, Blue), s(B, true, Blue)) :-
	    locked_door(B, A, red).

	print_state(s(Room, HasRedKey, HasBlueKey)) :-
		(	HasRedKey == false, HasBlueKey == false ->
			format('At room ~w holding with no keys~n', [Room])
		;	HasRedKey == true, HasBlueKey == false ->
			format('At room ~w holding the red key~n', [Room])
		;	HasRedKey == false, HasBlueKey == true ->
			format('At room ~w holding the blue key~n', [Room])
		;	% HasRedKey == true, HasBlueKey == true,
			format('At room ~w holding the red and blue keys~n', [Room])
		).

	initial(0).

	door(0, 1).
	door(0, 2).
	door(1, 2).
	door(1, 3).
	door(2, 4).

	locked_door(2, 5, red).
	locked_door(5, 6, blue).

	key(3, red).
	key(4, blue).

	treasure(6).

:- end_object.
