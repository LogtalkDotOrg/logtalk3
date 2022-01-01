%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


% this example is an adaptation of the sample code found on the Wikipedia
% page on the Memento design pattern:
%
% https://en.wikipedia.org/wiki/Memento_pattern


:- object(memento).

	:- public(new/2).
	new(Memento, State) :-
		self(Self),
		create_object(Memento, [extends(Self)], [], [state_(State)]).

	:- public(saved_state/1).
	saved_state(State) :-
		::state_(State).

	:- private(state_/1).
	:- dynamic(state_/1).

:- end_object.


:- object(originator).

	:- public(set/1).
	set(State) :-
		write('Originator: Setting state to '), write(State), nl,
		retractall(state_(_)),
		assertz(state_(State)).

	:- public(save_to_memento/1).
	save_to_memento(Memento) :-
		write('Originator: Saving to Memento.'), nl,
		state_(State),
		memento::new(Memento, State).

	:- public(restore_from_memento/1).
	restore_from_memento(Memento) :-
		Memento::saved_state(State),
		retractall(state_(_)),
		assertz(state_(State)),
		write('Originator: State after restoring from Memento: '),
		write(State), nl.

	:- private(state_/1).
	:- dynamic(state_/1).

:- end_object.
