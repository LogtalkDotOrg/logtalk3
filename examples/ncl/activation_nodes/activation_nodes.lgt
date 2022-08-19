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


:- object(activation(_X_, _Y_, _Z_),
	implements(monitoring)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2022-08-19,
		comment is 'Logtalk version of a NCL free nodes example found in its manual.'
	]).

	% set the object as the monitor for messages sent to itself
	:- initialization(define_events(after, activation(_,_,_), _, _, activation(_,_,_))).

	% free nodes

	:- public([a/1, b/1, c/1]).

	a(_X_).
	b(_Y_).
	c(_Z_).

	% activation node transformed into a main clause for the node goal
	% and an auxiliary clause to compute the threshold for activation

	node(T) :-
		threshold(T),
		(	T =< 0 ->
			write('  '), write(_X_-_Y_-_Z_), nl
		;	true
		).

	threshold(T) :-
		(nonvar(_X_) -> X is -1; X is 0),
		(nonvar(_Y_) -> Y is +1; Y is 0),
		(nonvar(_Z_) -> Z is -1; Z is 0),
		T is X + Y + Z + 1,
		write('T = '), write(T), nl.

	% event handler implementing the spreading activation
	after(activation(_X_, _Y_, _Z_), _, user) :-
		node(_).

:- end_object.
