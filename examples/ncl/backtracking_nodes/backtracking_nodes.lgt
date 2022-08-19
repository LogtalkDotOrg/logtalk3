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


:- object(backtracking(_X_, _Y_, _Z_),
	implements(monitoring)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2022-08-19,
		comment is 'Logtalk version of a NCL spreading activation nodes backtracking example found in its manual.'
	]).

	% set the object as the monitor for messages sent to itself
	:- initialization(define_events(after, backtracking(_,_,_), _, _, backtracking(_,_,_))).

	% free nodes

	:- public(n/1).

	n(_X_).
	n(_Y_).
	n(_Z_).

	% spreading activation nodes transformed into a single main clause
	% (defining the condition X>Y>Z) and an auxiliary clause per node
	% to compute the node activation threshold

	node(T1, T2) :-
		threshold_1(T1),
		(T1 =< 0 -> _X_ > _Y_; true),
		threshold_2(T2),
		(T2 =< 0 -> _Y_ > _Z_; true).

	threshold_1(T) :-
		(nonvar(_X_) -> X is -1; X is 0),
		(nonvar(_Y_) -> Y is -1; Y is 0),
		T is X + Y + 2.

	threshold_2(T) :-
		(nonvar(_Y_) -> Y is -1; Y is 0),
		(nonvar(_Z_) -> Z is -1; Z is 0),
		T is Y + Z + 2.

	% event handler implementing the spreading activation
	after(backtracking(_X_, _Y_, _Z_), _, user) :-
		node(_, _).

:- end_object.
