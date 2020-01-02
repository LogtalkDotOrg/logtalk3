%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- object(ping_pong).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2019/11/15,
		comment is 'Simple example of two threads playing ping-pong.'
	]).

	:- threaded.

	:- public(play/1).
	:- mode(play(+integer), one).
	:- info(play/1, [
		comment is 'Play a game of ping-pong with the given number of moves (>= 1).',
		argnames is ['Moves']
	]).

	play(Moves) :-
		threaded((
			ping(Moves),
			pong
		)).

	ping(Moves) :-
		(	Moves =:= 0 ->
			write('Game over!\n')
		;	write('Ping ...\n'),
			threaded_notify(throw(Moves)),
			threaded_wait(catch),
			ping(Moves - 1)
		).

	pong :-
		threaded_wait(throw(Moves)),
		write('.... Pong\n'),
		threaded_notify(catch),
		(	Moves =:= 1 ->
			true
		;	pong
		).

:- end_object.
