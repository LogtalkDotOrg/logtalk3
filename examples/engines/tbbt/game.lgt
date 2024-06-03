%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- object(game).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2024-02-02,
		comment is 'Implementation of the rock, paper, scissors, lizard, Spock game played in the "The Big Bang Theory" sitcom.'
	]).

	:- threaded.

	% create an engine to do all output from the other engines
	:- initialization(threaded_engine_create(_, output, writer)).

	:- public(explain/0).
	:- mode(explain, one).
	:- info(explain/0, [
		comment is 'Explains the game of rock, paper, scissors, lizard, Spock to wannabe nerds.'
	]).

	:- uses(format, [
		format/2
	]).

	explain :-
		format('~w~n~w~n~w~n~w~n~w~n~w~n~w~n~w~n~w~n~w~n', [
			'Scissors cuts Paper',
			'Paper covers Rock',
			'Rock crushes Lizard',
			'Lizard poisons Spock',
			'Spock smashes Scissors',
			'Scissors decapitates Lizard',
			'Lizard eats Paper',
			'Paper disproves Spock',
			'Spock vaporizes Rock',
			'(and as it always has) Rock crushes Scissors'
		]).

	:- public(play/0).
	:- mode(play, one).
	:- info(play/0, [
		comment is 'Plays a game of rock, paper, scissors, lizard, Spock between Sheldon and Raj.'
	]).

	play :-
		% in the sitcom, the game is first played between
		% Sheldon and Raj: create an engine for each one
		threaded_engine_create(done, loop(sheldon), sheldon),
		threaded_engine_create(done, loop(raj), raj),
		play_move,
		% wait for both engines to terminate before destroying them
		threaded_engine_next(sheldon, done),
		threaded_engine_next(raj, done),
		threaded_engine_destroy(sheldon),
		threaded_engine_destroy(raj).

	output :-
		threaded_engine_fetch(Format-Arguments),
		format(Format, Arguments),
		% workaround tail-recursive predicates leaking memory
		fail.
	output :-
		output.

	% each engine runs this loop predicate until
	%  there is a winning or loosing move
	loop(Me) :-
		select_move(Me, Move),
		% return the selected move to the object,
		% which acts as the game arbiter
		threaded_engine_yield(Move),
		% react to the move outcome
		threaded_engine_fetch(Result),
		handle_result(Result, Me).

	handle_result(win, Me) :-
		threaded_engine_post(writer, '~w:I win! I''m the best!\n'-[Me]).
	handle_result(lose, Me) :-
		threaded_engine_post(writer, '~w:Penny distracted me! It''s Penny''s fault!\n'-[Me]).
	handle_result(draw, Me) :-
		loop(Me).

	% arbiter predicate that collects engine moves, compares them,
	% communicate the move outcome to the engines, and decides if
	% the game continues
	play_move :-
		threaded_engine_next(sheldon, SheldonMove),
		threaded_engine_next(raj, RajMove),
		decide_move(SheldonMove, RajMove, SheldonResult, RajResult),
		threaded_engine_post(sheldon, SheldonResult),
		threaded_engine_post(raj, RajResult),
		(	SheldonResult == draw ->
			play_move
		;	true
		).

	% when selecting and printing the move, we could also
	% have called the threaded_engine_self/1 predicate
	% instead of passing the name of the engine
	select_move(Me, Move) :-
		random::random(1, 6, N),
		move(N, Move),
		threaded_engine_post(writer, '~w:~w\n'-[Me,Move]).

	move(1, scissors).
	move(2, rock).
	move(3, paper).
	move(4, lizard).
	move(5, spock).

	% compare the moves and decide the outcome for each player
	decide_move(Move1, Move2, Result1, Result2) :-
		(	final_move(Move1, Move2, Result1, Result2) ->
			true
		;	final_move(Move2, Move1, Result2, Result1) ->
			true
		;	Result1 = draw,
			Result2 = draw
		).

	% Scissors cuts Paper
	final_move(scissors, paper, win, lose).
	% Paper covers Rock
	final_move(paper, rock, win, lose).
	% Rock crushes Lizard
	final_move(rock, lizard, win, lose).
	% Lizard poisons Spock
	final_move(lizard, spock, win, lose).
	% Spock smashes Scissors
	final_move(spock, scissors, win, lose).
	% Scissors decapitates Lizard
	final_move(scissors, lizard, win, lose).
	% Lizard eats Paper
	final_move(lizard, paper, win, lose).
	% Paper disproves Spock
	final_move(paper, spock, win, lose).
	% Spock vaporizes Rock
	final_move(spock, rock, win, lose).
	% (and as it always has) Rock crushes Scissors
	final_move(rock, scissors, win, lose).

:- end_object.
