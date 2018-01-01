%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- object(hanoi(_Threads)).

	:- info([
		version is 1.1,
		date is 2007/12/27,
		author is 'Paulo Moura',
		comment is 'Multi-threaded version of the Towers of Hanoi problem.',
		parameters is ['Threads' - 'Number of threads to use. Valid values are 1, 2, 4, 8, 16, etc.']
	]).

	:- threaded.

	:- public(run/1).
	:- mode(run(+integer), one).
	:- info(run/1, [
		comment is 'Simulates solving the Towers of Hanoi problem for the specified number of disks without actually returning a list of the necessary disk moves.',
		argnames is ['Disks']
	]).

	:- public(run/2).
	:- mode(run(+integer, -list), one).
	:- info(run/2, [
		comment is 'Solves the towers of Hanoi problem for the specified number of disks returning a list of the necessary disk moves.',
		argnames is ['Disks', 'Moves']
	]).

	:- public(write_moves/1).
	:- mode(write_moves(+list), one).
	:- info(write_moves/1, [
		comment is 'Writes a list of disk moves to the standard output (one disk move per line).',
		argnames is ['Moves']
	]).

	run(Disks) :-
		parameter(1, Threads),
		mt_move(Threads, Disks, left, middle, right).

	mt_move(1, Disks, Left, Aux, Right) :- !,
		st_move(Disks, Left, Aux, Right).
	mt_move(Threads, Disks, Left, Aux, Right) :-
		Threads > 1,
		Threads2 is Threads//2,
		Disks > 1,
		Disks2 is Disks - 1,
		threaded((
			mt_move(Threads2, Disks2, Left, Right, Aux),
			mt_move(Threads2, Disks2, Aux, Left, Right)
		)).

	st_move(1, _, _, _) :- !.
	st_move(Disks, Left, Aux, Right) :-
		Disks > 1,
		Disks2 is Disks - 1,
		st_move(Disks2, Left, Right, Aux),
		st_move(Disks2, Aux, Left, Right).

	run(Disks, Moves) :-
		parameter(1, Threads),
		mt_move(Threads, Disks, left, middle, right, [], Moves).

	mt_move(1, Disks, Left, Aux, Right, Acc, Moves) :- !,
		st_move(Disks, Left, Aux, Right, Acc, Moves).
	mt_move(Threads, Disks, Left, Aux, Right, Acc, Moves) :-
		Threads > 1,
		Threads2 is Threads//2,
		Disks > 1,
		Disks2 is Disks - 1,
		threaded((
			mt_move(Threads2, Disks2, Left, Right, Aux, [Left-Right| Acc2], Moves),
			mt_move(Threads2, Disks2, Aux, Left, Right, Acc, Acc2)
		)).

	st_move(1, Left, _, Right, Acc, [Left-Right| Acc]) :- !.
	st_move(Disks, Left, Aux, Right, Acc, Moves) :-
		Disks > 1,
		Disks2 is Disks - 1,
		st_move(Disks2, Left, Right, Aux, [Left-Right| Acc2], Moves),
		st_move(Disks2, Aux, Left, Right, Acc, Acc2).

	write_moves([]).
	write_moves([Move| Moves]) :-
		write_move(Move), nl,
		write_moves(Moves).

	write_move(Pole1-Pole2) :-
		write('Move a disk from '),
		writeq(Pole1),
		write(' to '),
		writeq(Pole2),
		write('.'),
		nl.

:- end_object.
