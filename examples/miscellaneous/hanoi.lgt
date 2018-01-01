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


:- object(hanoi).

	:- info([
		version is 1.0,
		date is 1998/3/23,
		author is 'Paulo Moura',
		comment is 'Towers of Hanoi.'
	]).

	:- public(run/1).
	:- mode(run(+integer), one).

	:- info(run/1, [
		comment is 'Solves the towers of Hanoi problem for the specified number of disks.',
		argnames is ['Disks']
	]).

	run(Disks) :-
		move(Disks, left, middle, right).

	move(1, Left, _, Right) :-
		!,
		report(Left, Right).
	move(Disks, Left, Aux, Right) :-
		Disks2 is Disks - 1,
		move(Disks2, Left, Right, Aux),
		report(Left, Right),
		move(Disks2, Aux, Left, Right).

	report(Pole1, Pole2) :-
		write('Move a disk from '),
		writeq(Pole1),
		write(' to '),
		writeq(Pole2),
		write('.'),
		nl.

:- end_object.
