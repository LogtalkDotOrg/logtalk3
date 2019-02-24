%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
% page on the Facade design pattern:
%
% https://en.wikipedia.org/wiki/Facade_pattern


% complex computer parts

:- object(cpu).

	:- public([
		freeze/0, jump/1, execute/0
	]).

	freeze :-
		write('Freezing processor.'), nl.

	jump(Position) :-
		write('Jumping to: '), write(Position), nl.

	execute :-
		write('Executing.'), nl.

:- end_object.


:- object(memory).

	:- public(load/2).

	load(Position, Data) :-
		write('Loading from '), write(Position),
		write(' data: '), write(Data), nl.

:- end_object.


:- object(ssd).

	:- public(read/3).

	read(LBA, Size, Data) :-
		atom_concat('Some data from sector ', LBA, Data0),
		atom_concat(Data0, ' with size ', Data1),
		atom_concat(Data1, Size, Data).

:- end_object.


% a computer defined as a facade using a parametric object for
% defining its parts

:- object(computer_facade(_CPU_, _Memory_, _SSD_)).

	% the facade abstracts the complexity of starting a computer
	% to provide its clients with a simplified interface
	:- public(start/0).

	start :-
		_CPU_::freeze,
		_SSD_::read('100', '1024', Data),
		_Memory_::load('0x00', Data),
		_CPU_::jump('0x00'),
		_CPU_::execute.

:- end_object.


% an alternative definition for the computer facade; in a real scenario,
% there would be a constructor capable of assembling a specific computer
% from specific parts, that in turn would be defined or created as
% descendant objects for the generic prototypes above or from equivalent
% classes; but the main point is that a facade abstracts the complexity
% of the subsystems, isolating its clients from the subsystem details

:- object(computer_facade).

	:- public(start/0).

	start :-
		cpu::freeze,
		ssd::read('100', '1024', Data),
		memory::load('0x00', Data),
		cpu::jump('0x00'),
		cpu::execute.

:- end_object.
