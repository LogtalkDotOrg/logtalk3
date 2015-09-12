%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


:- object(shell).

	:- info([
		version is 1.0,
		date is 2004/4/29,
		author is 'Paulo Moura',
		comment is 'Simple example of command-line shell parsing.'
	]).

	:- public(parse/2).
	:- mode(parse(@list, -list), zero_or_one).
	:- info(parse/2, [
		comment is 'Parses a sequence of commands.',
		argnames is ['Sequence', 'Commands']
	]).

	parse(Sequence, Commands) :-
		phrase(commands(Commands), Sequence).

	commands([C| Cs]) -->
		command(C), separator, commands(Cs).
	commands([C]) -->
		command(C).

	separator --> ";".

	whitespace --> " ", whitespace.
	whitespace --> [].

	command(Cd) -->
		whitespace, "cd", whitespace, cdargs(Args), whitespace,
		{atom_concat(cd, Args, Cd)}.
	command(Ls) -->
		whitespace, "ls", whitespace, lsargs(Args), whitespace,
		{atom_concat(ls, Args, Ls)}.
	command(pwd) -->
		whitespace, "pwd", whitespace.

	cdargs(' ~') --> "~".
	cdargs(' ..') --> "..".
	cdargs(' .') --> ".".
	cdargs('') --> [].

	lsargs(' -l') --> "-l".
	lsargs(' -a') --> "-a".
	lsargs('') --> [].

:- end_object.
