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


:- object(team).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/10/14,
		comment is 'Example of synchronous concurrency as described in the corresponding Rosetta Code task.'
	]).

	:- threaded.

	:- public(start/0).
	start :-
		threaded((
			reader,
			writer(0)
		)).

	reader :-
		open('input.txt', read, Stream),
		repeat,
			read_term(Stream, Term, []),
			threaded_notify(term(Term)),
		Term == end_of_file,
		!,
		close(Stream),
		threaded_wait(lines(Lines)),
		write('Number of lines: '), write(Lines), nl.

	writer(N0) :-
		threaded_wait(term(Term)),
		(	Term == end_of_file ->
			threaded_notify(lines(N0))
		;	N is N0 + 1,
			write(Term), nl,
			writer(N)
		).

:- end_object.
