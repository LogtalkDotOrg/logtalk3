%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- object(counters).

	:- public([
		counter/2,
		inc_counter/1,
		dec_counter/1,
		save_counters/0
	]).

	:- private(counter_value_/2).
	:- dynamic(counter_value_/2).

	% load the counters persistent database file when the object is compiled and
	% loaded (use library notation for specifying the included file path for
	% portability as backend Prolog compilers diverge on the concept of current
	% directory)
	:- include(includes('counters.pl')).

	counter(Counter, Value) :-
		counter_value_(Counter, Value).

	inc_counter(Counter) :-
		retract(counter_value_(Counter, Old)),
		New is Old + 1,
		assertz(counter_value_(Counter, New)).

	dec_counter(Counter) :-
		retract(counter_value_(Counter, Old)),
		New is Old - 1,
		assertz(counter_value_(Counter, New)).

	save_counters :-
		% save the current state of the conters database to a persistent file
		logtalk::expand_library_path(includes, Directory),
		atom_concat(Directory, 'counters.pl', Path),
		open(Path, write, Stream),
		(	counter_value_(Counter, Value),
			write_canonical(Stream, counter_value_(Counter,Value)), write(Stream, '.\n'),
			fail
		;	true
		),
		close(Stream).

:- end_object.
