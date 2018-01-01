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


:- object(nasty1).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2013/10/18,
		comment is 'Simple example for illustrating the problems with lack of thread synchronization when calling methods with side-effects.'
	]).

	:- threaded.

	:- public(update_db/1).
	:- mode(update_db(-integer), one).
	:- info(update_db/1, [
		comment is 'Perform a database update with a long delay between retracting the old information and asserting the new one.',
		argnames is ['New']
	]).

	:- private(db/1).
	:- dynamic(db/1).

	:- public(io/1).
	:- mode(io(+atom), one).
	:- info(io/1, [
		comment is 'Write some characters to the standard output stream with a long delay between each write operation.',
		argnames is ['Chars']
	]).

	db(0).

	update_db(New) :-
		retract(db(Old)),
		waste_time,
		New is Old + 1,
		waste_time,
		assertz(db(New)).

	io(alpha) :-
		write(a), waste_time, write(b), waste_time, write(c), waste_time, write(d), waste_time, write(e),
		write(f), waste_time, write(g), waste_time, write(h), waste_time, write(i), waste_time, write(j),
		write(k), waste_time, write(l), waste_time, write(m), waste_time, write(n), waste_time, write(o),
		write(p), waste_time, write(q), waste_time, write(r), waste_time, write(s), waste_time, write(t),
		write(z), waste_time, write(y), waste_time, write(x), waste_time, write(w), waste_time, write(u),
		write(v), nl.

	io(digit) :-
		write(0), waste_time, write(1), waste_time, write(2), waste_time, write(3), waste_time, write(4),
		write(5), waste_time, write(6), waste_time, write(7), waste_time, write(8), waste_time, write(9), nl.

	waste_time :-
		thread_sleep(0.2).

:- end_object.


:- object(nasty2).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2013/10/18,
		comment is 'Simple example for using the "synchronized" predicate directive for multi-threading methods with side-effects.'
	]).

	:- threaded.

	:- public(update_db/1).
	:- synchronized(update_db/1).
	:- mode(update_db(-integer), one).
	:- info(update_db/1, [
		comment is 'Perform a database update with a long delay between retracting the old information and asserting the new one.',
		argnames is ['New']
	]).

	:- private(db/1).
	:- dynamic(db/1).

	:- public(io/1).
	:- synchronized(io/1).
	:- mode(io(+atom), one).
	:- info(io/1, [
		comment is 'Write some characters to the standard output stream with a long delay between each write operation.',
		argnames is ['Chars']
	]).

	db(0).

	update_db(New) :-
		retract(db(Old)),
		waste_time,
		New is Old + 1,
		waste_time,
		assertz(db(New)).

	io(alpha) :-
		write(a), waste_time, write(b), waste_time, write(c), waste_time, write(d), waste_time, write(e),
		write(f), waste_time, write(g), waste_time, write(h), waste_time, write(i), waste_time, write(j),
		write(k), waste_time, write(l), waste_time, write(m), waste_time, write(n), waste_time, write(o),
		write(p), waste_time, write(q), waste_time, write(r), waste_time, write(s), waste_time, write(t),
		write(z), waste_time, write(y), waste_time, write(x), waste_time, write(w), waste_time, write(u),
		write(v), nl.

	io(digit) :-
		write(0), waste_time, write(1), waste_time, write(2), waste_time, write(3), waste_time, write(4),
		write(5), waste_time, write(6), waste_time, write(7), waste_time, write(8), waste_time, write(9), nl.

	waste_time :-
		thread_sleep(0.2).

:- end_object.
