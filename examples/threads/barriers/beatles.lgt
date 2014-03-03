%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(beatles).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/10/23,
		comment is 'Simple example of using a barrier to synchronize a set of threads.'
	]).

	:- threaded.

	:- public(sing_along/0).
	:- mode(sing_along, one).
	:- info(sing_along/0, [
		comment is 'Wait for all threads to say "hello" and then proceed with the threads saying "goodbye".'
	]).

	:- uses(random, [random/3]).

	sing(Thread) :-
		random(1, 3, BusyHello), thread_sleep(BusyHello),			% spend some time before saying hello
		write(hello(Thread)), flush_output,
		threaded_notify(ready(Thread)),								% notify barrier that you have arrived
		threaded_wait(go(Thread)),									% wait for green light to cross the barrier
		random(1, 3, BusyGoodbye), thread_sleep(BusyGoodbye),		% spend some time before saying goodbye
		write(goodbye(Thread)), flush_output.

	sing_along :-
		threaded_ignore(sing(1)),									% start the threads
		threaded_ignore(sing(2)),
		threaded_ignore(sing(3)),
		threaded_ignore(sing(4)),
		threaded_wait([ready(1), ready(2), ready(3), ready(4)]),	% wait for all threads to reach the barrier
		nl, write('Enough of hellos! Time for goodbyes!'), nl,
		threaded_notify([go(1), go(2), go(3), go(4)]).				% give green light to all threads to cross the barrier

:- end_object.
