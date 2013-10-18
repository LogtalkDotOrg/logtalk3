%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(slow_print).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/10/18,
		comment is 'Simple example for using the synchronized/1 predicate directive.'
	]).

	:- threaded.

	:- public(start/0).

	:- private([slow_print_abc/0, slow_print_123/0]).
	:- synchronized([slow_print_abc/0, slow_print_123/0]).

	start :-
		% launch two threads, running never ending goals
		threaded((
			repeat_abc,
			repeat_123
		)).

	repeat_abc :-
		repeat, slow_print_abc, fail.

	repeat_123 :-
		repeat, slow_print_123, fail.

	slow_print_abc :-
		write(a), thread_sleep(0.2),
		write(b), thread_sleep(0.2),
		write(c), nl.

	slow_print_123 :-
		write(1), thread_sleep(0.2),
		write(2), thread_sleep(0.2),
		write(3), nl.

:- end_object.
