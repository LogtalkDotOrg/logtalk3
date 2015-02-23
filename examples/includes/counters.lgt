%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(counters).

	:- public([
		counter/2,
		inc_counter/1,
		dec_counter/1,
		save_counters/0
	]).

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

	:- private(counter_value_/2).
	:- dynamic(counter_value_/2).

	% load the counters persistent database file when the object is compiled and loaded
	:- include(includes('counters.pl')).

:- end_object.
