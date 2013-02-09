%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(buffer(_MaxCapacity)).

	:- info([
		version is 2.1,
		author is 'Paulo Moura',
		date is 2007/9/16,
		comment is 'Producer-consumer problem with a bounded buffer.'
	]).

	:- threaded.

	:- public(put/1).
	:- mode(put(?integer), one).
	:- info(put/1, [
		comment is 'Put an item in the buffer.'
	]).

	:- public(get/1).
	:- mode(get(?integer), one).
	:- info(get/1, [
		comment is 'Get an item from the buffer.'
	]).

	:- private(item_/1).
	:- dynamic(item_/1).

	:- private(size_/1).
	:- dynamic(size_/1).

	size_(0).

	:- synchronized([put_item/1, get_item/1]).

	put_item(Item) :-
		parameter(1, MaxCapacity),
		assertz(item_(Item)),
		retract(size_(N)),
		N2 is N + 1,
		assertz(size_(N2)),
		write(' produced item '), write(Item),
		write(' ('), write(N2), write('/'), write(MaxCapacity), write(' items in the buffer'), write(')'), nl.

	get_item(Item) :-
		parameter(1, MaxCapacity),
		retract(item_(Item)),
		retract(size_(N)),
		N2 is N - 1,
		assertz(size_(N2)),
		write(' consumed item '), write(Item),
		write(' ('), write(N2), write('/'), write(MaxCapacity), write(' items in the buffer'), write(')'), nl.

	put(Item) :-
		parameter(1, MaxCapacity),
		size_(N),
		(	N =:= MaxCapacity ->		% if the maximum buffer capacity have been
			threaded_wait(vacancy),		% reached, wait until an item is consumed
			put(Item)					% be sure to consume all "vacancy" notifications before proceeding
		;	put_item(Item),
			(	N =:= 0 ->
				threaded_notify(not_empty)
			;	true
			)
		).

	get(Item) :-
		parameter(1, MaxCapacity),
		size_(N),
		(	N =:= 0 ->					% if the buffer is empty, wait
			threaded_wait(not_empty),	% until an item is produced
			get(Item)					% be sure to consume all "not_empty" notifications before proceeding
		;	get_item(Item),
			(	N =:= MaxCapacity ->
				threaded_notify(vacancy)
			;	true
			)
		).

:- end_object.


:- object(producer(_MaxTime)).

	:- public(run/1).

	run(N) :-
		run(0, N).

	run(N, N) :- !.
	run(M, N) :-
		M < N,
		parameter(1, MaxTime),
		random::random(1, MaxTime, Random),	% simulate a variable amount of 
		thread_sleep(Random),				% time to produce a new item
		buffer(7)::put(M),
		M2 is M + 1,
		run(M2, N).

:- end_object.


:- object(consumer(_MaxTime)).

	:- public(run/1).

	run(N) :-
		run(0, N).

	run(N, N) :- !.
	run(M, N) :-
		M < N,
		parameter(1, MaxTime),
		random::random(1, MaxTime, Random),	% simulate a variable amount of 
		thread_sleep(Random),				% time to produce a new item
		buffer(7)::get(_Item),
		M2 is M + 1,
		run(M2, N).

:- end_object.
