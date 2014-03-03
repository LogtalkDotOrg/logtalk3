%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(logging).

	:- public([
		init_log/0,
		add_log_entry/1,
		print_log/0,
		log_entry/2			% public access to log entries
	]).

	:- private(log_/2).		% table of log entries
	:- dynamic(log_/2).

	:- op(600, xfy, (:)).	% used to represent event time in the log

	% retractall/1 retracts clauses in "this", i.e. in the object importing the category
	init_log :-
		retractall(log_(_, _)),
		add_log_entry(start).

	% assertz/1 asserts clauses in "this", i.e. in the object importing the category
	add_log_entry(Entry) :-
		date::today(Year, Month, Day),
		time::now(Hours, Mins, Secs),
		assertz(log_(Year/Month/Day-Hours:Mins:Secs, Entry)).

	% log_/2 is a private predicate but we can still use the ::/2 message sending control
	% construct because the sender of the message is the object, "this", importing the
	% category where the predicate is declared
	print_log :-
		this(This),
			This::log_(Date, Entry),
			write(Date), write(' - '), write(Entry), nl,
		fail.
	print_log.

	log_entry(Date, Entry) :-
		this(This),
		This::log_(Date, Entry).

:- end_category.


:- object(object,
	imports(logging)).

	% the following two initialization goals are equivalent because, in this case,
	% "self" and "this" are the same object:

	%:- initialization(::init_log).		% starts lookup for init_log/0 in "self"
	:- initialization(^^init_log).		% starts lookup for init_log/0 in "this"

:- end_object.
