%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_logtalk_flag(source_data, on).


:- object(test_object).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/17/12,
		comment is 'Sample object for testing with the `source_data` flag turned on.']).

	:- set_logtalk_flag(complements, allow).
	:- set_logtalk_flag(dynamic_declarations, allow).
	:- set_logtalk_flag(context_switching_calls, deny).
	:- set_logtalk_flag(events, allow).

	:- public(a/1).
	:- if(current_logtalk_flag(coinduction, supported)).
		:- coinductive(a/1).
	:- endif.
	a(1).

	:- protected(b/2).
	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized(b/2).
	:- endif.
	b(1, 2).
	b(2, 1).

	:- private(c/3).
	:- dynamic(c/3).
	c(1, 2, 3).
	c(2, 3, 1).
	c(3, 1, 2).

	d(1, 2, 3, 4).
	d(2, 3, 4, 1).
	d(3, 4, 1, 2).
	d(4, 1, 2, 3).

	:- private(e/5).

:- end_object.
