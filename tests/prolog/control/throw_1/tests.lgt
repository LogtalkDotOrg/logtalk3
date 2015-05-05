%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/05/05,
		comment is 'Unit tests for the ISO Prolog standard throw/1 control construct.'
	]).

	% tests from the Logtalk portability work

	throws(lgt_throw_1_1, error(instantiation_error,_)) :-
		{throw(_)}.

	throws(lgt_throw_1_2, my_error) :-
		{throw(my_error)}.

	% tests from the ECLiPSe test suite

	throws(eclipse_throw_1_3, a) :-
		{throw(a)}.

	throws(eclipse_throw_1_4, 1) :-
		{throw(1)}.

	throws(eclipse_throw_1_5, 1.0) :-
		{throw(1.0)}.

	throws(eclipse_throw_1_6, f(a)) :-
		{throw(f(a))}.

	succeeds(eclipse_throw_1_7) :-
		{catch(throw(f(_)), T, true)},
		nonvar(T), T = f(Y), var(Y).

:- end_object.
