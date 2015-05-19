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
		version is 1.2,
		author is 'Paulo Moura',
		date is 2015/05/19,
		comment is 'Unit tests for the ISO Prolog standard (\\+)/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.15.1.4

	fails(iso_not_1_01) :-
		{'\\+'(true)}.

	fails(iso_not_1_02) :-
		{'\\+'(!)}.

	succeeds(iso_not_1_03) :-
		{'\\+'((!,fail))}.

	succeeds(iso_not_1_04) :-
		findall(X, {(X=1;X=2), '\\+'((!,fail))}, L),
		L == [1, 2].

	succeeds(iso_not_1_05) :-
		{'\\+'(4 = 5)}.

	throws(iso_not_1_06, [error(type_error(callable,3),_), error(type_error(callable,'\\+'(3)),_)]) :-
		% the second exception term is a common but not strictly conforming alternative
		% try to force runtime goal checking
		G = '\\+'(3), {G}.

	throws(iso_not_1_07, error(instantiation_error,_)) :-
		% try to force runtime goal checking
		G = '\\+'(_X), {G}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		fails(iso_not_1_08) :-
			{'\\+'(X=f(X))}.
	:- else.
		- fails(iso_not_1_08) :-
			% STO; Undefined
			{'\\+'(X=f(X))}.
	:- endif.

	succeeds(lgt_not_1_09) :-
		{'\\+'('\\+'(X=1))},
		var(X).

:- end_object.
