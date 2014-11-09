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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/11/09,
		comment is 'Unit tests for the ISO Prolog standard once/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.15.2.4

	succeeds(iso_once_1_01) :-
		{once(!)}.

	succeeds(iso_once_1_02) :-
		findall(X, {once(!), (X=1; X=2)}, L),
		L == [1, 2].

	succeeds(iso_once_1_03) :-
		{once(repeat)}.

	fails(iso_once_1_04) :-
		{once(fail)}.

	:- if(current_logtalk_flag(coinduction, supported)).
		succeeds(iso_once_1_05) :-
			{once((X = f(X)))}.
	:- else.
		- succeeds(iso_once_1_05) :-
			% STO; Undefined
			{once((X = f(X)))}.
	:- endif.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(eddbali_once_1_06, error(type_error(callable,3),_)) :-
		% try to dealy the error to runtime
		G = 3, {once(G)}.

	throws(eddbali_once_1_07, error(instantiation_error,_)) :-
		{once(_X)}.

:- end_object.
