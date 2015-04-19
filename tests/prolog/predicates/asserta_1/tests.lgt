%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.1.4

:- dynamic(legs/2).
:- dynamic(foo/1).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/04/19,
		comment is 'Unit tests for the ISO Prolog standard asserta/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.1.4

	succeeds(iso_asserta_1_01) :-
		{asserta(legs(octopus, 8))}.

	succeeds(iso_asserta_1_02) :-
		{asserta((legs(A,4):-animal(A)))}.

	succeeds(iso_asserta_1_03) :-
		{asserta((foo(X) :- X,call(X)))}.

	throws(iso_asserta_1_04, error(instantiation_error,_)) :-
		{asserta(_)}.

	throws(iso_asserta_1_05, error(type_error(callable,4),_)) :-
		{asserta(4)}.

	throws(iso_asserta_1_06, error(type_error(callable,4),_)) :-
		{asserta((foo :- 4))}.

	:- if(current_logtalk_flag(prolog_conformance, iso_strict)).
		throws(iso_asserta_1_07, error(permission_error(modify,static_procedure,atom/1),_)) :-
			{asserta((atom(_) :- true))}.
	:- else.
		throws(iso_asserta_1_07, [error(permission_error(modify,static_procedure,atom/1),_), error(permission_error(modify,static_procedure,':'(user,atom/1)),_)]) :-
			% the second exception term is used in some of the Prolog compilers supporting modules
			{asserta((atom(_) :- true))}.
	:- endif.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	succeeds(eddbali_asserta_1_08) :-
		findall(X-Y, {asserta(insct(bee)),insct(X),asserta(insct(ant)),insct(Y)}, L),
		L == [bee-ant, bee-bee].

	throws(lgt_asserta_1_09, error(instantiation_error,_)) :-
		{asserta((_ :- foo))}.

	throws(lgt_asserta_1_10, error(instantiation_error,_)) :-
		{asserta((_ :- _))}.

	throws(lgt_asserta_1_11, error(type_error(callable,4),_)) :-
		{asserta((4 :- foo))}.

:- end_object.
