%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% database for tests

a(1).
a(2).
a(3).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2015/02/26,
		comment is 'Unit tests for the de facto Prolog standard forall/2 built-in predicate.'
	]).

	succeeds(commons_forall_2_01) :-
		{forall(true, true)}.

	succeeds(commons_forall_2_02) :-
		{forall(fail, true)}.

	succeeds(commons_forall_2_03) :-
		{forall(fail, fail)}.

	succeeds(commons_forall_2_04) :-
		{forall(a(X), integer(X))}.

	fails(commons_forall_2_05) :-
		{forall(true, fail)}.

	fails(commons_forall_2_06) :-
		{forall(a(X), atom(X))}.

	throws(commons_forall_2_07, error(instantiation_error,_)) :-
		{forall(_, true)}.

	throws(commons_forall_2_08, error(instantiation_error,_)) :-
		{forall(true, _)}.

	throws(commons_forall_2_09, error(type_error(callable,1),_)) :-
		Goal = 1,
		{forall(Goal, true)}.

	throws(commons_forall_2_10, error(type_error(callable,1),_)) :-
		Goal = 1,
		{forall(true, Goal)}.

:- end_object.
