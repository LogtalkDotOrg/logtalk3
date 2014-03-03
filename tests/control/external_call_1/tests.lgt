%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(external_call_test_object).

	:- public(p/1).
	p(Goal) :-
		{Goal}.

	:- public(q/1).
	q(Xs) :-
		findall(X, (a(X), {!}), Xs).

	:- public(r/1).
	r(Xs) :-
		G = !,
		findall(X, (a(X), {G}), Xs).

	a(1).
	a(2).
	a(3).

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/11/15,
		comment is 'Unit tests for the {}/1 built-in control construct.'
	]).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

	throws(external_call_1_1, error(instantiation_error,_)) :-
		external_call_test_object::p(_).

	throws(external_call_1_2, error(type_error(callable,1),_)) :-
		external_call_test_object::p(1).

	succeeds(external_call_1_3) :-
		external_call_test_object::p(true).

	fails(external_call_1_4) :-
		external_call_test_object::p(fail).

	succeeds(external_call_1_5) :-
		external_call_test_object::q(Xs),
		Xs == [1,2,3].

	succeeds(external_call_1_6) :-
		external_call_test_object::r(Xs),
		Xs == [1,2,3].

:- end_object.
