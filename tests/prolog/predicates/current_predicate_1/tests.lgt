%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 8.8.2.4

:- dynamic(cat/0).
cat.

:- dynamic(dog/0).
dog :- true.

elk(X) :- moose(X).

:- dynamic(insect/1).
insect(ant).
insect(bee).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2015/05/10,
		comment is 'Unit tests for the ISO Prolog standard current_predicate/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.8.2.4

	succeeds(iso_current_predicate_1_01) :-
		{current_predicate(dog/0)}.

	fails(iso_current_predicate_1_02) :-
		{current_predicate(current_predicate/1)}.

	succeeds(iso_current_predicate_1_03) :-
		{current_predicate(elk/Arity)},
		Arity == 1.

	fails(iso_current_predicate_1_04) :-
		{current_predicate(foo/_A)}.

	succeeds(iso_current_predicate_1_05) :-
		setof(Name, {current_predicate(Name/1)}, Names),
		memberchk(elk, Names), memberchk(insect, Names).

	throws(iso_current_predicate_1_06, error(type_error(predicate_indicator,4),_)) :-
		{current_predicate(4)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(eddbali_current_predicate_1_07, error(type_error(predicate_indicator,dog),_)) :-
		{current_predicate(dog)}.

	throws(eddbali_current_predicate_1_08, [error(type_error(predicate_indicator,0/dog),_), error(type_error(atom,0),_)]) :-
		% the first exception is the one specified in the ISO standard but the second one is common
		{current_predicate(0/dog)}.

	% tests from the ECLiPSe test suite

	throws(eclipse_current_predicate_1_09, [error(type_error(predicate_indicator,3/3),_), error(type_error(atom,3),_)]) :-
		% the first exception is the one specified in the ISO standard but the second one is common
		{current_predicate(3/3)}.

	throws(eclipse_current_predicate_1_10, [error(type_error(predicate_indicator,f/f),_), error(type_error(integer,f),_)]) :-
		% the first exception is the one specified in the ISO standard but the second one is common
		{current_predicate(f/f)}.

	throws(eclipse_current_predicate_1_11, [error(type_error(predicate_indicator,f/ -1),_), error(domain_error(not_less_than_zero,-1),_)]) :-
		% the first exception is the one specified in the ISO standard but the second one is common
		{current_predicate(f/ -1)}.

	% avoid library dependencies
	memberchk(X, [X| _]) :-
		!.
	memberchk(X, [_| L]) :-
		memberchk(X, L).

:- end_object.
