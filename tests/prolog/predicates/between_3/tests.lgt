%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if(current_logtalk_flag(prolog_dialect, xsb)).
	:- import(from(/(between,3), basics)).
:- endif.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/04/05,
		comment is 'Unit tests for the de facto Prolog standard between/3 built-in predicate.'
	]).

	succeeds(commons_between_3_01) :-
		findall(N, {between(1,3,N)}, L),
		L == [1, 2, 3].

	succeeds(commons_between_3_02) :-
		findall(N, {between(1,1,N)}, L),
		L == [1].

	fails(commons_between_3_03) :-
		{between(1, 0, _)}.

	throws(commons_between_3_04, error(instantiation_error,_)) :-
		{between(_, 3, _)}.

	throws(commons_between_3_05, error(instantiation_error,_)) :-
		{between(1, _, _)}.

	throws(commons_between_3_06, error(type_error(integer,a),_)) :-
		{between(a, 3, _)}.

	throws(commons_between_3_07, error(type_error(integer,a),_)) :-
		{between(1, a, _)}.

	throws(commons_between_3_08, error(type_error(integer,a),_)) :-
		{between(1, 3, a)}.

:- end_object.
