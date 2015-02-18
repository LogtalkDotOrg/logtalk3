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
		date is 2015/02/18,
		comment is 'Unit tests for the ISO Prolog standard compare/3 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.4.2.4

	succeeds(iso_compare_3_01) :-
		{compare(Order, 3, 5)},
		Order == (<).

	succeeds(iso_compare_3_02) :-
		{compare(Order, d, d)},
		Order == (=).

	succeeds(iso_compare_3_03) :-
		{compare(Order, Order, <)},
		Order == (<).

	fails(iso_compare_3_04) :-
		{compare(<, <, <)}.

	throws(iso_compare_3_05, error(type_error(atom,1+2),_)) :-
		{compare(1+2, 3, 3.0)}.

	throws(iso_compare_3_06, error(domain_error(order,>=),_)) :-
		{compare(>=, 3, 3.0)}.

	% standard order tests

	succeeds(lgt_compare_3_07) :-
		{compare(<, _X, 1.1)}.

	succeeds(lgt_compare_3_08) :-
		{compare(<, 1.1, 1)}.

	succeeds(lgt_compare_3_09) :-
		{compare(<, 1, a)}.

	succeeds(lgt_compare_3_10) :-
		{compare(<, a, a(_))}.

	succeeds(lgt_compare_3_11) :-
		{compare(<, a(_), a(_,_))}.

	succeeds(lgt_compare_3_12) :-
		{compare(<, b(_), a(_,_))}.

	succeeds(lgt_compare_3_13) :-
		{compare(<, a(1,2), a(1,3))}.

	succeeds(lgt_compare_3_14) :-
		{compare(<, a(1,2), b(1,2))}.

	% other tests

	succeeds(lgt_compare_3_15) :-
		{compare(>, (4,1,0), (4,0,1))}.

	fails(lgt_compare_3_16) :-
		{compare(>, (4,0,1), (4,1,0))}.

	fails(lgt_compare_3_17) :-
		{compare(<, (4,1,0), (4,0,1))}.

	succeeds(lgt_compare_3_18) :-
		{compare(<, (4,0,1), (4,1,0))}.

	succeeds(lgt_compare_3_19) :-
		{compare(>, (4,1,0), (4,0,1))}.

	fails(lgt_compare_3_20) :-
		{compare(>, (4,0,1), (4,1,0))}.

	fails(lgt_compare_3_21) :-
		{compare(<, (4,1,0), (4,0,1))}.

	succeeds(lgt_compare_3_22) :-
		{compare(<, (4,0,1), (4,1,0))}.

:- end_object.
