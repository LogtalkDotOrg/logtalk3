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
		date is 2014/10/14,
		comment is 'Unit tests for the ISO Prolog standard op/3 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.14.3.4

	succeeds(iso_op_3_01) :-
		{op(30, xfy, ++)},
		{current_op(30, xfy, ++), op(0, xfy, ++)}.

	succeeds(iso_op_3_02) :-
		{op(30, xfy, ++), op(0, xfy, ++)},
		{\+ current_op(_, _, ++)}.

	throws(iso_op_3_03, error(type_error(integer,max),_)) :-
		{op(max, xfy, ++)}.

	throws(iso_op_3_04, error(domain_error(operator_priority,-30),_)) :-
		{op(-30, xfy, ++)}.

	throws(iso_op_3_05, error(domain_error(operator_priority,1201),_)) :-
		{op(1201, xfy, ++)}.

	throws(iso_op_3_06, error(instantiation_error,_)) :-
		{op(30, _XFY, ++)}.

	throws(iso_op_3_07, error(domain_error(operator_specifier,yfy),_)) :-
		{op(30, yfy, ++)}.

	throws(iso_op_3_08, error(type_error(list,0),_)) :-
		{op(30, xfy, 0)}.

	succeeds(iso_op_3_09) :-
		{(op(30, xfy, ++), op(40, xfx, ++))},
		{current_op(40, xfx, ++), op(0, xfx, ++)}.

	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == eclipse; Dialect == swi; Dialect == yap))).
		% these Prolog systems support the definition of an atom as both an infix and a postfix operator
		succeeds(iso_op_3_10) :-
			true.
	:- else.
		throws(iso_op_3_10, error(permission_error(create,operator,++),_)) :-
			{op(30, xfy, ++), op(50, yf, ++)}.
	:- endif.

:- end_object.
