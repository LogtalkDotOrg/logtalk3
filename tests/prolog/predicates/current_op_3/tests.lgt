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
		comment is 'Unit tests for the ISO Prolog standard current_op/3 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.14.4.4

	succeeds(iso_current_op_3_01) :-
		{current_op(P, T, ':-')},
		P == 1200, T == xfx.

	succeeds(iso_current_op_3_02) :-
		{current_op(P, T, '-->')},
		P == 1200, T == xfx.

	succeeds(iso_current_op_3_03) :-
		{current_op(P, T, ':-')},
		P == 1200, T == fx.

	succeeds(iso_current_op_3_04) :-
		{current_op(P, T, '?-')},
		P == 1200, T == xfx.

	succeeds(iso_current_op_3_05) :-
		{current_op(P, T, ';')},
		P == 1100, T == xfy.

	succeeds(iso_current_op_3_06) :-
		{current_op(P, T, '->')},
		P == 1050, T == xfy.

	succeeds(iso_current_op_3_07) :-
		{current_op(P, T, ',')},
		P == 1000, T == xfy.

	succeeds(iso_current_op_3_08) :-
		{current_op(P, T, '\\+')},
		P == 900, T == fy.

	succeeds(iso_current_op_3_09) :-
		{current_op(P, T, '=')},
		P == 700, T == xfx.

	succeeds(iso_current_op_3_10) :-
		{current_op(P, T, '\\=')},
		P == 700, T == xfx.

	succeeds(iso_current_op_3_11) :-
		{current_op(P, T, '==')},
		P == 700, T == xfx.

	succeeds(iso_current_op_3_12) :-
		{current_op(P, T, '\\==')},
		P == 700, T == xfx.

	succeeds(iso_current_op_3_13) :-
		{current_op(P, T, '@<')},
		P == 700, T == xfx.

	succeeds(iso_current_op_3_14) :-
		{current_op(P, T, '@=<')},
		P == 700, T == xfx.

	succeeds(iso_current_op_3_15) :-
		{current_op(P, T, '@>')},
		P == 700, T == xfx.

	succeeds(iso_current_op_3_16) :-
		{current_op(P, T, '@>=')},
		P == 700, T == xfx.

	succeeds(iso_current_op_3_17) :-
		{current_op(P, T, '=..')},
		P == 700, T == xfx.

	succeeds(iso_current_op_3_18) :-
		{current_op(P, T, 'is')},
		P == 700, T == xfx.

	succeeds(iso_current_op_3_19) :-
		{current_op(P, T, '=:=')},
		P == 700, T == xfx.

	succeeds(iso_current_op_3_20) :-
		{current_op(P, T, '=\\=')},
		P == 700, T == xfx.

	succeeds(iso_current_op_3_21) :-
		{current_op(P, T, '<')},
		P == 700, T == xfx.

	succeeds(iso_current_op_3_22) :-
		{current_op(P, T, '=<')},
		P == 700, T == xfx.

	succeeds(iso_current_op_3_23) :-
		{current_op(P, T, '>')},
		P == 700, T == xfx.

	succeeds(iso_current_op_3_24) :-
		{current_op(P, T, '>=')},
		P == 700, T == xfx.

	succeeds(iso_current_op_3_25) :-
		{current_op(P, T, '+')},
		P == 500, T == yfx.

	succeeds(iso_current_op_3_26) :-
		{current_op(P, T, '-')},
		P == 500, T == yfx.

	succeeds(iso_current_op_3_27) :-
		{current_op(P, T, '/\\')},
		P == 500, T == yfx.

	succeeds(iso_current_op_3_28) :-
		{current_op(P, T, '\\/')},
		P == 500, T == yfx.

	succeeds(iso_current_op_3_29) :-
		{current_op(P, T, '*')},
		P == 400, T == yfx.

	succeeds(iso_current_op_3_30) :-
		{current_op(P, T, '/')},
		P == 400, T == yfx.

	succeeds(iso_current_op_3_31) :-
		{current_op(P, T, '//')},
		P == 400, T == yfx.

	succeeds(iso_current_op_3_32) :-
		{current_op(P, T, 'rem')},
		P == 400, T == yfx.

	succeeds(iso_current_op_3_33) :-
		{current_op(P, T, 'mod')},
		P == 400, T == yfx.

	succeeds(iso_current_op_3_34) :-
		{current_op(P, T, '<<')},
		P == 400, T == yfx.

	succeeds(iso_current_op_3_35) :-
		{current_op(P, T, '>>')},
		P == 400, T == yfx.

	succeeds(iso_current_op_3_36) :-
		{current_op(P, T, '**')},
		P == 200, T == xfx.

	succeeds(iso_current_op_3_37) :-
		{current_op(P, T, '^')},
		P == 200, T == xfy.

	succeeds(iso_current_op_3_38) :-
		{current_op(P, T, '-')},
		P == 200, T == fy.

	succeeds(iso_current_op_3_39) :-
		{current_op(P, T, '\\')},
		P == 200, T == fy.

:- end_object.
