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
		comment is 'Unit tests for the ISO Prolog standard arithmetic comparison built-in predicates.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.7.1.4

	fails(iso_arithmetic_comparison_01) :-
		{'=:='(0,1)}.

	succeeds(iso_arithmetic_comparison_02) :-
		{'=\\='(0, 1)}.

	succeeds(iso_arithmetic_comparison_03) :-
		{'<'(0, 1)}.

	fails(iso_arithmetic_comparison_04) :-
		{'>'(0, 1)}.

	fails(iso_arithmetic_comparison_05) :-
		{'>='(0, 1)}.

	succeeds(iso_arithmetic_comparison_06) :-
		{'=<'(0, 1)}.

	succeeds(iso_arithmetic_comparison_07) :-
		{'=:='(1.0, 1)}.

	fails(iso_arithmetic_comparison_08) :-
		{=\=(1.0, 1)}.

	fails(iso_arithmetic_comparison_09) :-
		{'<'(1.0, 1)}.

	fails(iso_arithmetic_comparison_10) :-
		{'>'(1.0, 1)}.

	succeeds(iso_arithmetic_comparison_11) :-
		{'>='(1.0, 1)}.

	succeeds(iso_arithmetic_comparison_12) :-
		{'=<'(1.0, 1)}.

	succeeds(iso_arithmetic_comparison_13) :-
		{'=:='(3*2, 7-1)}.

	fails(iso_arithmetic_comparison_14) :-
		{'=\\='(3*2, 7-1)}.

	fails(iso_arithmetic_comparison_15) :-
		{'<'(3*2, 7-1)}.

	fails(iso_arithmetic_comparison_16) :-
		{'>'(3*2, 7-1)}.

	succeeds(iso_arithmetic_comparison_17) :-
		{'>='(3*2, 7-1)}.

	succeeds(iso_arithmetic_comparison_18) :-
		{'=<'(3*2, 7-1)}.

	throws(iso_arithmetic_comparison_19, error(instantiation_error,_)) :-
		{'=:='(_X, 5)}.

	throws(iso_arithmetic_comparison_20, error(instantiation_error,_)) :-
		{=\=(_X, 5)}.

	throws(iso_arithmetic_comparison_21, error(instantiation_error,_)) :-
		{'<'(_X, 5)}.

	throws(iso_arithmetic_comparison_22, error(instantiation_error,_)) :-
		{'>'(_X, 5)}.

	throws(iso_arithmetic_comparison_23, error(instantiation_error,_)) :-
		{'>='(_X, 5)}.

	throws(iso_arithmetic_comparison_24, error(instantiation_error,_)) :-
		{'=<'(_X, 5)}.

:- end_object.
