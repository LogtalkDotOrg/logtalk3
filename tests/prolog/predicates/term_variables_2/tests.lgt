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
		comment is 'Unit tests for the ISO Prolog standard term_variables/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.5.5.4

	succeeds(iso_term_variables_2_01) :-
		{term_variables(t, Vars)},
		Vars == [].

	succeeds(iso_term_variables_2_02) :-
		{term_variables(A+B*C/B-D, Vars)},
		Vars == [A, B, C, D].

	throws(iso_term_variables_2_03, error(type_error(list,[_,_|a]),_)) :-
		{term_variables(t, [_, _|a])}.

	succeeds(iso_term_variables_2_04) :-
		{S=B+T, T=A*B, term_variables(S, Vars)},
		Vars == [B, A], T == A*B, S == B+A*B.

	succeeds(iso_term_variables_2_05) :-
		{T=A*B, S=B+T, term_variables(S, Vars)},
		Vars == [B, A], T == A*B, S == B+A*B.

	succeeds(iso_term_variables_2_06) :-
		{term_variables(A+B+B, [B|Vars])},
		A == B, Vars == [B].

	- succeeds(iso_term_variables_2_07) :-
		% STO; Undefined.
		{term_variables(_X+Vars, Vars), Vars = [_, _]}.

:- end_object.
