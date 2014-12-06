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
		date is 2014/12/06,
		comment is 'Unit tests for the ISO Prolog standard (;)/2 control construct.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.8.4

	succeeds(iso_if_then_else_3_01) :-
		{';'('->'(true, true), fail)}.

	succeeds(iso_if_then_else_3_02) :-
		{';'('->'(fail, true), true)}.

	fails(iso_if_then_else_3_03) :-
		{';'('->'(true, fail), fail)}.

	fails(iso_if_then_else_3_04) :-
		{';'('->'(fail, true), fail)}.

	succeeds(iso_if_then_else_3_05) :-
		{';'('->'(true, X=1), X=2)},
		X == 1.

	succeeds(iso_if_then_else_3_06) :-
		{';'('->'(fail, X=1), X=2)},
		X == 2.

	succeeds(iso_if_then_else_3_07) :-
		findall(X, {';'('->'(true, ';'(X=1, X=2)), true)}, L),
		L == [1,2].

	succeeds(iso_if_then_else_3_08) :-
		{';'('->'(';'(X=1, X=2), true), true)},
		X == 1.

	succeeds(iso_if_then_else_3_09) :-
		% the original example in the ISO/IEC 13211-1:1995(E) standard suffers from
		% a syntax error and was "fixed" in the ISO/IEC 13211-1:1995/Cor.1:2007;
		% however, with this fix (also used here) it's no longer a test for the
		% if-then-else control construct!
		{';'(('->'(!,fail), true), true)}.

	succeeds(lgt_if_then_else_3_10) :-
		% correct test goal for the botched TC1 fix in the previous test?
		% it makes sense to test for correct semantics when a cut is found
		% in the condition of an if-then-else
		{';'('->'((!, fail), true), true)}.

:- end_object.
