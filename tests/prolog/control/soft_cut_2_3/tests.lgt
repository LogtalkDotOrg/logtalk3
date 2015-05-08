%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% database for the tests

a(1). a(2). a(3).

b(4). b(5). b(6).

c(7). c(8). c(9).

condition_opaque_to_cut_2 :-
	'*->'((!,fail), true).
condition_opaque_to_cut_2.

condition_opaque_to_cut_2(1) :-
	'*->'(!, true).
condition_opaque_to_cut_2(2).

condition_opaque_to_cut_3 :-
	';'('*->'((!,fail), true), fail).
condition_opaque_to_cut_3.

condition_opaque_to_cut_3(1) :-
	';'('*->'(!, true), fail).
condition_opaque_to_cut_3(2).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2015/05/08,
		comment is 'Unit tests for the soft-cut (*->)/2 control construct that is becoming a de facto standard.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	succeeds(commons_soft_cut_2_3_01) :-
		{'*->'(true, true)}.

	fails(commons_soft_cut_2_3_02) :-
		{'*->'(true, fail)}.

	fails(commons_soft_cut_2_3_03) :-
		{'*->'(fail, true)}.

	fails(commons_soft_cut_2_3_04) :-
		{'*->'(fail, fail)}.

	succeeds(commons_soft_cut_2_3_05) :-
		{';'('*->'(true, true), fail)}.

	succeeds(commons_soft_cut_2_3_06) :-
		{';'('*->'(fail, true), true)}.

	fails(commons_soft_cut_2_3_07) :-
		{';'('*->'(true, fail), fail)}.

	fails(commons_soft_cut_2_3_08) :-
		{';'('*->'(fail, true), fail)}.

	succeeds(commons_soft_cut_2_3_09) :-
		findall(X-Y, {';'('*->'(a(X),b(Y)), c(_))}, L),
		L == [1-4, 1-5, 1-6, 2-4, 2-5, 2-6, 3-4, 3-5, 3-6].

	succeeds(commons_soft_cut_2_3_10) :-
		findall(Z, {';'('*->'(fail,b(_)), c(Z))}, L),
		L == [7, 8, 9].

	succeeds(commons_soft_cut_2_3_11) :-
		findall(Z, {';'('*->'((!,fail),b(_)), c(Z))}, L),
		L == [7, 8, 9].

	succeeds(commons_soft_cut_2_3_12) :-
		{condition_opaque_to_cut_2}.

	succeeds(commons_soft_cut_2_3_13) :-
		findall(X, {condition_opaque_to_cut_2(X)}, L),
		L == [1, 2].

	succeeds(commons_soft_cut_2_3_14) :-
		{condition_opaque_to_cut_3}.

	succeeds(commons_soft_cut_2_3_15) :-
		findall(X, {condition_opaque_to_cut_3(X)}, L),
		L == [1, 2].

:- end_object.
