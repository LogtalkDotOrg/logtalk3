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
		version is 2.2,
		author is 'Paulo Moura',
		date is 2012/12/09,
		comment is 'Unit tests for the "securemp" example.']).

	:- discontiguous(fails/1).
	:- discontiguous(succeeds/1).
	:- discontiguous(throws/2).

	fails(rule_a) :-
		logtalk_load(rule_a).				% compile-time error

	throws(rule_a_variant, error(existence_error(procedure, scale/3), _)) :-
		logtalk_load(rule_a_variant),		% runtime error
		{client_a_variant::double([1,2,3], _)}.

	succeeds(rule_b_1) :-
		logtalk_load(rule_b_1).

	throws(rule_b_2, error(existence_error(procedure, term/0), _)) :-
		logtalk_load(rule_b_2),				% runtime error
		{client_b_2::test}.

	throws(rule_b_3, error(existence_error(procedure, a/2), _)) :-
		logtalk_load(rule_b_3),				% runtime error
		{client_b_3::test(_)}.

	succeeds(rule_b_3_variant) :-
		logtalk_load(rule_b_3_variant),		% suspicious meta-predicate
		{client_b_3_variant::test(X)},		% definition but no error
		X == 3.

	fails(rule_c) :-
		logtalk_load(rule_c).				% compile-time error

:- end_object.
