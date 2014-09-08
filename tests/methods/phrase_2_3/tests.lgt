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
		date is 2014/09/08,
		comment is 'Unit tests for the phrase/2-3 built-in methods.'
	]).

	throws(phrase_2_1, error(permission_error(access, private_predicate, phrase/2), logtalk(logtalk::phrase(_, _), user))) :-
		{logtalk::phrase(_, _)}.

	throws(phrase_2_2, error(instantiation_error, logtalk(logtalk<<phrase(_, _), user))) :-
		{logtalk<<phrase(_, _)}.

	throws(phrase_2_3, error(type_error(callable,1), logtalk(logtalk<<phrase(1, _), user))) :-
		{logtalk<<phrase(1, _)}.

	throws(phrase_2_4, error(type_error(list,1), logtalk(logtalk<<phrase(foo, 1), user))) :-
		{logtalk<<phrase(foo, 1)}.

	throws(phrase_3_1, error(permission_error(access, private_predicate, phrase/3), logtalk(logtalk::phrase(_, _, _), user))) :-
		{logtalk::phrase(_, _, _)}.

	throws(phrase_3_2, error(instantiation_error, logtalk(logtalk<<phrase(_, _, _), user))) :-
		{logtalk<<phrase(_, _, _)}.

	throws(phrase_3_3, error(type_error(callable,1), logtalk(logtalk<<phrase(1, _, _), user))) :-
		{logtalk<<phrase(1, _, _)}.

	throws(phrase_3_4, error(type_error(list,1), logtalk(logtalk<<phrase(foo, 1, _), user))) :-
		{logtalk<<phrase(foo, 1, _)}.

	throws(phrase_3_5, error(type_error(list,1), logtalk(logtalk<<phrase(foo, _, 1), user))) :-
		{logtalk<<phrase(foo, _, 1)}.

:- end_object.
