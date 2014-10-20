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
		comment is 'Unit tests for the ISO Prolog standard set_prolog_flag/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.17.1.4

	succeeds(iso_set_prolog_flag_2_01) :-
		{set_prolog_flag(unknown, fail)},
		{current_prolog_flag(unknown, fail)}.

	throws(iso_set_prolog_flag_2_02, error(instantiation_error,_)) :-
		{set_prolog_flag(_X, off)}.

	throws(iso_set_prolog_flag_2_03, error(type_error(atom,5),_)) :-
		{set_prolog_flag(5, decimals)}.

	throws(iso_set_prolog_flag_2_04, error(domain_error(prolog_flag,date),_)) :-
		{set_prolog_flag(date, 'July 1999')}.

	throws(iso_set_prolog_flag_2_05, error(domain_error(flag_value,debug+trace),_)) :-
		{set_prolog_flag(debug, trace)}.

	throws(eddbali_set_prolog_flag_2_06, error(permission_error(modify,flag,max_arity),_)) :-
		{set_prolog_flag(max_arity, 40)}.

:- end_object.
