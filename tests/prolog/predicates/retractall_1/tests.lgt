%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% databse for tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.9.5.4

:- dynamic(insect/1).
insect(ant).
insect(bee).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/11/04,
		comment is 'Unit tests for the ISO Prolog standard retractall/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.9.5.4

	succeeds(iso_retractall_1_01) :-
		{retractall(insect(bee))}.

	succeeds(iso_retractall_1_02) :-
		{retractall(insect(_))}.

	succeeds(iso_retractall_1_03) :-
		{retractall(insect(spider))}.

	throws(iso_retractall_1_04, error(type_error(callable,3),_)) :-
		{retractall(3)}.

	:- if(current_logtalk_flag(prolog_conformance, iso_strict)).
		throws(iso_retractall_1_05, error(permission_error(modify,static_procedure,retractall/1),_)) :-
			{retractall(retractall(_))}.
	:- else.
		throws(iso_retractall_1_05, [error(permission_error(modify,static_procedure,retractall/1),_), error(permission_error(modify,static_procedure,':'(user,retractall/1)),_)]) :-
			% the second exception term is used in some of the Prolog compilers supporting modules
			{retractall(retractall(_))}.
	:- endif.

:- end_object.
