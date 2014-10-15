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
		comment is 'Unit tests for the ISO Prolog standard nonvar/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.3.7.4

	succeeds(iso_nonvar_1_01) :-
		{nonvar(33.3)}.

	succeeds(iso_nonvar_1_02) :-
		{nonvar(foo)}.

	fails(iso_nonvar_1_03) :-
		{nonvar(_Foo)}.

	succeeds(iso_nonvar_1_04) :-
		{foo = Foo, nonvar(Foo)}.

	fails(iso_nonvar_1_05) :-
		{nonvar(_)}.

	succeeds(iso_nonvar_1_06) :-
		{nonvar(a(b))}.

:- end_object.
