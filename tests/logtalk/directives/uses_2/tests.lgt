%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(uses_2_test_object_1).

	:- public(p/1).
	p(1).

	:- public(q/1).
	q(2).

:- end_object.



% test object for calling user-defined predicates in "user"
% it requires a back-end Prolog compiler that supports the
% definition of meta-predicates in plain Prolog

:- object(uses_2_test_object_2).

	:- uses(user, [foo/1]).

	:- public(p/1).
	p(X) :-
		foo(X).

	:- if((
		current_logtalk_flag(prolog_dialect, Dialect),
		(Dialect == eclipse; Dialect == sicstus; Dialect = swi; Dialect = yap)
	)).

	:- uses(user, [bar/1]).

	:- public(mp/1).
	mp(X) :-
		bar(l(X)).

	l(2).

	:- endif.

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/04/28,
		comment is 'Unit tests for the uses/2 built-in directive.'
	]).

	:- uses(uses_2_test_object_1, [p/1, q/1::r/1]).

	test(uses_2_1) :-
		p(X),
		X == 1.

	test(uses_2_2) :-
		r(X),
		X == 2.

	test(uses_2_3) :-
		uses_2_test_object_2::p(X),
		X == 1.

	:- if((
		current_logtalk_flag(prolog_dialect, Dialect),
		(Dialect == eclipse; Dialect == sicstus; Dialect = swi; Dialect = yap)
	)).

	test(uses_2_4) :-
		uses_2_test_object_2::mp(X),
		X == 2.

	:- endif.

:- end_object.
