%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.4.4

:- dynamic(insect/1).
insect(ant).
insect(bee).

:- dynamic(foo/1).
foo(X) :- call(X), call(X).
foo(X) :- call(X) -> call(X).

bar(_X) :- true.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/10/14,
		comment is 'Unit tests for the ISO Prolog standard abolish/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.4.4

	succeeds(iso_abolish_1_01) :-
		{abolish(foo/2)}.

	throws(iso_abolish_1_02, error(instantiation_error,_)) :-
		{abolish(foo/_)}.

	throws(iso_abolish_1_03, error(type_error(predicate_indicator,foo),_)) :-
		{abolish(foo)}.

	throws(iso_abolish_1_04, error(type_error(predicate_indicator,foo(X)),_)) :-
		{abolish(foo(X))}.

	throws(iso_abolish_1_05, [error(permission_error(modify,static_procedure,abolish/1),_), error(permission_error(modify,static_procedure,':'(user,abolish/1)),_)]) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{abolish(abolish/1)}.

	succeeds(eddbali_abolish_1_06) :-
		{abolish(foo/1)}.

	succeeds(eddbali_abolish_1_07) :-
		findall(X, {insect(X), abolish(insect/1)}, L),
		L == [ant, bee].

	throws(eddbali_abolish_1_08, error(instantiation_error,_)) :-
		{abolish(foo/_)}.

	throws(eddbali_abolish_1_09, [error(permission_error(modify,static_procedure,bar/1),_), error(permission_error(modify,static_procedure,':'(user,bar/1)),_)]) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{abolish(bar/1)}.

	throws(eddbali_abolish_1_10, error(type_error(integer,a),_)) :-
		{abolish(foo/a)}.

	throws(eddbali_abolish_1_11, error(domain_error(not_less_than_zero,-1),_)) :-
		{abolish(foo/(-1))}.

	:- if(current_prolog_flag(max_arity, unbounded)).
		succeeds(eddbali_abolish_1_12) :-
			true.
	:- else.
		throws(eddbali_abolish_1_12, error(representation_error(max_arity),_)) :-
			current_prolog_flag(max_arity, MaxArity),
			X is MaxArity + 1,
			{abolish(foo/X)}.
	:- endif.

	throws(eddbali_abolish_1_13, error(type_error(atom,5),_)) :-
		{abolish(5/2)}.

	throws(eddbali_abolish_1_14, error(type_error(predicate_indicator,insect),_)) :-
		{abolish(insect)}.

:- end_object.
