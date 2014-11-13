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
		date is 2014/05/05,
		comment is 'Unit tests for the predicate_property/2 built-in method.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	throws(predicate_property_2_01, error(instantiation_error,logtalk(This::predicate_property(_,_),user))) :-
		this(This),
		{This::predicate_property(_, _)}.

	throws(predicate_property_2_02, error(type_error(callable, 1),logtalk(This::predicate_property(1, _),user))) :-
		this(This),
		{This::predicate_property(1, _)}.

	throws(predicate_property_2_03, error(domain_error(predicate_property, bar),logtalk(This::predicate_property(foo, bar),user))) :-
		this(This),
		{This::predicate_property(foo, bar)}.

	throws(predicate_property_2_04, error(instantiation_error, logtalk(_::predicate_property(foo,_),test_object))) :-
		{test_object::ie(_)}.

	throws(predicate_property_2_05, error(type_error(object_identifier, 1), logtalk(1::predicate_property(foo,_),test_object))) :-
		{test_object::te}.

	% Prolog built-in predicates are interpreted as private predicates

	succeeds(predicate_property_2_06) :-
		predicate_property(write(_), scope(Scope)),
		Scope == private.

	succeeds(predicate_property_2_07) :-
		this(This),
		This::predicate_property(write(_), scope(Scope)),
		Scope == private.

	fails(predicate_property_2_08) :-
		this(This),
		{This::predicate_property(write(_), _)}.

	% test properties of a user-defined predicate

	succeeds(predicate_property_2_09) :-
		test_object::predicate_property(ie(_), scope(Scope)),
		Scope == (public).

	succeeds(predicate_property_2_10) :-
		test_object::predicate_property(ie(_), static).

	succeeds(predicate_property_2_11) :-
		test_object::predicate_property(ie(_), logtalk).

	fails(predicate_property_2_12) :-
		test_object::predicate_property(ie(_), prolog).

	fails(predicate_property_2_13) :-
		test_object::predicate_property(ie(_), (dynamic)).

	fails(predicate_property_2_14) :-
		test_object::predicate_property(ie(_), meta_predicate(_)).

	fails(predicate_property_2_15) :-
		test_object::predicate_property(ie(_), non_terminal(_)).

	fails(predicate_property_2_16) :-
		test_object::predicate_property(ie(_), (multifile)).

	fails(predicate_property_2_17) :-
		test_object::predicate_property(ie(_), built_in).

	fails(predicate_property_2_18) :-
		test_object::predicate_property(ie(_), synchronized).

	% test properties of a user-defined meta-predicate

	succeeds(predicate_property_2_19) :-
		test_object::predicate_property(meta(_,_), meta_predicate(Template)),
		Template == meta(0, *).

	% test properties of a user-defined non-terminal

	succeeds(predicate_property_2_20) :-
		test_object::predicate_property(nt(_,_), non_terminal(NonTerminal)),
		NonTerminal == nt//0.

:- end_object.
