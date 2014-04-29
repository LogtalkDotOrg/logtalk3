%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% objects for testing predicate aliases defined in classes and instances

:- object(alias_2_test_metaclass,
	instantiates(alias_2_test_metaclass)).

:- end_object.


:- object(alias_2_test_class,
	instantiates(alias_2_test_metaclass)).

	:- public(p/1).
	p(1).

:- end_object.


:- object(alias_2_test_subclass,
	instantiates(alias_2_test_metaclass),
	specializes(alias_2_test_class)).

	:- alias(alias_2_test_class, [
		p/1 as q/1
	]).

:- end_object.


:- object(alias_2_test_instance,
	instantiates(alias_2_test_subclass)).

	:- alias(alias_2_test_subclass, [
		p/1 as r/1
	]).

:- end_object.


% unit tests

:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/04/29,
		comment is 'Unit tests for the alias/2 built-in directive.'
	]).

	% test all possible syntaxes for the directive
	:- alias(lgtunit, [run/0 as run_alias1/0]).
	:- alias(lgtunit, [run/0 :: run_alias2/0]).

	% tests for predicate aliases defined in prototypes

	test(alias_2_1) :-
		predicate_property(run_alias1, alias_of(run)),
		predicate_property(run_alias1, declared_in(lgtunit)),
		predicate_property(run_alias1, defined_in(lgtunit)).

	test(alias_2_2) :-
		predicate_property(run_alias2, alias_of(run)),
		predicate_property(run_alias2, declared_in(lgtunit)),
		predicate_property(run_alias2, defined_in(lgtunit)).

	% tests for predicate aliases defined in instances and classes

	test(alias_2_3) :-
		alias_2_test_instance::predicate_property(q(_), alias_of(p(_))),
		alias_2_test_instance::predicate_property(q(_), declared_in(alias_2_test_class)),
		alias_2_test_instance::predicate_property(q(_), defined_in(alias_2_test_class)).

	test(alias_2_4) :-
		alias_2_test_instance::predicate_property(r(_), alias_of(p(_))),
		alias_2_test_instance::predicate_property(r(_), declared_in(alias_2_test_class)),
		alias_2_test_instance::predicate_property(r(_), defined_in(alias_2_test_class)).

:- end_object.
