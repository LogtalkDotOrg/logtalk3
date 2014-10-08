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
		version is 1.4,
		author is 'Parker Jones and Paulo Moura',
		date is 2014/10/08,
		comment is 'Unit tests for the "complements/allow" example.'
	]).

	cover(employee).
	cover(add_on).

	test(complements_allow_1) :-
		complements_object(Category, Object),
		Category == add_on, Object == employee.

	test(complements_allow_2) :-
		conforms_to_protocol(employee, Protocol),
		Protocol == monitoring.

	test(complements_allow_3) :-
		conforms_to_protocol(employee, Protocol, Scope),
		Protocol == monitoring,
		Scope == (public).

	test(complements_allow_4) :-
		employee::name(Name),
		Name == john.

	test(complements_allow_5) :-
		employee::predicates(Predicates),
		list::msort(Predicates, PredicatesSorted),
		PredicatesSorted = [after/3, age/1, before/3, income/1, name/1, predicates/1, salary/1].

	test(complements_allow_6) :-
		findall(Property, employee::predicate_property(predicates(_), Property), AllProperties),
		list::msort(AllProperties, AllPropertiesSorted),
		list::msort([logtalk, public, static, declared_in(add_on), defined_in(add_on), scope(public)], PropertiesSorted),
		list::subsequence(AllPropertiesSorted, PropertiesSorted, _).

	test(complements_allow_7) :-
		findall(Property, employee::predicate_property(income(_), Property), AllProperties),
		list::msort(AllProperties, AllPropertiesSorted),
		list::msort([logtalk, public, static, alias_of(salary(_)), declared_in(employee), defined_in(employee), scope(public)], PropertiesSorted),
		list::subsequence(AllPropertiesSorted, PropertiesSorted, _).

:- end_object.
