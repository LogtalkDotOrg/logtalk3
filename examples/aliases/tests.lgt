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
		version is 1.6,
		author is 'Parker Jones and Paulo Moura',
		date is 2015/01/20,
		comment is 'Unit tests for the "aliases" example.'
	]).

	cover(rectangle(_, _)).
	cover(square(_)).
	cover(ellipse(_, _)).
	cover(circle(_)).

	test(aliases_01) :-
		findall(Predicate, square(_)::current_predicate(Predicate), PredicatesUnsorted),
		list::msort(PredicatesUnsorted, PredicatesSorted),
		PredicatesSorted == [area/1, height/1, side/1, width/1].

	test(aliases_02) :-
		square(2)::side(Side),
		Side == 2.

	test(aliases_03) :-
		findall(Property, square(_)::predicate_property(side(_), Property), AllProperties),
		list::msort(AllProperties, AllPropertiesSorted),
		list::msort([alias_of(width(_)), alias_declared_in(square(_),_), logtalk, public, static, declared_in(rectangle(_,_)), defined_in(rectangle(_,_)), scope(public), number_of_clauses(1)], PropertiesSorted),
		list::subsequence(AllPropertiesSorted, PropertiesSorted, _).

	test(aliases_04) :-
		findall(Property, square(_)::predicate_property(width(_), Property), AllProperties),
		list::msort(AllProperties, AllPropertiesSorted),
		list::msort([logtalk, public, static, declared_in(rectangle(_,_)), defined_in(rectangle(_,_)), scope(public)], PropertiesSorted),
		list::subsequence(AllPropertiesSorted, PropertiesSorted, _).

	test(aliases_05) :-
		findall(Predicate, circle(_)::current_predicate(Predicate), PredicatesUnsorted),
		list::msort(PredicatesUnsorted, PredicatesSorted),
		PredicatesSorted == [area/1, r/1, rx/1, ry/1].

	test(aliases_06) :-
		circle(3)::r(Radius),
		Radius == 3.

	test(aliases_07) :-
		findall(Property, circle(3)::predicate_property(r(_), Property), AllProperties),
		list::msort(AllProperties, AllPropertiesSorted),
		list::msort([alias_of(rx(_)), logtalk, public, static, declared_in(ellipse(_, _)), defined_in(ellipse(_, _)), scope(public)], PropertiesSorted),
		list::subsequence(AllPropertiesSorted, PropertiesSorted, _).

	test(aliases_08) :-
		findall(Property, circle(3)::predicate_property(rx(_), Property), AllProperties),
		list::msort(AllProperties, AllPropertiesSorted),
		list::msort([logtalk, public, static, declared_in(ellipse(_, _)), defined_in(ellipse(_, _)), scope(public)], PropertiesSorted),
		list::subsequence(AllPropertiesSorted, PropertiesSorted, _).

	test(aliases_09) :-
		object_property(square(_), alias(side(_), AllProperties)),
		list::msort(AllProperties, AllPropertiesSorted),
		list::msort([for(width(_)), from(rectangle(_,_)), line_count(_)], PropertiesSorted),
		list::subsequence(AllPropertiesSorted, PropertiesSorted, _).

	test(aliases_10) :-
		object_property(circle(_), alias(r(_), AllProperties)),
		list::msort(AllProperties, AllPropertiesSorted),
		list::msort([for(rx(_)), from(ellipse(_,_)), line_count(_)], PropertiesSorted),
		list::subsequence(AllPropertiesSorted, PropertiesSorted, _).

:- end_object.
