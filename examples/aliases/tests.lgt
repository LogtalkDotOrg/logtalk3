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
		date is 2014/02/04,
		comment is 'Unit tests for the "aliases" example.'
	]).

	cover(rectangle(_, _)).
	cover(square(_)).
	cover(ellipse(_, _)).
	cover(circle(_)).

	test(aliases_1) :-
		findall(Predicate, square(_)::current_predicate(Predicate), PredicatesUnsorted),
		list::msort(PredicatesUnsorted, PredicatesSorted),
		PredicatesSorted == [area/1, height/1, side/1, width/1].

	test(aliases_2) :-
		square(2)::side(Side),
		Side == 2.

	test(aliases_3) :-
		findall(Property, square(2)::predicate_property(side(_), Property), PropertiesUnsorted),
		list::msort(PropertiesUnsorted, PropertiesSorted),
		PropertiesSorted = [logtalk, public, static, alias_of(width(_)), declared_in(rectangle(_, _)), defined_in(rectangle(_, _)), number_of_clauses(_), scope(public)].

	test(aliases_4) :-
		findall(Property, square(2)::predicate_property(width(_), Property), AllProperties),
		list::msort(AllProperties, AllPropertiesSorted),
		list::msort([logtalk, public, static, declared_in(rectangle(_, _)), defined_in(rectangle(_, _)), scope(public)], PropertiesSorted),
		list::subsequence(AllPropertiesSorted, PropertiesSorted, _).

	test(aliases_5) :-
		findall(Predicate, circle(_)::current_predicate(Predicate), PredicatesUnsorted),
		list::msort(PredicatesUnsorted, PredicatesSorted),
		PredicatesSorted == [area/1, r/1, rx/1, ry/1].

	test(aliases_6) :-
		circle(3)::r(Radius),
		Radius == 3.

	test(aliases_7) :-
		findall(Property, circle(3)::predicate_property(r(_), Property), PropertiesUnsorted),
		list::msort(PropertiesUnsorted, PropertiesSorted),
		PropertiesSorted = [logtalk, public, static, alias_of(rx(_)), declared_in(ellipse(_, _)), defined_in(ellipse(_, _)), number_of_clauses(_), scope(public)].

	test(aliases_8) :-
		findall(Property, circle(3)::predicate_property(rx(_), Property), AllProperties),
		list::msort(AllProperties, AllPropertiesSorted),
		list::msort([logtalk, public, static, declared_in(ellipse(_, _)), defined_in(ellipse(_, _)), scope(public)], PropertiesSorted),
		list::subsequence(AllPropertiesSorted, PropertiesSorted, _).

:- end_object.
