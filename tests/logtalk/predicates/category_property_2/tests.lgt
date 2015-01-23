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
		date is 2015/01/23,
		comment is 'Unit tests for the category_property/2 built-in predicate.'
	]).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

	throws(category_property_2_1, error(type_error(category_identifier, 1), logtalk(category_property(1, static), _))) :-
		category_property(1, static).

	throws(category_property_2_2, error(type_error(callable, 1), logtalk(category_property(monitoring, 1), _))) :-
		category_property(monitoring, 1).

	throws(category_property_2_3, error(domain_error(category_property, foo), logtalk(category_property(monitoring, foo), _))) :-
		category_property(monitoring, foo).

	fails(category_property_2_4) :-
		category_property(non_exisiting_category, _).

	fails(category_property_2_5) :-
		category_property(monitoring, (dynamic)).

	succeeds(category_property_2_6) :-
		findall(Prop, category_property(monitoring, Prop), _).

	% entity info
	succeeds(category_property_2_7) :-
		category_property(test_category, static),
		category_property(test_category, source_data),
		category_property(test_category, file(Basename, Directory)), ground(Basename), ground(Directory),
		category_property(test_category, lines(Start, End)), integer(Start), integer(End),
		category_property(test_category, number_of_clauses(NC)), NC == 7,
		category_property(test_category, number_of_user_clauses(NUC)), NUC == 7,
		category_property(test_category, info(Info)),
		member(version(_), Info),
		member(author(_), Info),
		member(date(_), Info),
		member(comment(_), Info).

	% entity interface
	succeeds(category_property_2_8) :-
		category_property(test_category, public(Public)), Public == [a/1],
		category_property(test_category, protected(Protected)), Protected == [b/2],
		category_property(test_category, private(Private)), Private == [c/3, e/5].

	% interface predicate declaration properties
	succeeds(category_property_2_9) :-
		category_property(test_category, declares(a/1, Properties1)),
		member((public), Properties1),
		member(scope(Scope1), Properties1), Scope1 == (public),
		member(static, Properties1),
		(	current_logtalk_flag(coinduction, supported) ->
			member(coinductive(Template), Properties1), Template == a((+))
		;	true
		),
		member(line_count(LC1), Properties1), integer(LC1),
		category_property(test_category, declares(b/2, Properties2)),
		member(protected, Properties2),
		member(scope(Scope2), Properties2), Scope2 == protected,
		member(static, Properties2),
		(	current_logtalk_flag(threads, supported) ->
			member(synchronized, Properties2)
		;	true
		),
		member(line_count(LC2), Properties2), integer(LC2),
		category_property(test_category, declares(c/3, Properties3)),
		member(private, Properties3),
		member(scope(Scope3), Properties3), Scope3 == private,
		member((dynamic), Properties3),
		member(line_count(LC3), Properties3), integer(LC3),
		\+ category_property(test_category, declares(d/4, _Properties4)),
		category_property(test_category, declares(e/5, Properties5)),
		member(private, Properties5),
		member(scope(Scope3), Properties5), Scope3 == private,
		member(static, Properties5),
		member(line_count(LC5), Properties5), integer(LC5).

	% interface predicate definition properties
	succeeds(category_property_2_10) :-
		category_property(test_category, defines(a/1, Properties1)),
		member(line_count(LC1), Properties1), integer(LC1),
		member(number_of_clauses(NC1), Properties1), NC1 == 1,
		category_property(test_category, defines(b/2, Properties2)),
		member(line_count(LC2), Properties2), integer(LC2),
		member(number_of_clauses(NC2), Properties2), NC2 == 2,
		\+ category_property(test_category, defines(c/3, _)),
		category_property(test_category, defines(d/4, Properties4)),
		member(line_count(LC4), Properties4), integer(LC4),
		member(number_of_clauses(NC4), Properties4), NC4 == 4,
		category_property(test_category, defines(e/5, Properties5)),
		\+ member(line_count(_LC5), Properties5),
		member(number_of_clauses(NC5), Properties5), NC5 == 0.		

	member(H, [H| _]) :-
		!.
	member(H, [_| T]) :-
		member(H, T).

:- end_object.
