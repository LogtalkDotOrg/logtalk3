%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2016/03/20,
		comment is 'Unit tests for the category_property/2 built-in predicate.'
	]).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

	throws(category_property_2_01, error(type_error(category_identifier, 1), logtalk(category_property(1, static), _))) :-
		category_property(1, static).

	throws(category_property_2_02, error(type_error(callable, 1), logtalk(category_property(monitoring, 1), _))) :-
		category_property(monitoring, 1).

	throws(category_property_2_03, error(domain_error(category_property, foo), logtalk(category_property(monitoring, foo), _))) :-
		category_property(monitoring, foo).

	fails(category_property_2_04) :-
		category_property(non_exisiting_category, _).

	fails(category_property_2_05) :-
		category_property(monitoring, (dynamic)).

	succeeds(category_property_2_06) :-
		findall(Prop, category_property(monitoring, Prop), _).

	% entity info
	succeeds(category_property_2_07) :-
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
	succeeds(category_property_2_08) :-
		category_property(test_category, public(Public)), Public == [a/1],
		category_property(test_category, protected(Protected)), Protected == [b/2],
		category_property(test_category, private(Private)), Private == [c/3, e/5].

	% interface predicate declaration properties
	succeeds(category_property_2_09) :-
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

	% check that all queries with explicit properties are valid
	fails(category_property_2_11) :-
		(	category_property(empty_category, built_in)
		;	category_property(empty_category, (dynamic))
		;	category_property(empty_category, static)
		;	category_property(empty_category, debugging)
		;	category_property(empty_category, public(_))
		;	category_property(empty_category, protected(_))
		;	category_property(empty_category, private(_))
		;	category_property(empty_category, declares(_, _))
		;	category_property(empty_category, alias(_, _))
		;	category_property(empty_category, source_data)
		;	category_property(empty_category, info(_))
		;	category_property(empty_category, file(_))
		;	category_property(empty_category, file(_, _))
		;	category_property(empty_category, lines(_, _))
		;	category_property(empty_category, events)
		;	category_property(empty_category, defines(_, _))
		;	category_property(empty_category, includes(_, _, _))
		;	category_property(empty_category, provides(_, _, _))
		;	category_property(empty_category, calls(_, _))
		;	category_property(empty_category, number_of_clauses(_))
		;	category_property(empty_category, number_of_user_clauses(_))
		),
		% force backtracking into all property queries
		fail.

	% determinism tests

	deterministic(category_property_2_12) :-
		category_property(debug_category, debugging).

	deterministic(category_property_2_13) :-
		category_property(test_category, source_data).

	deterministic(category_property_2_14) :-
		category_property(dynamic_category, (dynamic)).

	deterministic(category_property_2_15) :-
		category_property(test_category, static).

	deterministic(category_property_2_16) :-
		category_property(built_in_category, built_in).

	deterministic(category_property_2_17) :-
		category_property(test_category, file(_)).

	deterministic(category_property_2_18) :-
		category_property(test_category, file(_, _)).

	deterministic(category_property_2_19) :-
		category_property(test_category, lines(_, _)).

	deterministic(category_property_2_20) :-
		category_property(test_category, info(_)).

	deterministic(category_property_2_21) :-
		category_property(events_category, events).

	deterministic(category_property_2_22) :-
		category_property(test_category, number_of_clauses(_)).

	deterministic(category_property_2_23) :-
		category_property(test_category, number_of_user_clauses(_)).

	% auxiliary predicates (avoid library dependencies)

	member(H, [H| _]) :-
		!.
	member(H, [_| T]) :-
		member(H, T).

:- end_object.
