%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 2.4,
		author is 'Paulo Moura',
		date is 2017/01/06,
		comment is 'Unit tests for the category_property/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

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
		category_property(test_category, number_of_clauses(NC)), NC == 33,
		category_property(test_category, number_of_rules(NR)), NR == 25,
		category_property(test_category, number_of_user_clauses(NUC)), NUC == 31,
		category_property(test_category, number_of_user_rules(NUR)), NUR == 23,
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

	% predicate declaration properties

	succeeds(category_property_2_09) :-
		category_property(test_category, declares(a/1, Properties1)),
		member((public), Properties1),
		member(scope(Scope1), Properties1), Scope1 == (public),
		member(static, Properties1),
		(	current_logtalk_flag(coinduction, supported) ->
			member(coinductive(Template), Properties1), Template == a((+))
		;	true
		),
		member(line_count(LC1), Properties1), integer(LC1).

	succeeds(category_property_2_10) :-
		category_property(test_category, declares(b/2, Properties2)),
		member(protected, Properties2),
		member(scope(Scope2), Properties2), Scope2 == protected,
		member(static, Properties2),
		(	current_logtalk_flag(threads, supported) ->
			member(synchronized, Properties2)
		;	true
		),
		member(line_count(LC2), Properties2), integer(LC2).

	succeeds(category_property_2_11) :-
		category_property(test_category, declares(c/3, Properties3)),
		member(private, Properties3),
		member(scope(Scope3), Properties3), Scope3 == private,
		member((dynamic), Properties3),
		member(line_count(LC3), Properties3), integer(LC3),
		\+ category_property(test_category, declares(d/4, _Properties4)).

	succeeds(category_property_2_12) :-
		category_property(test_category, declares(e/5, Properties5)),
		member(private, Properties5),
		member(scope(Scope3), Properties5), Scope3 == private,
		member(static, Properties5),
		member(line_count(LC5), Properties5), integer(LC5).

	% predicate definition properties

	succeeds(category_property_2_13) :-
		category_property(test_category, defines(a/1, Properties1)),
		member(line_count(LC1), Properties1), integer(LC1),
		member(number_of_clauses(NC1), Properties1), NC1 == 1,
		member(number_of_rules(NR1), Properties1), NR1 == 0.

	succeeds(category_property_2_14) :-
		category_property(test_category, defines(b/2, Properties2)),
		member(line_count(LC2), Properties2), integer(LC2),
		member(number_of_clauses(NC2), Properties2), NC2 == 2,
		member(number_of_rules(NR2), Properties2), NR2 == 0.

	succeeds(category_property_2_15) :-
		\+ category_property(test_category, defines(c/3, _)).

	succeeds(category_property_2_16) :-
		category_property(test_category, defines(d/4, Properties4)),
		member(line_count(LC4), Properties4), integer(LC4),
		member(number_of_clauses(NC4), Properties4), NC4 == 4,
		member(number_of_rules(NR4), Properties4), NR4 == 0.

	succeeds(category_property_2_17) :-
		category_property(test_category, defines(e/5, Properties5)),
		\+ member(line_count(_LC5), Properties5),
		member(number_of_clauses(NC5), Properties5), NC5 == 0,
		member(number_of_rules(NR5), Properties5), NR5 == 0.		

	% predicate call properties

	succeeds(category_property_2_18) :-
		category_property(test_category, calls((local)/0, Properties1)),
		member(caller(Caller1), Properties1), Caller1 == caller1/0,
		member(line_count(Line1), Properties1), integer(Line1).

	succeeds(category_property_2_19) :-
		category_property(test_category, calls(logtalk::expand_library_path/2, Properties2)),
		member(caller(Caller2), Properties2), Caller2 == caller2/0,
		member(line_count(Line2), Properties2), integer(Line2).

	succeeds(category_property_2_20) :-
		category_property(test_category, calls(logtalk::message_tokens/4, Properties3)),
		member(caller(Caller3), Properties3), Caller3 == caller3/0,
		member(line_count(Line3), Properties3), integer(Line3),
		member(non_terminal(NonTerminal3), Properties3), NonTerminal3 == message_tokens//2.

	succeeds(category_property_2_21) :-
		category_property(test_category, calls(logtalk::loaded_file/1, Properties4)),
		member(caller(Caller4), Properties4), Caller4 == caller4/0,
		member(line_count(Line4), Properties4), integer(Line4),
		member(alias(Alias4), Properties4), Alias4 == loaded/1.

	succeeds(category_property_2_22) :-
		category_property(test_category, calls(logtalk::message_tokens/4, Properties5)),
		member(caller(Caller5), Properties5), Caller5 == caller5/0,
		member(line_count(Line5), Properties5), integer(Line5),
		member(non_terminal(NonTerminal5), Properties5), NonTerminal5 == message_tokens//2,
		member(alias(Alias5), Properties5), Alias5 == tokens/4.

	% predicate updates properties

	succeeds(category_property_2_23) :-
		setof(
			Updater,
			Properties^(
				category_property(test_category, updates(c/3, Properties)),
				member(updater(Updater), Properties)
			),
			Updaters
		),
		Updaters == [updater1/0, updater2/0, updater3/0, updater4/0, updater5/0, updater6/0].
		
	succeeds(category_property_2_24) :-
		forall(
			category_property(test_category, updates(c/3, Properties)),
			(member(line_count(Line), Properties), integer(Line))
		).

	succeeds(object_property_2_25) :-
		setof(
			Updater,
			Properties^(
				category_property(test_category, updates(::c/3, Properties)),
				member(updater(Updater), Properties)
			),
			Updaters
		),
		Updaters == [updater1s/0, updater2s/0, updater3s/0, updater4s/0, updater5s/0, updater6s/0].
		
	succeeds(object_property_2_26) :-
		forall(
			category_property(test_category, updates(::c/3, Properties)),
			(member(line_count(Line), Properties), integer(Line))
		).

	succeeds(object_property_2_27) :-
		setof(
			Updater,
			Properties^(
				category_property(test_category, updates(logtalk::c/3, Properties)),
				member(updater(Updater), Properties)
			),
			Updaters
		),
		Updaters == [updater1o/0, updater2o/0, updater3o/0, updater4o/0, updater5o/0, updater6o/0].
		
	succeeds(object_property_2_28) :-
		forall(
			category_property(test_object, updates(logtalk::c/3, Properties)),
			(member(line_count(Line), Properties), integer(Line))
		).

	% check that all queries with explicit properties are valid

	fails(category_property_2_29) :-
		category_property(empty_category, built_in),
		fail.

	fails(category_property_2_30) :-
		category_property(empty_category, (dynamic)),
		fail.

	fails(category_property_2_31) :-
		category_property(empty_category, static),
		fail.

	fails(category_property_2_32) :-
		category_property(empty_category, debugging),
		fail.

	fails(category_property_2_33) :-
		category_property(empty_category, public(_)),
		fail.

	fails(category_property_2_34) :-
		category_property(empty_category, protected(_)),
		fail.

	fails(category_property_2_35) :-
		category_property(empty_category, private(_)),
		fail.

	fails(category_property_2_36) :-
		category_property(empty_category, declares(_, _)),
		fail.

	fails(category_property_2_37) :-
		category_property(empty_category, alias(_, _)),
		fail.

	fails(category_property_2_38) :-
		category_property(empty_category, source_data),
		fail.

	fails(category_property_2_39) :-
		category_property(empty_category, info(_)),
		fail.

	fails(category_property_2_40) :-
		category_property(empty_category, file(_)),
		fail.

	fails(category_property_2_41) :-
		category_property(empty_category, file(_, _)),
		fail.

	fails(category_property_2_42) :-
		category_property(empty_category, lines(_, _)),
		fail.

	fails(category_property_2_43) :-
		category_property(empty_category, events),
		fail.

	fails(category_property_2_44) :-
		category_property(empty_category, defines(_, _)),
		fail.

	fails(category_property_2_45) :-
		category_property(empty_category, includes(_, _, _)),
		fail.

	fails(category_property_2_46) :-
		category_property(empty_category, provides(_, _, _)),
		fail.

	fails(category_property_2_47) :-
		category_property(empty_category, calls(_, _)),
		fail.

	fails(category_property_2_48) :-
		category_property(empty_category, number_of_clauses(_)),
		fail.

	fails(category_property_2_49) :-
		category_property(empty_category, number_of_rules(_)),
		fail.

	fails(category_property_2_50) :-
		category_property(empty_category, number_of_user_clauses(_)),
		fail.

	fails(category_property_2_51) :-
		category_property(empty_category, number_of_user_rules(_)),
		fail.

	% determinism tests

	deterministic(category_property_2_52) :-
		category_property(debug_category, debugging).

	deterministic(category_property_2_53) :-
		category_property(test_category, source_data).

	deterministic(category_property_2_54) :-
		category_property(dynamic_category, (dynamic)).

	deterministic(category_property_2_55) :-
		category_property(test_category, static).

	deterministic(category_property_2_56) :-
		category_property(built_in_category, built_in).

	deterministic(category_property_2_57) :-
		category_property(test_category, file(_)).

	deterministic(category_property_2_58) :-
		category_property(test_category, file(_, _)).

	deterministic(category_property_2_59) :-
		category_property(test_category, lines(_, _)).

	deterministic(category_property_2_60) :-
		category_property(test_category, info(_)).

	deterministic(category_property_2_61) :-
		category_property(events_category, events).

	deterministic(category_property_2_62) :-
		category_property(test_category, number_of_clauses(_)).

	deterministic(category_property_2_63) :-
		category_property(test_category, number_of_rules(_)).

	deterministic(category_property_2_64) :-
		category_property(test_category, number_of_user_clauses(_)).

	deterministic(category_property_2_65) :-
		category_property(test_category, number_of_user_rules(_)).

	% auxiliary predicates (avoid library dependencies)

	member(H, [H| _]) :-
		!.
	member(H, [_| T]) :-
		member(H, T).

:- end_object.
