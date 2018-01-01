%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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
		comment is 'Unit tests for the object_property/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	throws(object_property_2_01, error(type_error(object_identifier, 1), logtalk(object_property(1, static), _))) :-
		object_property(1, static).

	throws(object_property_2_02, error(type_error(callable, 1), logtalk(object_property(logtalk, 1), _))) :-
		object_property(logtalk, 1).

	throws(object_property_2_03, error(domain_error(object_property, foo), logtalk(object_property(logtalk, foo), _))) :-
		object_property(logtalk, foo).

	fails(object_property_2_04) :-
		object_property(non_exisiting_object, _).

	fails(object_property_2_05) :-
		object_property(logtalk, (dynamic)).

	succeeds(object_property_2_06) :-
		findall(Prop, object_property(logtalk, Prop), _).

	% entity info

	succeeds(object_property_2_07) :-
		object_property(test_object, static),
		object_property(test_object, dynamic_declarations),
		object_property(test_object, complements(allow)),
		object_property(test_object, complements),
		object_property(test_object, events),
		object_property(test_object, source_data),
		object_property(test_object, file(Basename, Directory)), ground(Basename), ground(Directory),
		object_property(test_object, lines(Start, End)), integer(Start), integer(End),
		object_property(test_object, number_of_clauses(NC)), NC == 36,
		object_property(test_object, number_of_rules(NR)), NR == 25,
		object_property(test_object, number_of_user_clauses(NUC)), NUC == 34,
		object_property(test_object, number_of_user_rules(NUR)), NUR == 23,
		object_property(test_object, info(Info)),
		member(version(_), Info),
		member(author(_), Info),
		member(date(_), Info),
		member(comment(_), Info).

	% entity interface

	succeeds(object_property_2_08) :-
		object_property(test_object, public(Public)), Public == [a/1],
		object_property(test_object, protected(Protected)), Protected == [b/2],
		object_property(test_object, private(Private)), Private == [c/3, e/5].

	% predicate declaration properties

	succeeds(object_property_2_09) :-
		object_property(test_object, declares(a/1, Properties1)),
		member((public), Properties1),
		member(scope(Scope1), Properties1), Scope1 == (public),
		member(static, Properties1),
		(	current_logtalk_flag(coinduction, supported) ->
			member(coinductive(Template), Properties1), Template == a((+))
		;	true
		),
		member(line_count(LC1), Properties1), integer(LC1).

	succeeds(object_property_2_10) :-
		object_property(test_object, declares(b/2, Properties2)),
		member(protected, Properties2),
		member(scope(Scope2), Properties2), Scope2 == protected,
		member(static, Properties2),
		(	current_logtalk_flag(threads, supported) ->
			member(synchronized, Properties2)
		;	true
		),
		member(line_count(LC2), Properties2), integer(LC2).

	succeeds(object_property_2_11) :-
		object_property(test_object, declares(c/3, Properties3)),
		member(private, Properties3),
		member(scope(Scope3), Properties3), Scope3 == private,
		member((dynamic), Properties3),
		member(line_count(LC3), Properties3), integer(LC3),
		\+ object_property(test_object, declares(d/4, _Properties4)).

	succeeds(object_property_2_12) :-
		object_property(test_object, declares(e/5, Properties5)),
		member(private, Properties5),
		member(scope(Scope3), Properties5), Scope3 == private,
		member(static, Properties5),
		member(line_count(LC5), Properties5), integer(LC5).

	% predicate definition properties

	succeeds(object_property_2_13) :-
		object_property(test_object, defines(a/1, Properties1)),
		member(line_count(LC1), Properties1), integer(LC1),
		member(number_of_clauses(NC1), Properties1), NC1 == 1,
		member(number_of_rules(NR1), Properties1), NR1 == 0.

	succeeds(object_property_2_14) :-
		object_property(test_object, defines(b/2, Properties2)),
		member(line_count(LC2), Properties2), integer(LC2),
		member(number_of_clauses(NC2), Properties2), NC2 == 2,
		member(number_of_rules(NR2), Properties2), NR2 == 0.

	succeeds(object_property_2_15) :-
		object_property(test_object, defines(c/3, Properties3)),
		member(line_count(LC3), Properties3), integer(LC3),
		member(number_of_clauses(NC3), Properties3), NC3 == 3,
		member(number_of_rules(NR3), Properties3), NR3 == 0.

	succeeds(object_property_2_16) :-
		object_property(test_object, defines(d/4, Properties4)),
		member(line_count(LC4), Properties4), integer(LC4),
		member(number_of_clauses(NC4), Properties4), NC4 == 4,
		member(number_of_rules(NR4), Properties4), NR4 == 0.

	succeeds(object_property_2_17) :-
		object_property(test_object, defines(e/5, Properties5)),
		\+ member(line_count(_LC5), Properties5),
		member(number_of_clauses(NC5), Properties5), NC5 == 0,	
		member(number_of_rules(NR5), Properties5), NR5 == 0.		

	% predicate call properties

	succeeds(object_property_2_18) :-
		object_property(test_object, calls((local)/0, Properties1)),
		member(caller(Caller1), Properties1), Caller1 == caller1/0,
		member(line_count(Line1), Properties1), integer(Line1).

	succeeds(object_property_2_19) :-
		object_property(test_object, calls(logtalk::expand_library_path/2, Properties2)),
		member(caller(Caller2), Properties2), Caller2 == caller2/0,
		member(line_count(Line2), Properties2), integer(Line2).

	succeeds(object_property_2_20) :-
		object_property(test_object, calls(logtalk::message_tokens/4, Properties3)),
		member(caller(Caller3), Properties3), Caller3 == caller3/0,
		member(line_count(Line3), Properties3), integer(Line3),
		member(non_terminal(NonTerminal3), Properties3), NonTerminal3 == message_tokens//2.

	succeeds(object_property_2_21) :-
		object_property(test_object, calls(logtalk::loaded_file/1, Properties4)),
		member(caller(Caller4), Properties4), Caller4 == caller4/0,
		member(line_count(Line4), Properties4), integer(Line4),
		member(alias(Alias4), Properties4), Alias4 == loaded/1.

	succeeds(object_property_2_22) :-
		object_property(test_object, calls(logtalk::message_tokens/4, Properties5)),
		member(caller(Caller5), Properties5), Caller5 == caller5/0,
		member(line_count(Line5), Properties5), integer(Line5),
		member(non_terminal(NonTerminal5), Properties5), NonTerminal5 == message_tokens//2,
		member(alias(Alias5), Properties5), Alias5 == tokens/4.

	% predicate updates properties

	succeeds(object_property_2_23) :-
		setof(
			Updater,
			Properties^(
				object_property(test_object, updates(c/3, Properties)),
				member(updater(Updater), Properties)
			),
			Updaters
		),
		Updaters == [updater1/0, updater2/0, updater3/0, updater4/0, updater5/0, updater6/0].
		
	succeeds(object_property_2_24) :-
		forall(
			object_property(test_object, updates(c/3, Properties)),
			(member(line_count(Line), Properties), integer(Line))
		).

	succeeds(object_property_2_25) :-
		setof(
			Updater,
			Properties^(
				object_property(test_object, updates(::c/3, Properties)),
				member(updater(Updater), Properties)
			),
			Updaters
		),
		Updaters == [updater1s/0, updater2s/0, updater3s/0, updater4s/0, updater5s/0, updater6s/0].
		
	succeeds(object_property_2_26) :-
		forall(
			object_property(test_object, updates(::c/3, Properties)),
			(member(line_count(Line), Properties), integer(Line))
		).

	succeeds(object_property_2_27) :-
		setof(
			Updater,
			Properties^(
				object_property(test_object, updates(logtalk::c/3, Properties)),
				member(updater(Updater), Properties)
			),
			Updaters
		),
		Updaters == [updater1o/0, updater2o/0, updater3o/0, updater4o/0, updater5o/0, updater6o/0].
		
	succeeds(object_property_2_28) :-
		forall(
			object_property(test_object, updates(logtalk::c/3, Properties)),
			(member(line_count(Line), Properties), integer(Line))
		).

	% check that all queries with explicit properties are valid

	fails(object_property_2_29) :-
		object_property(empty_object, built_in),
		fail.

	fails(object_property_2_30) :-
		object_property(empty_object, (dynamic)),
		fail.

	fails(object_property_2_31) :-
		object_property(empty_object, static),
		fail.

	fails(object_property_2_32) :-
		object_property(empty_object, debugging),
		fail.

	fails(object_property_2_33) :-
		object_property(empty_object, public(_)),
		fail.

	fails(object_property_2_34) :-
		object_property(empty_object, protected(_)),
		fail.

	fails(object_property_2_35) :-
		object_property(empty_object, private(_)),
		fail.

	fails(object_property_2_36) :-
		object_property(empty_object, declares(_, _)),
		fail.

	fails(object_property_2_37) :-
		object_property(empty_object, alias(_, _)),
		fail.

	fails(object_property_2_38) :-
		object_property(empty_object, source_data),
		fail.

	fails(object_property_2_39) :-
		object_property(empty_object, info(_)),
		fail.

	fails(object_property_2_40) :-
		object_property(empty_object, file(_)),
		fail.

	fails(object_property_2_41) :-
		object_property(empty_object, file(_, _)),
		fail.

	fails(object_property_2_42) :-
		object_property(empty_object, lines(_, _)),
		fail.

	fails(object_property_2_43) :-
		object_property(empty_object, events),
		fail.

	fails(object_property_2_44) :-
		object_property(empty_object, defines(_, _)),
		fail.

	fails(object_property_2_45) :-
		object_property(empty_object, includes(_, _, _)),
		fail.

	fails(object_property_2_46) :-
		object_property(empty_object, provides(_, _, _)),
		fail.

	fails(object_property_2_47) :-
		object_property(empty_object, calls(_, _)),
		fail.

	fails(object_property_2_48) :-
		object_property(empty_object, number_of_clauses(_)),
		fail.

	fails(object_property_2_49) :-
		object_property(empty_object, number_of_rules(_)),
		fail.

	fails(object_property_2_50) :-
		object_property(empty_object, number_of_user_clauses(_)),
		fail.

	fails(object_property_2_51) :-
		object_property(empty_object, number_of_user_rules(_)),
		fail.

	fails(object_property_2_52) :-
		object_property(empty_object, threaded),
		fail.

	fails(object_property_2_53) :-
		object_property(empty_object, context_switching_calls),
		fail.

	fails(object_property_2_54) :-
		object_property(empty_object, dynamic_declarations),
		fail.

	fails(object_property_2_55) :-
		object_property(empty_object, complements),
		fail.

	fails(object_property_2_56) :-
		object_property(empty_object, complements(_)),
		fail.

	% determinism tests

	deterministic(object_property_2_57) :-
		object_property(debug_object, debugging).

	deterministic(object_property_2_58) :-
		object_property(test_object, source_data).

	deterministic(object_property_2_59) :-
		object_property(dynamic_object, (dynamic)).

	deterministic(object_property_2_60) :-
		object_property(test_object, static).

	deterministic(object_property_2_61) :-
		object_property(built_in_object, built_in).

	deterministic(object_property_2_62) :-
		object_property(test_object, file(_)).

	deterministic(object_property_2_63) :-
		object_property(test_object, file(_, _)).

	deterministic(object_property_2_64) :-
		object_property(test_object, lines(_, _)).

	deterministic(object_property_2_65) :-
		object_property(test_object, info(_)).

	deterministic(object_property_2_66) :-
		object_property(options_object, events).

	deterministic(object_property_2_67) :-
		object_property(test_object, number_of_clauses(_)).

	deterministic(object_property_2_68) :-
		object_property(test_object, number_of_rules(_)).

	deterministic(object_property_2_69) :-
		object_property(test_object, number_of_user_clauses(_)).

	deterministic(object_property_2_70) :-
		object_property(test_object, number_of_user_rules(_)).

	deterministic(object_property_2_71) :-
		object_property(options_object, context_switching_calls).

	deterministic(object_property_2_72) :-
		object_property(options_object, dynamic_declarations).

	deterministic(object_property_2_73) :-
		object_property(options_object, complements(_)).

	deterministic(object_property_2_74) :-
		object_property(options_object, complements).

	:- if(current_logtalk_flag(threads, supported)).
		deterministic(object_property_2_75) :-
			object_property(threaded_object, threaded).
	:- else.
		- deterministic(object_property_2_75).
	:- endif.

	% auxiliary predicates (avoid library dependencies)

	member(H, [H| _]) :-
		!.
	member(H, [_| T]) :-
		member(H, T).

:- end_object.
