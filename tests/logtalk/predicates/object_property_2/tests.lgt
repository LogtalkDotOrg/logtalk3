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
		comment is 'Unit tests for the object_property/2 built-in predicate.'
	]).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

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
		object_property(test_object, number_of_clauses(N)), N == 10,
		object_property(test_object, number_of_user_clauses(NUC)), NUC == 10,
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

	% interface predicate declaration properties
	succeeds(object_property_2_09) :-
		object_property(test_object, declares(a/1, Properties1)),
		member((public), Properties1),
		member(scope(Scope1), Properties1), Scope1 == (public),
		member(static, Properties1),
		(	current_logtalk_flag(coinduction, supported) ->
			member(coinductive(Template), Properties1), Template == a((+))
		;	true
		),
		member(line_count(LC1), Properties1), integer(LC1),
		object_property(test_object, declares(b/2, Properties2)),
		member(protected, Properties2),
		member(scope(Scope2), Properties2), Scope2 == protected,
		member(static, Properties2),
		(	current_logtalk_flag(threads, supported) ->
			member(synchronized, Properties2)
		;	true
		),
		member(line_count(LC2), Properties2), integer(LC2),
		object_property(test_object, declares(c/3, Properties3)),
		member(private, Properties3),
		member(scope(Scope3), Properties3), Scope3 == private,
		member((dynamic), Properties3),
		member(line_count(LC3), Properties3), integer(LC3),
		\+ object_property(test_object, declares(d/4, _Properties4)),
		object_property(test_object, declares(e/5, Properties5)),
		member(private, Properties5),
		member(scope(Scope3), Properties5), Scope3 == private,
		member(static, Properties5),
		member(line_count(LC5), Properties5), integer(LC5).

	% interface predicate definition properties
	succeeds(object_property_2_10) :-
		object_property(test_object, defines(a/1, Properties1)),
		member(line_count(LC1), Properties1), integer(LC1),
		member(number_of_clauses(NC1), Properties1), integer(NC1),
		object_property(test_object, defines(b/2, Properties2)),
		member(line_count(LC2), Properties2), integer(LC2),
		member(number_of_clauses(NC2), Properties2), NC2 == 2,
		object_property(test_object, defines(c/3, Properties3)),
		member(line_count(LC3), Properties3), integer(LC3),
		member(number_of_clauses(NC3), Properties3), NC3 == 3,
		object_property(test_object, defines(d/4, Properties4)),
		member(line_count(LC4), Properties4), integer(LC4),
		member(number_of_clauses(NC4), Properties4), NC4 == 4,
		object_property(test_object, defines(e/5, Properties5)),
		\+ member(line_count(_LC5), Properties5),
		member(number_of_clauses(NC5), Properties5), NC5 == 0.		

	% check that all queries with explicit properties are valid
	fails(object_property_2_11) :-
		(	object_property(empty_object, built_in)
		;	object_property(empty_object, (dynamic))
		;	object_property(empty_object, static)
		;	object_property(empty_object, debugging)
		;	object_property(empty_object, public(_))
		;	object_property(empty_object, protected(_))
		;	object_property(empty_object, private(_))
		;	object_property(empty_object, declares(_, _))
		;	object_property(empty_object, alias(_, _))
		;	object_property(empty_object, source_data)
		;	object_property(empty_object, info(_))
		;	object_property(empty_object, file(_))
		;	object_property(empty_object, file(_, _))
		;	object_property(empty_object, lines(_, _))
		;	object_property(empty_object, events)
		;	object_property(empty_object, defines(_, _))
		;	object_property(empty_object, includes(_, _, _))
		;	object_property(empty_object, provides(_, _, _))
		;	object_property(empty_object, calls(_, _))
		;	object_property(empty_object, number_of_clauses(_))
		;	object_property(empty_object, number_of_user_clauses(_))
		;	object_property(empty_object, threaded)
		;	object_property(empty_object, context_switching_calls)
		;	object_property(empty_object, dynamic_declarations)
		;	object_property(empty_object, complements)
		;	object_property(empty_object, complements(_))
		),
		% force backtracking into all property queries
		fail.

	% determinism tests

	deterministic(object_property_2_12) :-
		object_property(debug_object, debugging).

	deterministic(object_property_2_13) :-
		object_property(test_object, source_data).

	deterministic(object_property_2_14) :-
		object_property(dynamic_object, (dynamic)).

	deterministic(object_property_2_15) :-
		object_property(test_object, static).

	deterministic(object_property_2_16) :-
		object_property(built_in_object, built_in).

	deterministic(object_property_2_17) :-
		object_property(test_object, file(_)).

	deterministic(object_property_2_18) :-
		object_property(test_object, file(_, _)).

	deterministic(object_property_2_19) :-
		object_property(test_object, lines(_, _)).

	deterministic(object_property_2_20) :-
		object_property(test_object, info(_)).

	deterministic(object_property_2_21) :-
		object_property(options_object, events).

	deterministic(object_property_2_22) :-
		object_property(test_object, number_of_clauses(_)).

	deterministic(object_property_2_23) :-
		object_property(test_object, number_of_user_clauses(_)).

	deterministic(object_property_2_24) :-
		object_property(options_object, context_switching_calls).

	deterministic(object_property_2_25) :-
		object_property(options_object, dynamic_declarations).

	deterministic(object_property_2_26) :-
		object_property(options_object, complements(_)).

	deterministic(object_property_2_27) :-
		object_property(options_object, complements).

	:- if(current_logtalk_flag(threads, supported)).
		deterministic(object_property_2_28) :-
			object_property(threaded_object, threaded).
	:- else.
		- deterministic(object_property_2_28).
	:- endif.

	% auxiliary predicates (avoid library dependencies)

	member(H, [H| _]) :-
		!.
	member(H, [_| T]) :-
		member(H, T).

:- end_object.
