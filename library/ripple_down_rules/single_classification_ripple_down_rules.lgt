%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(single_classification_ripple_down_rules,
	implements(ripple_down_rules_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-06,
		comment is 'Single-Classification Ripple-Down Rules implementation.'
	]).

	:- uses(list, [
		length/2
	]).

	new(Model) :-
		new(Model, []).

	new(scrdr(Default, empty, 1), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(default(Default), Options).

	valid_option(default(_)).

	default_option(default(none)).

	:- meta_predicate(revise(*, *, *, 2, 3, *)).

	classify(Model, Case, Conclusion) :-
		classify(Model, Case, Conclusion, _).

	classify(Model, Case, Conclusion, Trace) :-
		classify_with_context(Model, Case, [], Conclusion, Trace).

	classify_with_context(Model, Case, Context, Conclusion, Trace) :-
		check_model(Model),
		Model = scrdr(Default, Root, _),
		(   Root == empty ->
			Conclusion = Default,
			Trace = [default(Default)]
		;   evaluate(Root, Case, Context, Matched, RuleConclusion, Trace),
			(   Matched == true ->
				Conclusion = RuleConclusion
			;   Conclusion = Default
			)
		).

	revise(Model, Case, Correction, Condition, Conclusion, NewModel) :-
		check_model(Model),
		check_correction(Correction),
		check_callable(Condition, ripple_down_rule_condition),
		check_callable(Conclusion, ripple_down_rule_conclusion),
		(   invoke_condition(Condition, Case, []) ->
			true
		;   domain_error(ripple_down_rule_condition_for_case, Condition)
		),
		Model = scrdr(Default, Root, NextId),
		(   Root == empty ->
			NewRoot = rule(NextId, Condition, Conclusion, Case, empty, empty)
		;   classify(Model, Case, _, Trace),
			attachment(Trace, ParentId, Edge),
			cornerstone(Root, ParentId, Cornerstone),
			(   invoke_condition(Condition, Cornerstone, []) ->
				domain_error(ripple_down_rule_condition_for_cornerstone, Condition)
			;   insert_rule(Root, ParentId, Edge, rule(NextId, Condition, Conclusion, Case, empty, empty), NewRoot)
			)
		),
		NewNextId is NextId + 1,
		NewModel = scrdr(Default, NewRoot, NewNextId).

	as_list(Model, Rules) :-
		check_model(Model),
		Model = scrdr(_, Root, _),
		(   Root == empty ->
			Rules = []
		;   rule_descriptors(Root, none, root, Rules, [])
		).

	size(Model, Size) :-
		as_list(Model, Rules),
		length(Rules, Size).

	check_model(Model) :-
		(   var(Model) ->
			instantiation_error
		;   valid_model(Model) ->
			true
		;   domain_error(ripple_down_rules_model, Model)
		).

	valid_model(scrdr(_, empty, 1)) :-
		!.
	valid_model(scrdr(_, Root, NextId)) :-
		Root \== empty,
		integer(NextId),
		NextId > 1,
		valid_rule(Root, NextId, Ids, []),
		sort(Ids, UniqueIds),
		length(Ids, Count),
		length(UniqueIds, Count),
		last_id(UniqueIds, LastId),
		NextId =:= LastId + 1.

	check_correction(Correction) :-
		(   var(Correction) ->
			instantiation_error
		;   Correction == replace ->
			true
		;   domain_error(ripple_down_rule_correction, Correction)
		).

	check_callable(Closure, Domain) :-
		(   var(Closure) ->
			instantiation_error
		;   callable(Closure) ->
			true
		;   domain_error(Domain, Closure)
		).

	evaluate(rule(Id, Condition, Conclusion, _, Except, Else), Case, Conclusions, Matched, Result, Trace) :-
		(   invoke_condition(Condition, Case, Conclusions) ->
			(   Except \== empty ->
				evaluate(Except, Case, Conclusions, ExceptMatched, ExceptResult, ExceptTrace),
				(   ExceptMatched == true ->
					Result = ExceptResult,
					Trace = [evaluated(Id, true, false)| ExceptTrace]
				;   invoke_conclusion(Conclusion, Case, Conclusions, Result),
					Trace = [evaluated(Id, true, true)| ExceptTrace]
				)
			;   invoke_conclusion(Conclusion, Case, Conclusions, Result),
				Trace = [evaluated(Id, true, true)]
			),
			Matched = true
		;   Else \== empty ->
			evaluate(Else, Case, Conclusions, Matched, Result, ElseTrace),
			Trace = [evaluated(Id, false, false)| ElseTrace]
		;   Matched = false,
			Trace = [evaluated(Id, false, false)]
		).

	:- meta_predicate(invoke_condition(2, *, *)).

	invoke_condition(Condition, Case, Conclusions) :-
		call(Condition, Case, Conclusions).

	:- meta_predicate(invoke_conclusion(3, *, *, *)).

	invoke_conclusion(Conclusion, Case, Conclusions, Result) :-
		call(Conclusion, Case, Conclusions, Result).

	attachment(Trace, ParentId, Edge) :-
		last_evaluation(Trace, ParentId, Fired),
		(   Fired == true -> Edge = except; Edge = else ).

	last_evaluation([evaluated(Id, Fired, _)], Id, Fired) :-
		!.
	last_evaluation([_| Trace], Id, Fired) :-
		last_evaluation(Trace, Id, Fired).

	cornerstone(rule(Id, _, _, Cornerstone, _, _), Id, Cornerstone) :-
		!.
	cornerstone(rule(_, _, _, _, Except, _), Id, Cornerstone) :-
		Except \== empty,
		cornerstone(Except, Id, Cornerstone),
		!.
	cornerstone(rule(_, _, _, _, _, Else), Id, Cornerstone) :-
		Else \== empty,
		cornerstone(Else, Id, Cornerstone).

	insert_rule(rule(ParentId, Condition, Conclusion, Cornerstone, empty, Else), ParentId, except, NewRule,
			rule(ParentId, Condition, Conclusion, Cornerstone, NewRule, Else)) :-
		!.
	insert_rule(rule(ParentId, Condition, Conclusion, Cornerstone, Except, empty), ParentId, else, NewRule,
			rule(ParentId, Condition, Conclusion, Cornerstone, Except, NewRule)) :-
		!.
	insert_rule(rule(Id, Condition, Conclusion, Cornerstone, Except0, Else), ParentId, Edge, NewRule,
			rule(Id, Condition, Conclusion, Cornerstone, Except, Else)) :-
		Except0 \== empty,
		insert_rule(Except0, ParentId, Edge, NewRule, Except),
		!.
	insert_rule(rule(Id, Condition, Conclusion, Cornerstone, Except, Else0), ParentId, Edge, NewRule,
			rule(Id, Condition, Conclusion, Cornerstone, Except, Else)) :-
		Else0 \== empty,
		insert_rule(Else0, ParentId, Edge, NewRule, Else).

	rule_descriptors(rule(Id, Condition, Conclusion, Cornerstone, Except, Else), Parent, Edge,
			[rule(Id, Parent, Edge, Condition, Conclusion, Cornerstone)| Rules], Tail) :-
		(   Except == empty ->
			ExceptRules = ElseRules
		;   rule_descriptors(Except, Id, except, ExceptRules, ElseRules)
		),
		(   Else == empty ->
			ElseRules = Tail
		;   rule_descriptors(Else, Id, else, ElseRules, Tail)
		),
		Rules = ExceptRules.

	valid_rule(rule(Id, Condition, Conclusion, _, Except, Else), NextId, [Id| Ids], Tail) :-
		integer(Id),
		Id > 0,
		Id < NextId,
		callable(Condition),
		callable(Conclusion),
		valid_child(Except, NextId, Ids, ElseIds),
		valid_child(Else, NextId, ElseIds, Tail).

	valid_child(empty, _, Tail, Tail) :-
		!.
	valid_child(Rule, NextId, Ids, Tail) :-
		Rule \== empty,
		valid_rule(Rule, NextId, Ids, Tail).

	last_id([Id], Id) :-
		!.
	last_id([_| Ids], Id) :-
		last_id(Ids, Id).

:- end_object.
