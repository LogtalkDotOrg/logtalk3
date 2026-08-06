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


:- object(multi_classification_ripple_down_rules,
	implements(ripple_down_rules_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-06,
		comment is 'Multi-Classification Ripple-Down Rules implementation.'
	]).

	:- uses(list, [
		append/3, length/2, member/2, valid/1 as proper_list/1
	]).

	new(Model) :-
		new(Model, []).

	new(mcrdr(Default, [], 1), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(default(Default), Options).

	valid_option(default(Default)) :-
		nonvar(Default),
		proper_list(Default).

	default_option(default([])).

	classify(Model, Case, Conclusions) :-
		classify(Model, Case, Conclusions, _).

	classify(Model, Case, Conclusions, Trace) :-
		classify_with_context(Model, Case, [], Conclusions, Trace).

	classify_with_context(Model, Case, Context, Conclusions, Trace) :-
		check_model(Model),
		Model = mcrdr(Default, Rules, _),
		evaluate_rules(Rules, Case, Context, Default, Conclusions, Trace, []).

	:- meta_predicate(revise(*, *, *, 2, 3, *)).

	revise(Model, Case, Correction, Condition, Conclusion, NewModel) :-
		check_model(Model),
		check_correction(Correction),
		check_callable(Condition, ripple_down_rule_condition),
		check_callable(Conclusion, ripple_down_rule_conclusion),
		Model = mcrdr(Default, Rules, NextId),
		classify(Model, Case, Actual, Trace),
		(   invoke_condition(Condition, Case, Actual) ->
			true
		;   domain_error(ripple_down_rule_condition_for_case, Condition)
		),
		invoke_conclusion(Conclusion, Case, Actual, Value),
		revise_rules(Correction, Rules, NextId, Case, Condition, Conclusion, Value, Trace, NewRules),
		NewNextId is NextId + 1,
		NewModel = mcrdr(Default, NewRules, NewNextId).

	as_list(Model, Rules) :-
		check_model(Model),
		Model = mcrdr(_, Roots, _),
		rule_descriptors(Roots, Rules, []).

	size(Model, Size) :-
		as_list(Model, Rules),
		length(Rules, Size).

	check_model(Model) :-
		(   var(Model) ->
			instantiation_error
		;   valid_model(Model) ->
			true
		;   domain_error(ripple_down_rules, Model)
		).

	valid_model(mcrdr(Default, Rules, NextId)) :-
		check_conclusions(Default),
		integer(NextId),
		NextId > 0,
		valid_rules(Rules, NextId, Ids, []),
		sort(Ids, UniqueIds),
		length(Ids, Count),
		length(UniqueIds, Count),
		NextId =:= Count + 1.

	check_correction(Correction) :-
		(   var(Correction) ->
			instantiation_error
		;   Correction == add ->
			true
		;   Correction == remove ->
			true
		;   Correction == filter ->
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

	check_conclusions(Conclusions) :-
		(   var(Conclusions) ->
			instantiation_error
		;   proper_list(Conclusions) ->
			true
		;   type_error(list, Conclusions)
		).

	revise_rules(Correction, Rules, Id, Case, Condition, Conclusion, Value, Trace, NewRules) :-
		(	Correction == add ->
			append(Rules, [rule(Id, add, Condition, Conclusion, Case, [])], NewRules)
		;	contributor(Trace, Value, ParentId, Cornerstone),
			(   invoke_condition(Condition, Cornerstone, []) ->
				domain_error(ripple_down_rule_condition_for_cornerstone, Condition)
			;   insert_refinement(Rules, ParentId, rule(Id, Correction, Condition, Conclusion, Case, []), NewRules)
			)
		).

	contributor([contributed(Id, Value, Cornerstone)| _], Value, Id, Cornerstone) :-
		!.
	contributor([_| Trace], Value, Id, Cornerstone) :-
		contributor(Trace, Value, Id, Cornerstone).
	contributor([], Value, _, _) :-
		domain_error(mcrdr_conclusion, Value).

	evaluate_rules([], _, _, Conclusions, Conclusions, Trace, Trace).
	evaluate_rules([Rule| Rules], Case, Context, Conclusions0, Conclusions, Trace, Tail) :-
		append(Context, Conclusions0, VisibleConclusions),
		evaluate_rule(Rule, Case, VisibleConclusions, Conclusions0, Conclusions1, Trace, Next),
		evaluate_rules(Rules, Case, Context, Conclusions1, Conclusions, Next, Tail).

	evaluate_rule(rule(Id, add, Condition, Conclusion, Cornerstone, Refinements), Case, VisibleConclusions, Conclusions0, Conclusions, Trace, Tail) :-
		(   invoke_condition(Condition, Case, VisibleConclusions) ->
			invoke_conclusion(Conclusion, Case, VisibleConclusions, Value0),
			evaluate_refinements(Refinements, Case, VisibleConclusions, Value0, Action, Trace0, Tail),
			apply_action(Action, Id, Cornerstone, Conclusions0, Conclusions, Trace, Trace0)
		;   Conclusions = Conclusions0,
			Trace = [evaluated(Id, false, false)| Tail]
		).

	evaluate_refinements([], _, _, Value, keep(Value), Trace, Trace).
	evaluate_refinements([rule(Id, Kind, Condition, Conclusion, _, Children)| Rules], Case, Conclusions, Value0, Action, Trace, Tail) :-
		(   invoke_condition(Condition, Case, Conclusions) ->
			(   Kind == remove ->
				Action = remove,
				Trace = [evaluated(Id, true, true)| Tail]
			;   invoke_conclusion(Conclusion, Case, Conclusions, Value1),
				evaluate_refinements(Children, Case, Conclusions, Value1, Action, Trace1, Tail),
				Trace = [evaluated(Id, true, true)| Trace1]
			)
		;   Trace = [evaluated(Id, false, false)| Trace1],
			evaluate_refinements(Rules, Case, Conclusions, Value0, Action, Trace1, Tail)
		).

	apply_action(remove, Id, _, Conclusions, Conclusions, [evaluated(Id, true, false)| Trace], Trace).
	apply_action(keep(Value), Id, Cornerstone, Conclusions0, Conclusions, [evaluated(Id, true, true), contributed(Id, Value, Cornerstone)| Trace], Trace) :-
		add_unique(Value, Conclusions0, Conclusions).

	add_unique(Value, Conclusions, Conclusions) :-
		member(Value, Conclusions),
		!.
	add_unique(Value, Conclusions, NewConclusions) :-
		append(Conclusions, [Value], NewConclusions).

	:- meta_predicate(invoke_condition(2, *, *)).

	invoke_condition(Condition, Case, Conclusions) :-
		call(Condition, Case, Conclusions).

	:- meta_predicate(invoke_conclusion(3, *, *, *)).

	invoke_conclusion(Conclusion, Case, Conclusions, Result) :-
		call(Conclusion, Case, Conclusions, Result).

	insert_refinement([rule(Id, Kind, Condition, Conclusion, Cornerstone, Refinements)| Rules], Id, NewRule,
			[rule(Id, Kind, Condition, Conclusion, Cornerstone, NewRefinements)| Rules]) :-
		append(Refinements, [NewRule], NewRefinements),
		!.
	insert_refinement([Rule| Rules0], ParentId, NewRule, [Rule| Rules]) :-
		insert_refinement(Rules0, ParentId, NewRule, Rules).

	rule_descriptors([], Tail, Tail).
	rule_descriptors([Rule| Rules], Descriptors, Tail) :-
		rule_descriptor(Rule, none, top, Descriptors, Next),
		rule_descriptors(Rules, Next, Tail).

	rule_descriptor(rule(Id, Kind, Condition, Conclusion, Cornerstone, Children), Parent, Edge,
			[rule(Id, Parent, Edge, Condition, Conclusion, Cornerstone)| Rules], Tail) :-
		child_descriptors(Children, Id, Kind, Rules, Tail).

	child_descriptors([], _, _, Tail, Tail).
	child_descriptors([Rule| Rules], Parent, _, Descriptors, Tail) :-
		Rule = rule(_, Kind, _, _, _, _),
		rule_descriptor(Rule, Parent, Kind, Descriptors, Next),
		child_descriptors(Rules, Parent, Kind, Next, Tail).

	valid_rules([], _, Tail, Tail).
	valid_rules([Rule| Rules], NextId, Ids, Tail) :-
		valid_rule(Rule, NextId, Ids, Next),
		valid_rules(Rules, NextId, Next, Tail).

	valid_rule(rule(Id, Kind, Condition, Conclusion, _, Children), NextId, [Id| Ids], Tail) :-
		integer(Id),
		Id > 0,
		Id < NextId,
		atom(Kind),
		valid_kind(Kind),
		callable(Condition),
		callable(Conclusion),
		valid_rules(Children, NextId, Ids, Tail).

	valid_kind(add).
	valid_kind(remove).
	valid_kind(filter).

:- end_object.
