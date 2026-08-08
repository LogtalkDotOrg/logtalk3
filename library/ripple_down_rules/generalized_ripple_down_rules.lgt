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


:- object(generalized_ripple_down_rules,
	implements(ripple_down_rules_common_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-08,
		comment is 'Generalized Ripple-Down Rules implementation using keyed Single-Classification Ripple-Down Rules and Multi-Classification Ripple-Down Rules submodels.'
	]).

	:- public(put/4).
	:- mode(put(+compound, +term, +compound, -compound), one_or_error).
	:- info(put/4, [
		comment is 'Returns a new Generalized Ripple-Down Rules model with the keyed Single-Classification Ripple-Down Rules or Multi-Classification Ripple-Down Rules submodel added or replaced.',
		argnames is ['Model', 'Key', 'Submodel', 'NewModel'],
		exceptions is [
			'``Model`` is a variable' - instantiation_error,
			'``Model`` is neither a variable nor a valid model' - domain_error(ripple_down_rules, 'Model'),
			'``Key`` is a variable' - instantiation_error,
			'``Submodel`` is a variable' - instantiation_error,
			'``Submodel`` is neither a variable nor a valid submodel' - domain_error(ripple_down_rules_submodel, 'Submodel')
		]
	]).

	:- public(get/3).
	:- mode(get(+compound, +term, -compound), zero_or_one_or_error).
	:- info(get/3, [
		comment is 'Returns the submodel stored under a key.',
		argnames is ['Model', 'Key', 'Submodel'],
		exceptions is [
			'``Model`` is a variable' - instantiation_error,
			'``Model`` is neither a variable nor a valid model' - domain_error(ripple_down_rules, 'Model'),
			'``Key`` is a variable' - instantiation_error
		]
	]).

	:- public(remove/3).
	:- mode(remove(+compound, +term, -compound), zero_or_one_or_error).
	:- info(remove/3, [
		comment is 'Returns a new Generalized Ripple-Down Rules model with the keyed submodel removed.',
		argnames is ['Model', 'Key', 'NewModel'],
		exceptions is [
			'``Model`` is a variable' - instantiation_error,
			'``Model`` is neither a variable nor a valid model' - domain_error(ripple_down_rules, 'Model'),
			'``Key`` is a variable' - instantiation_error
		]
	]).

	new(Model) :-
		new(Model, []).

	new(grdr(Map, MaximumCycles), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(maximum_cycles(MaximumCycles), Options),
		avltree::new(Map).

	valid_option(maximum_cycles(MaximumCycles)) :-
		integer(MaximumCycles),
		MaximumCycles > 0.

	default_option(maximum_cycles(16)).

	classify(Model, Case, Conclusions) :-
		classify(Model, Case, Conclusions, _).

	classify(Model, Case, Conclusions, Trace) :-
		check_model(Model),
		Model = grdr(Map, MaximumCycles),
		avltree::as_list(Map, Submodels),
		fixed_point(Submodels, Case, [], MaximumCycles, 1, Conclusions, Trace).

	:- meta_predicate(revise(*, *, *, 2, 3, *)).

	revise(Model, Key-Case, Correction, Condition, Conclusion, NewModel) :-
		check_model(Model),
		get(Model, Key, Submodel),
		submodel_variant(Submodel, Variant),
		call_variant_revise(Variant, Submodel, Case, Correction, Condition, Conclusion, NewSubmodel),
		put(Model, Key, NewSubmodel, NewModel).

	put(Model, Key, Submodel, NewModel) :-
		check_model(Model),
		check_key(Key),
		check_submodel(Submodel),
		Model = grdr(Map, MaximumCycles),
		avltree::insert(Map, Key, Submodel, NewMap),
		NewModel = grdr(NewMap, MaximumCycles).

	get(Model, Key, Submodel) :-
		check_model(Model),
		check_key(Key),
		Model = grdr(Map, _),
		avltree::lookup(Key, Submodel, Map).

	remove(Model, Key, NewModel) :-
		check_model(Model),
		check_key(Key),
		Model = grdr(Map, MaximumCycles),
		avltree::delete(Map, Key, _, NewMap),
		NewModel = grdr(NewMap, MaximumCycles).

	as_list(Model, Rules) :-
		check_model(Model),
		Model = grdr(Map, _),
		avltree::as_list(Map, Submodels),
		submodel_descriptors(Submodels, Rules).

	size(Model, Size) :-
		check_model(Model),
		Model = grdr(Map, _),
		avltree::as_list(Map, Submodels),
		submodel_size(Submodels, 0, Size).

	check_model(Model) :-
		(   var(Model) ->
			instantiation_error
		;   valid_model(Model) ->
			true
		;   domain_error(ripple_down_rules, Model)
		).

	valid_model(grdr(Map, MaximumCycles)) :-
		integer(MaximumCycles),
		MaximumCycles > 0,
		avltree::valid(Map),
		avltree::as_list(Map, Submodels),
		valid_submodels(Submodels).

	check_key(Key) :-
		(   var(Key) ->
			instantiation_error
		;   true
		).

	check_submodel(Submodel) :-
		(   var(Submodel) ->
			instantiation_error
		;   submodel_variant(Submodel, single_classification_ripple_down_rules),
			single_classification_ripple_down_rules::valid_model(Submodel) ->
			true
		;   submodel_variant(Submodel, multi_classification_ripple_down_rules),
			multi_classification_ripple_down_rules::valid_model(Submodel) ->
			true
		;   domain_error(ripple_down_rules_submodel, Submodel)
		).

	submodel_variant(scrdr(_, _, _), single_classification_ripple_down_rules).
	submodel_variant(mcrdr(_, _, _), multi_classification_ripple_down_rules).

	:- meta_predicate(call_variant_revise(*, *, *, *, 2, 3, *)).

	call_variant_revise(single_classification_ripple_down_rules, Model, Case, Correction, Condition, Conclusion, NewModel) :-
		single_classification_ripple_down_rules::revise(Model, Case, Correction, Condition, Conclusion, NewModel).
	call_variant_revise(multi_classification_ripple_down_rules, Model, Case, Correction, Condition, Conclusion, NewModel) :-
		multi_classification_ripple_down_rules::revise(Model, Case, Correction, Condition, Conclusion, NewModel).

	fixed_point(_, _, _, MaximumCycles, Cycle, _, _) :-
		Cycle > MaximumCycles,
		domain_error(grdr_non_convergence, maximum_cycles(MaximumCycles)).
	fixed_point(Submodels, Case, Conclusions0, MaximumCycles, Cycle, Conclusions, [pass(Cycle, PassTrace)| Trace]) :-
		!,
		evaluate_pass(Submodels, Case, Conclusions0, UnorderedConclusions, PassTrace),
		keysort(UnorderedConclusions, Conclusions1),
		(   Conclusions1 == Conclusions0 ->
			Conclusions = Conclusions1,
			Trace = []
		;   NextCycle is Cycle + 1,
			fixed_point(Submodels, Case, Conclusions1, MaximumCycles, NextCycle, Conclusions, Trace)
		).

	evaluate_pass([], _, Conclusions, Conclusions, []).
	evaluate_pass([Key-Submodel| Submodels], Case, Conclusions0, Conclusions, [key(Key, Values, RuleTrace)| Trace]) :-
		classify_submodel(Submodel, Case, Conclusions0, Values, RuleTrace),
		put_conclusion(Key, Values, Conclusions0, Conclusions1),
		evaluate_pass(Submodels, Case, Conclusions1, Conclusions, Trace).

	classify_submodel(Submodel, Case, Context, Value, Trace) :-
		submodel_variant(Submodel, single_classification_ripple_down_rules),
		!,
		single_classification_ripple_down_rules::classify_with_context(Submodel, Case, Context, Value, Trace).
	classify_submodel(Submodel, Case, Context, Values, Trace) :-
		multi_classification_ripple_down_rules::classify_with_context(Submodel, Case, Context, Values, Trace).

	put_conclusion(Key, none, Conclusions, NewConclusions) :-
		!,
		delete_key(Conclusions, Key, NewConclusions).
	put_conclusion(Key, [], Conclusions, NewConclusions) :-
		!,
		delete_key(Conclusions, Key, NewConclusions).
	put_conclusion(Key, Value, Conclusions, NewConclusions) :-
		update_key(Conclusions, Key, Value, NewConclusions).

	update_key([], Key, Value, [Key-Value]).
	update_key([Key-_| Conclusions], Key, Value, [Key-Value| Conclusions]) :-
		!.
	update_key([Pair| Conclusions], Key, Value, [Pair| NewConclusions]) :-
		update_key(Conclusions, Key, Value, NewConclusions).

	delete_key([], _, []).
	delete_key([Key-_| Conclusions], Key, Conclusions) :-
		!.
	delete_key([Pair| Conclusions], Key, [Pair| NewConclusions]) :-
		delete_key(Conclusions, Key, NewConclusions).

	submodel_descriptors([], []).
	submodel_descriptors([Key-Submodel| Submodels], [key(Key, Variant, Rules)| Descriptors]) :-
		submodel_variant(Submodel, Variant),
		Variant::as_list(Submodel, Rules),
		submodel_descriptors(Submodels, Descriptors).

	submodel_size([], Size, Size).
	submodel_size([_-Submodel| Submodels], Size0, Size) :-
		submodel_variant(Submodel, Variant),
		Variant::size(Submodel, SubmodelSize),
		Size1 is Size0 + SubmodelSize,
		submodel_size(Submodels, Size1, Size).

	valid_submodels([]).
	valid_submodels([Key-Submodel| Submodels]) :-
		nonvar(Key),
		check_submodel(Submodel),
		valid_submodels(Submodels).

:- end_object.
