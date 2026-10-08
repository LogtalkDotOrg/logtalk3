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


:- object(lasso_feature_selector,
	imports(feature_selector_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Regression feature selection by maximum absolute Lasso coefficient per original feature, with optional deterministic holdout regularization search. Search requires two usable rows.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(format, [
		format/2
	]).

	:- private(usable_examples/3).
	:- mode(usable_examples(+list(compound), +object_identifier, -non_negative_integer), one_or_error).
	:- info(usable_examples/3, [
		comment is 'Counts known numeric targets after shared dataset validation.',
		argnames is ['Examples', 'Dataset', 'Count'],
		exceptions is [
			'A known target is not numeric' - type_error(number, 'Target'),
			'No known target remains' - domain_error(non_empty_examples, 'Dataset')
		]
	]).

	:- private(count_numeric_targets/2).
	:- mode(count_numeric_targets(+list(compound), -non_negative_integer), one_or_error).
	:- info(count_numeric_targets/2, [
		comment is 'Counts numeric targets while skipping unbound targets.',
		argnames is ['Examples', 'Count'],
		exceptions is [
			'A known target is not numeric' - type_error(number, 'Target')
		]
	]).

	:- private(check_search_count/1).
	:- mode(check_search_count(+non_negative_integer), one_or_error).
	:- info(check_search_count/1, [
		comment is 'Requires at least two known numeric targets for holdout search.',
		argnames is ['Count'],
		exceptions is [
			'Fewer than two usable rows remain' - domain_error(lasso_search_examples, 'Count')
		]
	]).

	:- private(group_scores/3).
	:- mode(group_scores(+list(compound), +list(float), -list(pair)), zero_or_one).
	:- info(group_scores/3, [
		comment is 'Consumes exact encoder blocks and returns original feature scores in encoder order.',
		argnames is ['Encoders', 'Weights', 'Scores']
	]).

	:- private(encoder_block/3).
	:- mode(encoder_block(+compound, -atom, -positive_integer), zero_or_one).
	:- info(encoder_block/3, [
		comment is 'Returns an encoder name and its number of coefficient columns.',
		argnames is ['Encoder', 'Feature', 'Count']
	]).

	:- private(block_maximum/5).
	:- mode(block_maximum(+non_negative_integer, +list(float), +float, -float, -list(float)), zero_or_one).
	:- info(block_maximum/5, [
		comment is 'Consumes a coefficient block and computes its maximum absolute weight.',
		argnames is ['Count', 'Weights', 'Maximum0', 'Maximum', 'Remaining']
	]).

	:- private(select_active/3).
	:- mode(select_active(+list(pair), +list(compound), -list(atom)), one).
	:- info(select_active/3, [
		comment is 'Applies the selection strategy only to groups strictly above the coefficient cutoff.',
		argnames is ['Scores', 'Options', 'Selected']
	]).

	:- private(active_scores/3).
	:- mode(active_scores(+list(pair), +number, -list(pair)), one).
	:- info(active_scores/3, [
		comment is 'Retains scores strictly above the coefficient cutoff.',
		argnames is ['Scores', 'Cutoff', 'Active']
	]).

	:- private(model_diagnostics/8).
	:- mode(model_diagnostics(+compound, +list(pair), +list(atom), +positive_integer, +positive_integer, +list(compound), +term, -list(compound)), one).
	:- info(model_diagnostics/8, [
		comment is 'Builds counts, effective options, aggregation metadata, and complete nested learner diagnostics.',
		argnames is ['Regressor', 'Scores', 'Selected', 'OriginalCount', 'UsableCount', 'Options', 'SearchResult', 'Diagnostics']
	]).

	:- private(valid_model/1).
	:- mode(valid_model(+ground), zero_or_one).
	:- info(valid_model/1, [
		comment is 'Checks the retained regressor and recomputes scores, selection, and metadata without binding a partial input.',
		argnames is ['Selector'],
		exceptions is [
			'The retained regressor is invalid' - domain_error(regressor, 'Regressor'),
			'A stored option is invalid' - domain_error(option, 'Option'),
			'A generated regularization bound exceeds backend floating-point limits' - evaluation_error(float_overflow),
			'Stored options are not a list' - type_error(list, 'Options'),
			'A stored option is not compound' - type_error(compound, 'Option')
		]
	]).

	:- private(store_regressor_options/3).
	:- mode(store_regressor_options(+list(compound), +list(compound), -list(compound)), one).
	:- info(store_regressor_options/3, [
		comment is 'Replaces the first nested options occurrence with the learner effective options, preserving later occurrences.',
		argnames is ['Options', 'RegressorOptions', 'Stored']
	]).

	:- private(complete_regressor_options/2).
	:- mode(complete_regressor_options(+list(compound), +list(compound)), zero_or_one).
	:- info(complete_regressor_options/2, [
		comment is 'Checks that each learner default option indicator has an effective value.',
		argnames is ['Defaults', 'Options']
	]).

	learn(Dataset, Selector, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, MergedOptions),
		^^dataset_examples(Dataset, Examples),
		length(Examples, OriginalCount),
		usable_examples(Examples, Dataset, UsableCount),
		^^option(regressor_options(RegressorOptions), MergedOptions),
		^^option(regularization_search(Search), MergedOptions),
		search_options(Search, Dataset, Examples, UsableCount, RegressorOptions, FitOptions, SearchResult),
		lasso_regression::learn(regression_dataset_adapter(Dataset), Regressor, FitOptions),
		lasso_regression::regressor_options(Regressor, EffectiveRegressorOptions),
		store_regressor_options(MergedOptions, EffectiveRegressorOptions, Options),
		Regressor = lasso_regressor(Encoders, _Bias, Weights, _NestedDiagnostics),
		group_scores(Encoders, Weights, Unsorted),
		^^sort_by_decreasing_score(Unsorted, Scores),
		select_active(Scores, Options, Selected),
		model_diagnostics(Regressor, Scores, Selected, OriginalCount, UsableCount, Options, SearchResult, Diagnostics),
		Selector = lasso_feature_selector(Regressor, Scores, Selected, Diagnostics).

	:- private(valid_regularization_grid/1).
	:- mode(valid_regularization_grid(@term), zero_or_one).
	:- info(valid_regularization_grid/1, [
		comment is 'Checks an explicit penalty list or an increasing bounded linear grid with at least two candidates.',
		argnames is ['Grid']
	]).

	:- private(regularization_grid_values/2).
	:- mode(regularization_grid_values(+term, -list(number)), one_or_error).
	:- info(regularization_grid_values/2, [
		comment is 'Expands a validated linear grid to floating-point penalties or preserves an explicit penalty list.',
		argnames is ['Grid', 'Values'],
		exceptions is [
			'A generated bound exceeds backend floating-point limits' - evaluation_error(float_overflow)
		]
	]).

	search_options(none, _Dataset, _Examples, _Count, Options, Options, none).
	search_options(holdout(Fraction, Grid), Dataset, Examples, Count, Options, FitOptions,
		holdout(TrainingCount, ValidationCount, Trials, Winner)) :-
		check_search_count(Count),
		regularization_grid_values(Grid, Values),
		ValidationCount is min(Count - 1, max(1, ceiling(Fraction * Count))),
		TrainingCount is Count - ValidationCount,
		known_examples(Examples, Known),
		split_examples(TrainingCount, Known, Training, Validation),
		findall(Feature-Domain, regression_dataset_adapter(Dataset)::attribute_values(Feature, Domain), Declarations),
		search_trials(Values, Declarations, Training, Validation, ValidationCount, Options, Trials),
		best_trial(Trials, Winner),
		penalty_options(Options, Winner, FitOptions).

	check_search_count(Count) :-
		(	Count >= 2 ->
			true
		;	domain_error(lasso_search_examples, Count)
		).

	valid_regularization_grid(Grid) :-
		(	nonvar(Grid), Grid = linear(Minimum, Maximum, Count) ->
			number(Minimum),
			number(Maximum),
			Minimum >= 0,
			Minimum < Maximum,
			integer(Count),
			Count >= 2
		;	valid(non_empty_list(non_negative_number), Grid)
		).

	regularization_grid_values(Grid, Values) :-
		(	Grid = linear(Minimum, Maximum, Count) ->
			Lower is Minimum + 0.0,
			Upper is Maximum + 0.0,
			Last is Count - 1,
			Values = [Lower| Rest],
			linear_grid_values(1, Last, Lower, Upper, Rest)
		;	Values = Grid
		).

	linear_grid_values(Last, Last, _Minimum, Maximum, [Maximum]) :-
		!.
	linear_grid_values(Index, Last, Minimum, Maximum, [Value| Values]) :-
		Value is Minimum + (Maximum - Minimum) * (Index / Last),
		Next is Index + 1,
		linear_grid_values(Next, Last, Minimum, Maximum, Values).

	known_examples([], []).
	known_examples([example(Id, Pairs, Target)| Examples], Known) :-
		(	var(Target) ->
			Known = Rest
		;	Known = [example(Id, Pairs, Target)| Rest]
		),
		known_examples(Examples, Rest).

	split_examples(0, Examples, [], Examples) :-
		!.
	split_examples(Count, [Example| Examples], [Example| Training], Validation) :-
		Next is Count - 1,
		split_examples(Next, Examples, Training, Validation).

	search_trials([], _Declarations, _Training, _Validation, _Count, _Options, []).
	search_trials([Value| Values], Declarations, Training, Validation, Count, Options,
		[trial(Value, MSE, Convergence, Iterations, Delta)| Trials]) :-
		penalty_options(Options, Value, CandidateOptions),
		lasso_regression::learn(regression_examples_adapter(Declarations, Training), Candidate, CandidateOptions),
		validation_error(Validation, Candidate, 0.0, Error),
		MSE is Error / Count,
		lasso_regression::diagnostics(Candidate, Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(final_delta(Delta), Diagnostics),
		search_trials(Values, Declarations, Training, Validation, Count, Options, Trials).

	validation_error([], _Regressor, Error, Error).
	validation_error([example(_Id, Pairs, Target)| Examples], Regressor, Error0, Error) :-
		(	Regressor = lasso_regressor([], Prediction, [], _) ->
			true
		;	lasso_regression::predict(Regressor, Pairs, Prediction)
		),
		Difference is Prediction - Target,
		Error1 is Error0 + Difference * Difference,
		validation_error(Examples, Regressor, Error1, Error).

	penalty_options([], Value, [regularization(Penalty)]) :-
		Penalty is Value + 0.0.
	penalty_options([regularization(_)| Options], Value, [regularization(Penalty)| Options]) :-
		!,
		Penalty is Value + 0.0.
	penalty_options([Option| Options], Value, [Option| FitOptions]) :-
		penalty_options(Options, Value, FitOptions).

	best_trial([trial(Value, Error, _, _, _)| Trials], Winner) :-
		best_trial(Trials, Value, Error, Winner).

	best_trial([], Winner, _Error, Winner).
	best_trial([trial(Value, Error, _, _, _)| Trials], Value0, Error0, Winner) :-
		(	Error < Error0 ->
			Value1 = Value,
			Error1 = Error
		;	Error =:= Error0, Value > Value0 ->
			Value1 = Value,
			Error1 = Error
		;	Value1 = Value0,
			Error1 = Error0
		),
		best_trial(Trials, Value1, Error1, Winner).

	usable_examples(Examples, Dataset, Count) :-
		count_numeric_targets(Examples, Count),
		(	Count > 0 ->
			true
		;	domain_error(non_empty_examples, Dataset)
		).

	count_numeric_targets(Examples, Count) :-
		count_numeric_targets(Examples, 0, Count).

	count_numeric_targets([], Count, Count).
	count_numeric_targets([example(_Id, _Pairs, Target)| Examples], Count0, Count) :-
		(	var(Target) ->
			Increment = 0
		;	number(Target) ->
			Increment = 1
		;	type_error(number, Target)
		),
		Count1 is Count0 + Increment,
		count_numeric_targets(Examples, Count1, Count).

	group_scores([], [], []).
	group_scores([Encoder| Encoders], Weights, [Feature-Score| Scores]) :-
		encoder_block(Encoder, Feature, Count),
		block_maximum(Count, Weights, 0.0, Score, Remaining),
		group_scores(Encoders, Remaining, Scores).

	encoder_block(continuous(Feature, _Mean, _Scale), Feature, 2) :-
		!.
	encoder_block(categorical(Feature, Values), Feature, Count) :-
		length(Values, Count).

	block_maximum(0, Weights, Maximum, Maximum, Weights) :-
		!.
	block_maximum(Count, [Weight| Weights], Maximum0, Maximum, Remaining) :-
		Count > 0,
		Maximum1 is max(Maximum0, abs(Weight)),
		NextCount is Count - 1,
		block_maximum(NextCount, Weights, Maximum1, Maximum, Remaining).

	select_active(Scores, Options, Selected) :-
		^^option(coefficient_threshold(Cutoff), Options),
		active_scores(Scores, Cutoff, Active),
		^^option(selection_strategy(Strategy), Options),
		(	Strategy == all ->
			length(Active, Count),
			^^select_top_k(Active, Count, Selected)
		;	Strategy = top_k(Count) ->
			^^select_top_k(Active, Count, Selected)
		;	Strategy = threshold(Threshold),
			^^select_above_threshold(Active, Threshold, Selected)
		).

	active_scores([], _Cutoff, []).
	active_scores([Feature-Score| Scores], Cutoff, Active) :-
		(	Score > Cutoff ->
			Active = [Feature-Score| Rest]
		;	Active = Rest
		),
		active_scores(Scores, Cutoff, Rest).

	model_diagnostics(Regressor, Scores, Selected, OriginalCount, UsableCount, Options, SearchResult, Diagnostics) :-
		lasso_regression::diagnostics(Regressor, NestedDiagnostics),
		memberchk(encoded_feature_count(EncodedCount), NestedDiagnostics),
		length(Scores, CandidateCount),
		length(Selected, SelectedCount),
		ExcludedCount is OriginalCount - UsableCount,
		(	Scores = [_-Maximum| _] ->
			true
		;	Maximum = 0.0
		),
		^^base_selector_diagnostics(lasso_feature_selector, OriginalCount, Options, [
			usable_example_count(UsableCount),
			excluded_example_count(ExcludedCount),
			candidate_count(CandidateCount),
			selected_count(SelectedCount),
			encoded_feature_count(EncodedCount),
			aggregation(max_abs),
			maximum_absolute_coefficient(Maximum),
			regressor_diagnostics(NestedDiagnostics),
			regularization_search_result(SearchResult)
		], Diagnostics).

	selected_features(lasso_feature_selector(_Regressor, _Scores, Selected, _Diagnostics), Selected).

	feature_scores(lasso_feature_selector(_Regressor, Scores, _Selected, _Diagnostics), Scores).

	check_selector(Selector) :-
		(	\+ ground(Selector) ->
			instantiation_error
		;	catch(valid_model(Selector), _Error, fail) ->
			true
		;	domain_error(selector, Selector)
		).

	valid_model(lasso_feature_selector(Regressor, Scores, Selected, Diagnostics)) :-
		Regressor = lasso_regressor(Encoders, _Bias, Weights, NestedDiagnostics),
		lasso_regression::check_regressor(Regressor),
		^^valid_selector_metadata(lasso_feature_selector, Diagnostics),
		memberchk(options(Options), Diagnostics),
		^^check_options(Options),
		^^merge_options(Options, EffectiveOptions),
		Options == EffectiveOptions,
		^^option(regressor_options(RegressorOptions), Options),
		lasso_regression::regressor_options(Regressor, StoredOptions),
		RegressorOptions == StoredOptions,
		lasso_regression::default_options(Defaults),
		complete_regressor_options(Defaults, StoredOptions),
		memberchk(target(Target), NestedDiagnostics),
		Target == target,
		memberchk(training_example_count(UsableCount), NestedDiagnostics),
		valid(positive_integer, UsableCount),
		memberchk(example_count(OriginalCount), Diagnostics),
		OriginalCount >= UsableCount,
		group_scores(Encoders, Weights, Unsorted),
		^^sort_by_decreasing_score(Unsorted, ExpectedScores),
		Scores == ExpectedScores,
		select_active(Scores, Options, ExpectedSelected),
		Selected == ExpectedSelected,
		^^option(regularization_search(Search), Options),
		memberchk(regularization_search_result(SearchResult), Diagnostics),
		valid_search_result(Search, UsableCount, StoredOptions, SearchResult),
		model_diagnostics(Regressor, Scores, Selected, OriginalCount, UsableCount, Options, SearchResult, ExpectedDiagnostics),
		Diagnostics == ExpectedDiagnostics.

	valid_search_result(none, _Count, _Options, none).
	valid_search_result(holdout(Fraction, Grid), Count, Options, holdout(TrainingCount, ValidationCount, Trials, Winner)) :-
		Count >= 2,
		regularization_grid_values(Grid, Values),
		valid(positive_integer, TrainingCount),
		valid(positive_integer, ValidationCount),
		ValidationCount =:= min(Count - 1, max(1, ceiling(Fraction * Count))),
		Count =:= TrainingCount + ValidationCount,
		valid_trials(Values, Trials, Options),
		best_trial(Trials, Expected),
		Winner == Expected,
		lasso_regression::option(regularization(Penalty), Options),
		Winner =:= Penalty.

	valid_trials([], [], _Options).
	valid_trials([Value| Values], [trial(Value, Error, Convergence, Iterations, Delta)| Trials], Options) :-
		number(Error),
		Error >= 0,
		valid(positive_integer, Iterations),
		lasso_regression::option(maximum_iterations(Maximum), Options),
		Iterations =< Maximum,
		( Convergence == tolerance -> true
		; Convergence == maximum_iterations_exhausted,
			Iterations =:= Maximum
		),
		number(Delta),
		Delta >= 0,
		valid_trials(Values, Trials, Options).

	selector_export_template(_Dataset, _Selector, Functor, Template) :-
		Template =.. [Functor, 'Selector'].

	selector_term_template(lasso_feature_selector(_Regressor, _Scores, _Selected, _Diagnostics),
		lasso_feature_selector('Regressor', 'FeatureScores', 'SelectedFeatures', 'Diagnostics')).

	export_to_clauses(_Dataset, Selector, Functor, [Clause]) :-
		Clause =.. [Functor, Selector].

	print_selector(Selector) :-
		^^print_selector_template(Selector),
		Selector = lasso_feature_selector(Regressor, Scores, Selected, Diagnostics),
		format('Feature scores: ~w~nSelected features: ~w~nDiagnostics: ~w~n', [Scores, Selected, Diagnostics]),
		lasso_regression::print_regressor(Regressor).

	default_option(regressor_options([])).
	default_option(coefficient_threshold(0.0)).
	default_option(selection_strategy(all)).
	default_option(regularization_search(none)).

	valid_option(regressor_options(Options)) :-
		lasso_regression::valid_options(Options).
	valid_option(coefficient_threshold(Cutoff)) :-
		number(Cutoff),
		Cutoff >= 0.
	valid_option(selection_strategy(Strategy)) :-
		(	Strategy == all ->
			true
		;	Strategy = top_k(Count),
			integer(Count),
			Count > 0
		).
	valid_option(selection_strategy(threshold(Threshold))) :-
		number(Threshold).
	valid_option(regularization_search(Search)) :-
		(	Search == none ->
			true
		;	Search = holdout(Fraction, Grid),
			number(Fraction),
			Fraction > 0,
			Fraction < 1,
			valid_regularization_grid(Grid)
		).

	store_regressor_options([regressor_options(_)| Options], RegressorOptions, [regressor_options(RegressorOptions)| Options]) :-
		!.
	store_regressor_options([Option| Options], RegressorOptions, [Option| Stored]) :-
		store_regressor_options(Options, RegressorOptions, Stored).

	complete_regressor_options([], _Options).
	complete_regressor_options([Default| Defaults], Options) :-
		functor(Default, Name, Arity),
		functor(Template, Name, Arity),
		lasso_regression::option(Template, Options),
		complete_regressor_options(Defaults, Options).

:- end_object.
