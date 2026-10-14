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


:- object(intermittent_demand_forecasting,
	imports(forecaster_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-14,
		comment is 'Croston, SBA, and TSB intermittent-demand forecasters with causal initialization, configurable missing observations, and immutable online updates.'
	]).

	:- public(fitted_values/2).
	:- mode(fitted_values(+object_identifier, -list), one_or_error).
	:- info(fitted_values/2, [
		comment is 'Returns aligned one-step fitted values for ``Dataset`` using default options.',
		argnames is ['Dataset', 'Values'],
		exceptions is [
			'An observation index or the declared series length is a variable' - instantiation_error,
			'The declared series length is not an integer' - type_error(integer, 'DeclaredLength'),
			'The declared series length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The dataset contains no observations' - domain_error(non_empty_series, 'Dataset'),
			'The indices are not a gap-free 1-based sequence' - domain_error(series_index_sequence, 'Dataset'),
			'The declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'An observation is neither a variable nor a number' - type_error(number, 'Observation'),
			'An observation is negative' - domain_error(non_negative_number, 'Observation'),
			'All observations are missing' - domain_error(insufficient_observations, 'Dataset'),
			'Fitting, error-total, or replay arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	:- public(fitted_values/3).
	:- mode(fitted_values(+object_identifier, -list, +list(compound)), one_or_error).
	:- info(fitted_values/3, [
		comment is 'Returns aligned one-step fitted values for ``Dataset`` using the specified ``Options``.',
		argnames is ['Dataset', 'Values', 'Options'],
		exceptions is [
			'An observation index or the declared series length is a variable' - instantiation_error,
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The declared series length is not an integer' - type_error(integer, 'DeclaredLength'),
			'The declared series length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The dataset contains no observations' - domain_error(non_empty_series, 'Dataset'),
			'The indices are not a gap-free 1-based sequence' - domain_error(series_index_sequence, 'Dataset'),
			'The declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'An observation is neither a variable nor a number' - type_error(number, 'Observation'),
			'An observation is negative' - domain_error(non_negative_number, 'Observation'),
			'All observations are missing' - domain_error(insufficient_observations, 'Dataset'),
			'Fitting, error-total, or replay arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	:- public(update/3).
	:- mode(update(+compound, @term, -compound), one_or_error).
	:- info(update/3, [
		comment is 'Returns a new forecaster incorporating ``Observation`` without modifying ``Forecaster``.',
		argnames is ['Forecaster', 'Observation', 'UpdatedForecaster'],
		exceptions is [
			'``Forecaster`` is a variable' - instantiation_error,
			'``Forecaster`` is not a valid intermittent-demand forecaster' - domain_error(forecaster, 'Forecaster'),
			'``Observation`` is neither a variable nor a number' - type_error(number, 'Observation'),
			'``Observation`` is negative' - domain_error(non_negative_number, 'Observation'),
			'Update, error-total, or diagnostic validation arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	learn(Dataset, Forecaster) :-
		^^learn(Dataset, Forecaster).

	learn(Dataset, Forecaster, UserOptions) :-
		prepare_learning(Dataset, Forecaster, UserOptions, discard).

	fitted_values(Dataset, Values) :-
		fitted_values(Dataset, Values, []).

	fitted_values(Dataset, Values, UserOptions) :-
		prepare_learning(Dataset, _Forecaster, UserOptions, values(Values)).

	:- private(replay_series/5).
	:- mode(replay_series(@list, +atom, +compound, +compound, -list), one_or_error).
	:- info(replay_series/5, [
		comment is 'Replays an already validated series using a concrete method, validated parameters, and valid starting state. Emits pre-observation predictions for numeric targets and fresh placeholders for missing targets, preserving order and all input variables.',
		argnames is ['Series', 'Method', 'Parameters', 'State', 'Values'],
		exceptions is [
			'Prediction or transition arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	replay_series([], _Method, _Parameters, _State, []).
	replay_series([Observation| Observations], Method, Parameters, State0, [Prediction| Predictions]) :-
		(	var(Observation) ->
			true
		;	forecast_value(Method, State0, Parameters, Prediction)
		),
		transition(Method, Parameters, State0, Observation, State1),
		replay_series(Observations, Method, Parameters, State1, Predictions).

	:- private(prepare_learning/4).
	:- mode(prepare_learning(+object_identifier, -compound, +list(compound), +compound), one_or_error).
	:- info(prepare_learning/4, [
		comment is 'Collects and validates once, fits all requested configurations using exact RMSE priority, and optionally returns aligned predictions without retaining history in the model.',
		argnames is ['Dataset', 'Forecaster', 'Options', 'Output'],
		exceptions is [
			'An observation index or the declared series length is a variable' - instantiation_error,
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The declared series length is not an integer' - type_error(integer, 'DeclaredLength'),
			'The declared series length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The dataset contains no observations' - domain_error(non_empty_series, 'Dataset'),
			'The indices are not a gap-free 1-based sequence' - domain_error(series_index_sequence, 'Dataset'),
			'The declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'An observation is neither a variable nor a number' - type_error(number, 'Observation'),
			'An observation is negative' - domain_error(non_negative_number, 'Observation'),
			'All observations are missing' - domain_error(insufficient_observations, 'Dataset'),
			'Learning or error-total arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	prepare_learning(Dataset, Forecaster, UserOptions, Output) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, MergedOptions),
		^^option(model(Method), MergedOptions),
		^^option(alpha(Alpha), MergedOptions),
		^^option(beta(Beta), MergedOptions),
		^^option(missing(Policy), MergedOptions),
		^^dataset_series(Dataset, Series),
		demand_summary(Series, summary(0,0,0), Summary),
		Summary = summary(_, Count, _),
		(	Count > 0 ->
			true
		;	domain_error(insufficient_observations, Dataset)
		),
		(	Method \== auto, Alpha \== auto, Beta \== auto ->
			prepare_parameters(Alpha, Beta, Policy, Parameters),
			fit_series_output(Series, Method, Parameters, Output, Fit),
			fitted_model(Summary, Fit, Forecaster)
		;	(	Method == auto ->
				Methods = [paired,tsb]
			;	Methods = [Method]
			),
			(	(Alpha == auto; Beta == auto) ->
				^^option(coefficient_grid(RequestedGrid), MergedOptions),
				normalize_coefficient_grid(RequestedGrid, Grid)
			;	Grid = []
			),
			coefficient_candidates(Alpha, Grid, Alphas),
			coefficient_candidates(Beta, Grid, Betas),
			select_methods(Methods, Alphas, Betas, Series, Policy, none, Best),
			fitted_model(Summary, Best, Forecaster),
			(	Output == discard ->
				true
			;	Output = values(Values),
				Best = fit(SelectedMethod,Parameters,_,_,_,_),
				replay_series(Series, SelectedMethod, Parameters, pending_state(0), Values)
			)
		).

	coefficient_candidates(Value, Grid, Candidates) :-
		(	Value == auto ->
			Candidates = Grid
		;	Candidates = [Value]
		).

	:- private(normalize_coefficient_grid/2).
	:- mode(normalize_coefficient_grid(+list(number), -list(float)), one_or_error).
	:- info(normalize_coefficient_grid/2, [
		comment is 'Normalizes a validated nonempty coefficient grid to ascending distinct floats without modifying the supplied list. Integer 1 and float 1.0 become one candidate. No comparison tolerance is used.',
		argnames is ['Grid', 'NormalizedGrid'],
		exceptions is [
			'Coefficient conversion raises an evaluation error' - evaluation_error('Error')
		]
	]).

	normalize_coefficient_grid(Grid, NormalizedGrid) :-
		findall(Float, (member(Value, Grid), Float is float(Value)), Floats),
		sort(Floats, NormalizedGrid).

	:- private(select_methods/7).
	:- mode(select_methods(+list, +list, +list, @list, +atom, +term, -compound), one_or_error).
	:- info(select_methods/7, [
		comment is 'Evaluates every method while retaining only the best concrete fit.',
		argnames is ['Methods', 'Alphas', 'Betas', 'Series', 'Policy', 'PreviousBest', 'Best'],
		exceptions is ['Candidate arithmetic raises an evaluation error' - evaluation_error('Error')]
	]).

	select_methods([], _Alphas, _Betas, _Series, _Policy, Best, Best).
	select_methods([Method| Methods], Alphas, Betas, Series, Policy, Best0, Best) :-
		select_alphas(Alphas, Method, Betas, Series, Policy, Best0, Best1),
		select_methods(Methods, Alphas, Betas, Series, Policy, Best1, Best).

	:- private(select_alphas/7).
	:- mode(select_alphas(+list, +atom, +list, @list, +atom, +term, -compound), one_or_error).
	:- info(select_alphas/7, [
		comment is 'Evaluates each size coefficient for a concrete method.',
		argnames is ['Alphas', 'Method', 'Betas', 'Series', 'Policy', 'PreviousBest', 'Best'],
		exceptions is ['Candidate arithmetic raises an evaluation error' - evaluation_error('Error')]
	]).

	select_alphas([], _Method, _Betas, _Series, _Policy, Best, Best).
	select_alphas([Alpha| Alphas], Method, Betas, Series, Policy, Best0, Best) :-
		select_betas(Betas, Method, Alpha, Series, Policy, Best0, Best1),
		select_alphas(Alphas, Method, Betas, Series, Policy, Best1, Best).

	:- private(select_betas/7).
	:- mode(select_betas(+list, +atom, +number, @list, +atom, +term, -compound), one_or_error).
	:- info(select_betas/7, [
		comment is 'Fits every occurrence coefficient without suppressing losing-candidate errors.',
		argnames is ['Betas', 'Method', 'Alpha', 'Series', 'Policy', 'PreviousBest', 'Best'],
		exceptions is ['Candidate arithmetic raises an evaluation error' - evaluation_error('Error')]
	]).

	select_betas([], _Method, _Alpha, _Series, _Policy, Best, Best).
	select_betas([Beta| Betas], Method, Alpha, Series, Policy, Best0, Best) :-
		prepare_parameters(Alpha, Beta, Policy, Parameters),
		(	Method == paired ->
			fit_paired_series(Series, Parameters, SBA, Croston),
			better_candidate(SBA, Best0, Intermediate),
			better_candidate(Croston, Intermediate, Best1)
		;	fit_series(Series, Method, Parameters, Candidate),
			better_candidate(Candidate, Best0, Best1)
		),
		select_betas(Betas, Method, Alpha, Series, Policy, Best1, Best).

	:- private(fit_paired_series/4).
	:- mode(fit_paired_series(@list, +compound, -compound, -compound), one_or_error).
	:- info(fit_paired_series/4, [
		comment is 'Fits SBA and Croston with shared transitions and independent error totals and metrics.',
		argnames is ['Series', 'Parameters', 'SBA', 'Croston'],
		exceptions is ['Transition or scoring arithmetic raises an evaluation error' - evaluation_error('Error')]
	]).

	fit_paired_series(Series, Parameters, fit(sba,Parameters,State,SBATotals,SBAMAE,SBARMSE), fit(croston,Parameters,State,CrostonTotals,CrostonMAE,CrostonRMSE)) :-
		learn_pair(Series, Parameters,
			pair_state(pending_state(0),forecast_error_totals(0,0,0),forecast_error_totals(0,0,0)),
			pair_state(State,SBATotals,CrostonTotals)),
		^^forecast_error_metrics(SBATotals, SBAMAE, SBARMSE),
		^^forecast_error_metrics(CrostonTotals, CrostonMAE, CrostonRMSE).

	:- private(learn_pair/4).
	:- mode(learn_pair(@list, +compound, +compound, -compound), one_or_error).
	:- info(learn_pair/4, [
		comment is 'Advances one shared Croston state and two causal error streams.',
		argnames is ['Series', 'Parameters', 'Accumulator', 'Result'],
		exceptions is ['Transition or scoring arithmetic raises an evaluation error' - evaluation_error('Error')]
	]).

	learn_pair([], _Parameters, Accumulator, Accumulator).
	learn_pair([Observation| Observations], Parameters, pair_state(State0,SBA0,Croston0), Result) :-
		(	var(Observation) ->
			SBA1 = SBA0,
			Croston1 = Croston0
		;	paired_predictions(State0, Parameters, SBAPrediction, CrostonPrediction),
			^^accumulate_forecast_error(Observation, SBAPrediction, SBA0, SBA1),
			^^accumulate_forecast_error(Observation, CrostonPrediction, Croston0, Croston1)
		),
		transition(croston, Parameters, State0, Observation, State1),
		learn_pair(Observations, Parameters, pair_state(State1,SBA1,Croston1), Result).

	:- private(paired_predictions/4).
	:- mode(paired_predictions(+compound, +compound, -number, -number), one_or_error).
	:- info(paired_predictions/4, [
		comment is 'Computes both method predictions from one rate, retaining integer cold-start zeros.',
		argnames is ['State', 'Parameters', 'SBA', 'Croston'],
		exceptions is ['Prediction arithmetic raises an evaluation error' - evaluation_error('Error')]
	]).

	paired_predictions(pending_state(_), _Parameters, 0, 0) :-
		!.
	paired_predictions(croston_state(Size,Interval,_), parameters(_,_,_,Correction), SBA, Croston) :-
		Croston is Size / Interval,
		SBA is Correction * Croston.

	:- private(better_candidate/3).
	:- mode(better_candidate(+compound, +term, -compound), one).
	:- info(better_candidate/3, [
		comment is 'Chooses the lower rounded RMSE, breaking exact ties by method, alpha, and beta priority.',
		argnames is ['Candidate', 'PreviousBest', 'Best']
	]).

	better_candidate(Candidate, none, Candidate) :-
		!.
	better_candidate(Candidate, Previous, Best) :-
		candidate_key(Candidate, Key),
		candidate_key(Previous, PreviousKey),
		(	Key @< PreviousKey ->
			Best = Candidate
		;	Best = Previous
		).

	:- private(candidate_key/2).
	:- mode(candidate_key(+compound, -compound), one).
	:- info(candidate_key/2, [
		comment is 'Returns the exact RMSE and deterministic method/coefficient priority key.',
		argnames is ['Candidate', 'Key']
	]).

	candidate_key(fit(Method,parameters(weight(Alpha,_),weight(Beta,_),_,_),_,_,_,RMSE), key(RMSE,Priority,Alpha,Beta)) :-
		method_priority(Method, Priority).

	:- private(method_priority/2).
	:- mode(method_priority(+atom, -integer), one).
	:- info(method_priority/2, [
		comment is 'Returns the priority of a validated concrete method.',
		argnames is ['Method', 'Priority']
	]).

	method_priority(sba, 0).
	method_priority(croston, 1).
	method_priority(tsb, 2).

	:- private(fit_series/4).
	:- mode(fit_series(@list, +atom, +compound, -compound), one_or_error).
	:- info(fit_series/4, [
		comment is 'Fits a validated series, retaining terminal state and error metrics without constructing public diagnostics.',
		argnames is ['Series', 'Method', 'Parameters', 'Fit'],
		exceptions is ['Fitting or metric arithmetic raises an evaluation error' - evaluation_error('Error')]
	]).

	fit_series(Series, Method, Parameters, Fit) :-
		fit_series_output(Series, Method, Parameters, discard, Fit).

	:- private(fit_series_output/5).
	:- mode(fit_series_output(@list, +atom, +compound, +term, -compound), one_or_error).
	:- info(fit_series_output/5, [
		comment is 'Fits and scores a validated series, optionally emitting pre-observation predictions in the same traversal.',
		argnames is ['Series', 'Method', 'Parameters', 'Output', 'Fit'],
		exceptions is ['Fitting or metric arithmetic raises an evaluation error' - evaluation_error('Error')]
	]).

	fit_series_output(Series, Method, Parameters, Output, fit(Method,Parameters,State,Totals,MAE,RMSE)) :-
		learn_state(
			Series, Method, Parameters,
			learning_state(pending_state(0), forecast_error_totals(0,0,0)),
			learning_state(State, Totals), Output
		),
		^^forecast_error_metrics(Totals, MAE, RMSE).

	:- private(fitted_model/3).
	:- mode(fitted_model(+compound, +compound, -compound), one_or_error).
	:- info(fitted_model/3, [
		comment is 'Builds canonical public diagnostics once from the winning fit and validated observation summary.',
		argnames is ['Summary', 'Fit', 'Forecaster'],
		exceptions is ['Counter arithmetic raises an evaluation error' - evaluation_error('Error')]
	]).

	fitted_model(summary(Length, Count, Positive), fit(Method,Parameters,State,Totals,MAE,RMSE), intermittent_demand_forecaster(Method,State,Diagnostics)) :-
		Parameters = parameters(weight(Alpha,_),weight(Beta,_),Policy,_),
		Options = [model(Method),alpha(Alpha),beta(Beta),missing(Policy)],
		Totals = forecast_error_totals(Count, Absolute, Squared),
		Missing is Length - Count,
		effective_periods(Policy, Length, Count, Effective),
		^^base_forecaster_diagnostics(intermittent_demand_forecasting, Length, Options, [
			method(Method), observed_count(Count), missing_count(Missing), positive_count(Positive),
			effective_period_count(Effective), update_count(0), scored_count(Count),
			sum_absolute_error(Absolute), sum_squared_error(Squared),
			mean_absolute_error(MAE), root_mean_squared_error(RMSE)
		], Diagnostics).

	:- private(learn_state/6).
	:- mode(learn_state(@list, +atom, +compound, +compound, -compound, +term), one_or_error).
	:- info(learn_state/6, [
		comment is 'Advances causal state and error totals with optional aligned prediction output.',
		argnames is ['Series', 'Method', 'Parameters', 'Accumulator', 'Result', 'Output'],
		exceptions is ['Prediction, transition, or scoring arithmetic raises an evaluation error' - evaluation_error('Error')]
	]).

	learn_state([], _Method, _Parameters, Accumulator, Accumulator, Output) :-
		(	Output == discard ->
			true
		;	Output = values([])
		).
	learn_state([Observation| Observations], Method, Parameters, learning_state(State0, Totals0), Accumulator, Output) :-
		score_observation(Method, State0, Parameters, Observation, Totals0, Totals1, Prediction),
		transition(Method, Parameters, State0, Observation, State1),
		(	Output == discard ->
			NextOutput = discard
		;	Output = values([Prediction| Predictions]),
			NextOutput = values(Predictions)
		),
		learn_state(Observations, Method, Parameters, learning_state(State1, Totals1), Accumulator, NextOutput).

	:- private(score_observation/7).
	:- mode(score_observation(+atom, +compound, +compound, @term, +compound, -compound, -term), one_or_error).
	:- info(score_observation/7, [
		comment is 'Scores a validated numeric target or preserves totals and a fresh prediction placeholder for a missing target.',
		argnames is ['Method', 'State', 'Parameters', 'Observation', 'PreviousTotals', 'Totals', 'Prediction'],
		exceptions is ['Prediction or scoring arithmetic raises an evaluation error' - evaluation_error('Error')]
	]).

	score_observation(Method, State, Parameters, Observation, Totals0, Totals, Prediction) :-
		(	var(Observation) ->
			Totals = Totals0
		;	forecast_value(Method, State, Parameters, Prediction),
			^^accumulate_forecast_error(Observation, Prediction, Totals0, Totals)
		).

	check_demand(Observation) :-
		^^check_observation(Observation),
		(	var(Observation) ->
			true
		;	Observation >= 0 ->
			true
		;	domain_error(non_negative_number, Observation)
		).

	:- private(demand_summary/3).
	:- mode(demand_summary(@list, +compound, -compound), one_or_error).
	:- info(demand_summary/3, [
		comment is 'Validates demands and summarizes elapsed, observed, and positive counts without binding missing values.',
		argnames is ['Series', 'Accumulator', 'Summary'],
		exceptions is [
			'An observation is not a number or variable' - type_error(number, 'Observation'),
			'An observation is negative' - domain_error(non_negative_number, 'Observation'),
			'Counter arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	demand_summary([], Summary, Summary).
	demand_summary([Observation| Observations], summary(Length0, Count0, Positive0), Summary) :-
		check_demand(Observation),
		Length is Length0 + 1,
		(	var(Observation) ->
			Count = Count0
		;	Count is Count0 + 1
		),
		positive_count(Observation, Positive0, Positive),
		demand_summary(Observations, summary(Length, Count, Positive), Summary).

	:- private(prepare_parameters/4).
	:- mode(prepare_parameters(+number, +number, +atom, -compound), one_or_error).
	:- info(prepare_parameters/4, [
		comment is 'Prepares validated coefficients, their complements, and the SBA correction for a fitting pass or public operation.',
		argnames is ['Alpha', 'Beta', 'Policy', 'Parameters'],
		exceptions is [
			'Coefficient arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	prepare_parameters(Alpha, Beta, Policy, parameters(weight(Alpha,AlphaComplement),weight(Beta,BetaComplement),Policy,Correction)) :-
		AlphaComplement is 1 - Alpha,
		BetaComplement is 1 - Beta,
		Correction is 1 - Beta / 2.

	positive_count(Observation, Positive0, Positive) :-
		(	var(Observation) ->
			Positive = Positive0
		;	Observation > 0 ->
			Positive is Positive0 + 1
		;	Positive = Positive0
		).

	effective_periods(skip, _Length, Count, Count).
	effective_periods(elapsed, Length, _Count, Length).

	transition(Method, parameters(Alpha, Beta, Policy, _), State, Observation, Updated) :-
		(	var(Observation) ->
			missing_state(Policy, State, Updated)
		;	numeric_state(Method, Alpha, Beta, State, Observation, Updated)
		).

	missing_state(skip, State, State).
	missing_state(elapsed, pending_state(Clock), pending_state(Next)) :-
		!,
		Next is Clock + 1.
	missing_state(elapsed, croston_state(Size, Interval, Age), croston_state(Size, Interval, Next)) :-
		!,
		Next is Age + 1.
	missing_state(elapsed, tsb_state(Size, Probability), tsb_state(Size, Probability)).

	numeric_state(Method, _Alpha, _Beta, pending_state(Clock), Observation, Updated) :-
		!,
		Period is Clock + 1,
		(	Observation > 0 ->
			initialized_state(Method, Observation, Period, Updated)
		;	Updated = pending_state(Period)
		).
	numeric_state(_Method, Alpha, Beta, croston_state(Size, Interval, Age), Observation, Updated) :-
		!,
		Gap is Age + 1,
		(	Observation > 0 ->
			positive_smoothing(Alpha, Observation, Size, NewSize),
			positive_smoothing(Beta, Gap, Interval, NewInterval),
			Updated = croston_state(NewSize, NewInterval, 0)
		;	Updated = croston_state(Size, Interval, Gap)
		).
	numeric_state(tsb, Alpha, weight(Beta,Complement), tsb_state(Size, Probability), Observation, tsb_state(NewSize, NewProbability)) :-
		(	Observation > 0 ->
			positive_smoothing(Alpha, Observation, Size, NewSize),
			NewProbability is Beta + Complement * Probability
		;	NewSize = Size,
			NewProbability is Complement * Probability
		).

	:- private(positive_smoothing/4).
	:- mode(positive_smoothing(+compound, +number, +number, -number), one_or_error).
	:- info(positive_smoothing/4, [
		comment is 'Smooths positive endpoints using their minimum plus a weighted difference, avoiding independent product underflow.',
		argnames is ['Weight', 'Observation', 'Previous', 'Smoothed'],
		exceptions is [
			'Smoothing arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	positive_smoothing(weight(Coefficient,Complement), Observation, Previous, Smoothed) :-
		(	Observation >= Previous ->
			Smoothed is Previous + Coefficient * (Observation - Previous)
		;	Smoothed is Observation + Complement * (Previous - Observation)
		).

	initialized_state(tsb, Size, Period, tsb_state(Size, Probability)) :-
		!,
		Probability is 1 / Period.
	initialized_state(_Method, Size, Period, croston_state(Size, Period, 0)).

	forecast(Forecaster, Horizon, Forecasts) :-
		checked_forecaster(Forecaster, Data),
		^^check_forecast_horizon(Horizon),
		(	Horizon =:= 0 ->
			Forecasts = []
		;	Forecaster = intermittent_demand_forecaster(Method, State, _Diagnostics),
			arg(3, Data, [model(Method),alpha(Alpha),beta(Beta),missing(Policy)]),
			prepare_parameters(Alpha, Beta, Policy, Parameters),
			forecast_value(Method, State, Parameters, Value),
			^^constant_forecast(Value, Horizon, Forecasts)
		).

	forecast_value(_Method, pending_state(_), _Parameters, 0) :-
		!.
	forecast_value(croston, croston_state(Size, Interval, _), _Parameters, Value) :-
		Value is Size / Interval.
	forecast_value(sba, croston_state(Size, Interval, _), parameters(_,_,_,Correction), Value) :-
		Value is Correction * (Size / Interval).
	forecast_value(tsb, tsb_state(Size, Probability), _Parameters, Value) :-
		Value is Size * Probability.

	update(Forecaster, Observation, intermittent_demand_forecaster(Method, State, UpdatedDiagnostics)) :-
		checked_forecaster(Forecaster, Data),
		check_demand(Observation),
		Forecaster = intermittent_demand_forecaster(Method, OriginalState, Diagnostics),
		Data = diagnostic_values(Model,Length0,Options,Method,Count0,Missing0,Positive0,_,Updates0,Count0,Absolute0,Squared0,MAE0,RMSE0),
		Options = [model(Method),alpha(Alpha),beta(Beta),missing(Policy)],
		prepare_parameters(Alpha, Beta, Policy, Parameters),
		score_observation(Method, OriginalState, Parameters, Observation, forecast_error_totals(Count0,Absolute0,Squared0), Totals, _),
		Totals = forecast_error_totals(Count,Absolute,Squared),
		transition(Method, Parameters, OriginalState, Observation, State),
		Length is Length0 + 1,
		Updates is Updates0 + 1,
		(	var(Observation) ->
			Missing is Missing0 + 1,
			MAE = MAE0,
			RMSE = RMSE0
		;	Missing = Missing0,
			^^forecast_error_metrics(Totals, MAE, RMSE)
		),
		positive_count(Observation, Positive0, Positive),
		effective_periods(Policy, Length, Count, Effective),
		UpdatedData = diagnostic_values(Model,Length,Options,Method,Count,Missing,Positive,Effective,Updates,Count,Absolute,Squared,MAE,RMSE),
		rewrite_diagnostics(Diagnostics, UpdatedData, 0, UpdatedDiagnostics).

	check_forecaster(Forecaster) :-
		checked_forecaster(Forecaster, _).

	:- private(checked_forecaster/2).
	:- mode(checked_forecaster(@term, -compound), one_or_error).
	:- info(checked_forecaster/2, [
		comment is 'Validates a model and returns its extracted fields for reuse by public operations.',
		argnames is ['Forecaster', 'Data'],
		exceptions is [
			'``Forecaster`` is a variable' - instantiation_error,
			'``Forecaster`` is invalid' - domain_error(forecaster, 'Forecaster'),
			'Validation arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	checked_forecaster(Forecaster, Data) :-
		(	var(Forecaster) ->
			instantiation_error
		;	forecaster_data(Forecaster, Method, State, Diagnostics),
			valid_diagnostics(Method, State, Diagnostics, Data) ->
			true
		;	domain_error(forecaster, Forecaster)
		).

	:- private(valid_diagnostics/4).
	:- mode(valid_diagnostics(+atom, +compound, +list, -compound), zero_or_one_or_error).
	:- info(valid_diagnostics/4, [
		comment is 'Extracts required fields once and validates options, counters, metrics, and state.',
		argnames is ['Method', 'State', 'Diagnostics', 'Data'],
		exceptions is ['Validation arithmetic raises an evaluation error' - evaluation_error('Error')]
	]).

	valid_diagnostics(Method, State, Diagnostics, Data) :-
		functor(Data, diagnostic_values, 14),
		extract_diagnostics(Diagnostics, Data),
		Data = diagnostic_values(intermittent_demand_forecasting,Length,Options,Method,Count,Missing,Positive,Effective,Updates,Scored,Absolute,Squared,MAE,RMSE),
		Options = [model(Method), alpha(Alpha), beta(Beta), missing(Policy)],
		concrete_method(Method),
		valid_coefficient(Alpha), valid_coefficient(Beta), valid_option(missing(Policy)),
		valid(positive_integer, Length),
		valid(positive_integer, Count),
		valid(non_negative_integer, Missing),
		Count + Missing =:= Length,
		valid(non_negative_integer, Positive), Positive =< Count,
		effective_periods(Policy, Length, Count, Expected), Effective == Expected,
		valid(non_negative_integer, Updates), Updates < Length,
		Scored == Count,
		valid_error_metrics(Count, Absolute, Squared, MAE, RMSE),
		valid_state(Method, State, Positive, Effective).

	:- private(extract_diagnostics/2).
	:- mode(extract_diagnostics(+list, +compound), zero_or_one).
	:- info(extract_diagnostics/2, [
		comment is 'Extracts ground diagnostic fields, rejecting malformed arities and duplicate error metrics.',
		argnames is ['Diagnostics', 'Data']
	]).

	extract_diagnostics([], Data) :-
		ground(Data).
	extract_diagnostics([Diagnostic| Diagnostics], Data) :-
		compound(Diagnostic),
		functor(Diagnostic, Name, Arity),
		(	diagnostic_position(Name, Position) ->
			Arity =:= 1,
			arg(1, Diagnostic, Value),
			arg(Position, Data, Field),
			(	var(Field) ->
				Field = Value
			;	Position < 10
			)
		;	true
		),
		extract_diagnostics(Diagnostics, Data).

	:- private(rewrite_diagnostics/4).
	:- mode(rewrite_diagnostics(+list, +compound, +non_negative_integer, -list), one).
	:- info(rewrite_diagnostics/4, [
		comment is 'Rebuilds diagnostic values in one traversal, preserving order, extra metadata, and later common-field occurrences.',
		argnames is ['Diagnostics', 'Data', 'Seen', 'UpdatedDiagnostics']
	]).

	rewrite_diagnostics([], _Data, _Seen, []).
	rewrite_diagnostics([Diagnostic| Diagnostics], Data, Seen, [Updated| Rest]) :-
		functor(Diagnostic, Name, _),
		(	diagnostic_position(Name, Position) ->
			Bit is 1 << (Position - 1),
			(	Seen /\ Bit =:= 0 ->
				arg(Position, Data, Value),
				Updated =.. [Name,Value],
				NextSeen is Seen \/ Bit
			;	Updated = Diagnostic,
				NextSeen = Seen
			)
		;	Updated = Diagnostic,
			NextSeen = Seen
		),
		rewrite_diagnostics(Diagnostics, Data, NextSeen, Rest).

	:- private(diagnostic_position/2).
	:- mode(diagnostic_position(+atom, -positive_integer), zero_or_one).
	:- info(diagnostic_position/2, [
		comment is 'Maps required diagnostic names to their compact field positions.',
		argnames is ['Name', 'Position']
	]).

	diagnostic_position(model, 1).
	diagnostic_position(training_series_length, 2).
	diagnostic_position(options, 3).
	diagnostic_position(method, 4).
	diagnostic_position(observed_count, 5).
	diagnostic_position(missing_count, 6).
	diagnostic_position(positive_count, 7).
	diagnostic_position(effective_period_count, 8).
	diagnostic_position(update_count, 9).
	diagnostic_position(scored_count, 10).
	diagnostic_position(sum_absolute_error, 11).
	diagnostic_position(sum_squared_error, 12).
	diagnostic_position(mean_absolute_error, 13).
	diagnostic_position(root_mean_squared_error, 14).

	:- private(valid_error_metrics/5).
	:- mode(valid_error_metrics(+positive_integer, @term, @term, @term, @term), zero_or_one_or_error).
	:- info(valid_error_metrics/5, [
		comment is 'Checks numeric error totals and exact consistency of their derived metrics.',
		argnames is ['Count', 'Absolute', 'Squared', 'MAE', 'RMSE'],
		exceptions is ['Metric arithmetic raises an evaluation error' - evaluation_error('Error')]
	]).

	valid_error_metrics(Count, Absolute, Squared, MAE, RMSE) :-
		number(Absolute), Absolute >= 0,
		number(Squared), Squared >= 0,
		number(MAE), MAE >= 0,
		number(RMSE), RMSE >= 0,
		^^forecast_error_metrics(forecast_error_totals(Count,Absolute,Squared), ExpectedMAE, ExpectedRMSE),
		MAE =:= ExpectedMAE,
		RMSE =:= ExpectedRMSE.

	valid_state(_Method, pending_state(Clock), Positive, Effective) :-
		!,
		Positive =:= 0, Clock == Effective.
	valid_state(Method, croston_state(Size, Interval, Age), Positive, Effective) :-
		!,
		Method \== tsb,
		Positive > 0,
		number(Size), Size > 0,
		number(Interval), Interval >= 1, Interval =< Effective,
		valid(non_negative_integer, Age), Age < Effective.
	valid_state(tsb, tsb_state(Size, Probability), Positive, _Effective) :-
		Positive > 0,
		number(Size), Size > 0,
		number(Probability), Probability >= 0, Probability =< 1.

	forecaster_data(Forecaster, Method, State, Diagnostics) :-
		Forecaster =.. [_, Method, State, Diagnostics].

	forecaster_term_template(Forecaster, Template) :-
		functor(Forecaster, Functor, 3),
		Template =.. [Functor, 'Method', 'State', 'Diagnostics'].

	forecaster_export_template(_Dataset, _Forecaster, Functor, Template) :-
		Template =.. [Functor, 'Forecaster'].

	export_to_clauses(_Dataset, Forecaster, Functor, [Clause]) :-
		check_forecaster(Forecaster),
		context(Context),
		check(atom, Functor, Context),
		Clause =.. [Functor, Forecaster].

	print_forecaster(Forecaster) :-
		check_forecaster(Forecaster),
		^^print_forecaster_template(Forecaster),
		forecaster_data(Forecaster, Method, State, Diagnostics),
		format('Method: ~w~nState: ~q~nDiagnostics: ~q~n', [Method, State, Diagnostics]).

	default_option(model(sba)).
	default_option(alpha(0.1)).
	default_option(beta(0.1)).
	default_option(missing(skip)).
	default_option(coefficient_grid([0.1,0.2,0.5,0.8,1.0])).

	valid_option(model(Method)) :-
		(	Method == auto ->
			true
		;	concrete_method(Method)
		).
	valid_option(alpha(Value)) :-
		(	Value == auto ->
			true
		;	valid_coefficient(Value)
		).
	valid_option(beta(Value)) :-
		(	Value == auto ->
			true
		;	valid_coefficient(Value)
		).
	valid_option(missing(Policy)) :-
		once((Policy == skip; Policy == elapsed)).
	valid_option(coefficient_grid(Grid)) :-
		valid(non_empty_list, Grid),
		valid_grid_values(Grid).

	valid_grid_values([]).
	valid_grid_values([Value| Values]) :-
		valid_coefficient(Value),
		valid_grid_values(Values).

	concrete_method(Method) :-
		once((Method == croston; Method == sba; Method == tsb)).

	valid_coefficient(Value) :-
		number(Value), Value > 0, Value =< 1.

:- end_object.
