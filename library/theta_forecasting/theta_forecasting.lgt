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


:- object(theta_forecasting,
	implements(forecaster_protocol),
	imports([forecaster_common, exponential_smoothing_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Standard Theta forecasting combining simple exponential smoothing and linear trend extrapolation.'
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(numberlist, [
		linear_regression/4
	]).

	:- uses(type, [
		valid/2, check/3
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one_or_error).
	:- info(learn/3, [
		comment is 'Learns a standard Theta forecaster using the given options.',
		argnames is ['Dataset', 'Forecaster', 'Options'],
		exceptions is [
			'An option, length, or strict-policy observation is unbound' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'An observation is not numeric' - type_error(number, 'Observation'),
			'The declared length is not an integer' - type_error(integer, 'DeclaredLength'),
			'The declared length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The dataset has no observations' - domain_error(non_empty_series, 'Dataset'),
			'Indices are not gap-free and 1-based' - domain_error(series_index_sequence, 'Dataset'),
			'Declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'There are fewer than two observations' - domain_error(series_length, 'Dataset'),
			'There are fewer than two known observations' - domain_error(insufficient_known_observations, 'Dataset'),
			'The frequency is not an integer' - type_error(integer, 'Frequency'),
			'The frequency is not positive' - domain_error(positive_integer, 'Frequency'),
			'Forced seasonality needs a frequency greater than one' - domain_error(seasonal_frequency, 'Frequency'),
			'Forced seasonality has no dataset frequency' - domain_error(seasonal_frequency, 'Dataset'),
			'An explicit frequency accompanies no seasonality' - domain_error(theta_forecasting_option, 'Option'),
			'The series has fewer than two seasonal cycles' - domain_error(series_length, 'Series'),
			'A seasonal phase has no valid centered-window deviation' - domain_error(insufficient_seasonal_phase_observations, 'Phase'),
			'Multiplicative data or factors are not positive' - domain_error(positive_number, 'Value'),
			'Fitting arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- public(learn/2).
	:- mode(learn(+object_identifier, -compound), one_or_error).
	:- info(learn/2, [
		comment is 'Learns a standard Theta forecaster using default options.',
		argnames is ['Dataset', 'Forecaster'],
		exceptions is [
			'A length or frequency is unbound' - instantiation_error,
			'An observation is not numeric' - type_error(number, 'Observation'),
			'The declared length is not an integer' - type_error(integer, 'DeclaredLength'),
			'The declared length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The dataset has no observations' - domain_error(non_empty_series, 'Dataset'),
			'Indices are not gap-free and 1-based' - domain_error(series_index_sequence, 'Dataset'),
			'Declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'There are fewer than two observations' - domain_error(series_length, 'Dataset'),
			'There are fewer than two known observations' - domain_error(insufficient_known_observations, 'Dataset'),
			'The dataset frequency is not an integer' - type_error(integer, 'Frequency'),
			'The dataset frequency is not positive' - domain_error(positive_integer, 'Frequency'),
			'Seasonal decomposition has fewer than two cycles' - domain_error(series_length, 'Series'),
			'A seasonal phase has no valid centered-window deviation' - domain_error(insufficient_seasonal_phase_observations, 'Phase'),
			'Multiplicative factors are not positive' - domain_error(positive_number, 'Value'),
			'Fitting arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- public(fitted_values/2).
	:- mode(fitted_values(+object_identifier, -list), one_or_error).
	:- info(fitted_values/2, [
		comment is 'Returns aligned SES training fits on the original scale using default options. Missing targets and the first known initialization anchor have independent unbound placeholders. These are not prefix-wise Theta forecasts.',
		argnames is ['Dataset', 'Values'],
		exceptions is [
			'A length or frequency is unbound' - instantiation_error,
			'An observation is not numeric' - type_error(number, 'Observation'),
			'The declared length is not an integer' - type_error(integer, 'DeclaredLength'),
			'The declared length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The dataset has no observations' - domain_error(non_empty_series, 'Dataset'),
			'Indices are not gap-free and 1-based' - domain_error(series_index_sequence, 'Dataset'),
			'Declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'There are fewer than two observations' - domain_error(series_length, 'Dataset'),
			'There are fewer than two known observations' - domain_error(insufficient_known_observations, 'Dataset'),
			'The dataset frequency is not an integer' - type_error(integer, 'Frequency'),
			'The dataset frequency is not positive' - domain_error(positive_integer, 'Frequency'),
			'Seasonal decomposition has fewer than two cycles' - domain_error(series_length, 'Series'),
			'A seasonal phase has no valid centered-window deviation' - domain_error(insufficient_seasonal_phase_observations, 'Phase'),
			'Multiplicative factors are not positive' - domain_error(positive_number, 'Value'),
			'The residual count is inconsistent' - domain_error(residual_count, 'Residuals'),
			'Fitting or fit restoration arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- public(fitted_values/3).
	:- mode(fitted_values(+object_identifier, -list, +list(compound)), one_or_error).
	:- info(fitted_values/3, [
		comment is 'Returns aligned SES training fits on the original scale using the learning options. Missing targets and the first known initialization anchor have independent unbound placeholders. Parameters and seasonal factors are fitted once using the full sample, not individual prefixes.',
		argnames is ['Dataset', 'Values', 'Options'],
		exceptions is [
			'An option, length, or strict-policy observation is unbound' - instantiation_error,
			'Options are not a list' - type_error(list, 'Options'),
			'An option is not a compound term' - type_error(compound, 'Option'),
			'An option is invalid' - domain_error(option, 'Option'),
			'An observation is not numeric' - type_error(number, 'Observation'),
			'The declared length is not an integer' - type_error(integer, 'DeclaredLength'),
			'The declared length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The dataset has no observations' - domain_error(non_empty_series, 'Dataset'),
			'Indices are not gap-free and 1-based' - domain_error(series_index_sequence, 'Dataset'),
			'Declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'There are fewer than two observations' - domain_error(series_length, 'Dataset'),
			'There are fewer than two known observations' - domain_error(insufficient_known_observations, 'Dataset'),
			'The frequency is not an integer' - type_error(integer, 'Frequency'),
			'The frequency is not positive' - domain_error(positive_integer, 'Frequency'),
			'Forced seasonality needs a frequency greater than one' - domain_error(seasonal_frequency, 'Frequency'),
			'Forced seasonality has no dataset frequency' - domain_error(seasonal_frequency, 'Dataset'),
			'An explicit frequency accompanies no seasonality' - domain_error(theta_forecasting_option, 'Option'),
			'The series has fewer than two seasonal cycles' - domain_error(series_length, 'Series'),
			'A seasonal phase has no valid centered-window deviation' - domain_error(insufficient_seasonal_phase_observations, 'Phase'),
			'Multiplicative data or factors are not positive' - domain_error(positive_number, 'Value'),
			'The residual count is inconsistent' - domain_error(residual_count, 'Residuals'),
			'Fitting or fit restoration arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- public(forecast/3).
	:- mode(forecast(+compound, +non_negative_integer, -list(number)), one_or_error).
	:- info(forecast/3, [
		comment is 'Forecasts the next ``Horizon`` observations. A zero horizon returns an empty list after validation.',
		argnames is ['Forecaster', 'Horizon', 'Forecasts'],
		exceptions is [
			'Forecaster or horizon is unbound' - instantiation_error,
			'The forecaster is invalid' - domain_error(forecaster, 'Forecaster'),
			'The horizon is not an integer' - type_error(integer, 'Horizon'),
			'The horizon is negative' - domain_error(non_negative_integer, 'Horizon'),
			'Forecast or validation arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- public(check_forecaster/1).
	:- mode(check_forecaster(@compound), one_or_error).
	:- info(check_forecaster/1, [
		comment is 'Checks the complete state and diagnostics without binding the argument.',
		argnames is ['Forecaster'],
		exceptions is [
			'The forecaster is unbound' - instantiation_error,
			'The forecaster is invalid' - domain_error(forecaster, 'Forecaster'),
			'Validation arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- public(export_to_clauses/4).
	:- mode(export_to_clauses(+object_identifier, +compound, +atom, -list(clause)), one_or_error).
	:- info(export_to_clauses/4, [
		comment is 'Exports a validated forecaster as one fact.',
		argnames is ['Dataset', 'Forecaster', 'Functor', 'Clauses'],
		exceptions is [
			'Forecaster or functor is unbound' - instantiation_error,
			'The forecaster is invalid' - domain_error(forecaster, 'Forecaster'),
			'The functor is not an atom' - type_error(atom, 'Functor'),
			'Validation arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- public(print_forecaster/1).
	:- mode(print_forecaster(+compound), one_or_error).
	:- info(print_forecaster/1, [
		comment is 'Prints a validated forecaster and its diagnostics.',
		argnames is ['Forecaster'],
		exceptions is [
			'The forecaster is unbound' - instantiation_error,
			'The forecaster is invalid' - domain_error(forecaster, 'Forecaster'),
			'Validation arithmetic fails' - evaluation_error('Error'),
			'The current stream cannot be written' - permission_error(output, stream, 'Stream')
		]
	]).

	:- public(export_to_file/4).
	:- mode(export_to_file(+object_identifier, +compound, +atom, +atom), one_or_error).
	:- info(export_to_file/4, [
		comment is 'Exports a validated forecaster with dataset and diagnostics comments.',
		argnames is ['Dataset', 'Forecaster', 'Functor', 'File'],
		exceptions is [
			'A forecaster, functor, file, or dataset length is unbound' - instantiation_error,
			'The forecaster is invalid' - domain_error(forecaster, 'Forecaster'),
			'The functor is not an atom' - type_error(atom, 'Functor'),
			'The file specification is invalid' - type_error(source_sink, 'File'),
			'The output location does not exist' - existence_error(source_sink, 'File'),
			'The file cannot be opened' - permission_error(open, source_sink, 'File'),
			'The stream cannot be written' - permission_error(output, stream, 'Stream'),
			'The dataset has no observations' - domain_error(non_empty_series, 'Dataset'),
			'Indices are not gap-free and 1-based' - domain_error(series_index_sequence, 'Dataset'),
			'The declared length is not an integer' - type_error(integer, 'DeclaredLength'),
			'The declared length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'Declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'Validation arithmetic fails' - evaluation_error('Error')
		]
	]).

	learn(Dataset, Forecaster) :-
		learn(Dataset, Forecaster, []).

	learn(Dataset, Forecaster, UserOptions) :-
		prepare_learning(Dataset, Forecaster, _FitData, UserOptions).

	fitted_values(Dataset, Values) :-
		fitted_values(Dataset, Values, []).

	fitted_values(Dataset, Values, UserOptions) :-
		prepare_learning(Dataset, _Forecaster, theta_fit(Adjusted,Seasonality,Residuals), UserOptions),
		^^residual_fitted_values(Adjusted, Residuals, AdjustedFits),
		(	Seasonality == none ->
			Values = AdjustedFits
		;	Seasonality = seasonal(Method,_Frequency,_Phase,Factors),
			^^restore_seasonality(Method, Factors, 1, AdjustedFits, Values)
		).

	:- private(prepare_learning/4).
	:- mode(prepare_learning(+object_identifier, -compound, -compound, +list(compound)), one_or_error).
	:- info(prepare_learning/4, [
		comment is 'Collects and fits once, returning transient adjusted observations and residuals alongside the forecaster.',
		argnames is ['Dataset', 'Forecaster', 'FitData', 'Options'],
		exceptions is [
			'An option, length, or strict-policy observation is unbound' - instantiation_error,
			'Options are not a list' - type_error(list, 'Options'),
			'An option is not a compound term' - type_error(compound, 'Option'),
			'An option is invalid' - domain_error(option, 'Option'),
			'An observation is not numeric' - type_error(number, 'Observation'),
			'The declared length is not an integer' - type_error(integer, 'DeclaredLength'),
			'The declared length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The dataset has no observations' - domain_error(non_empty_series, 'Dataset'),
			'Indices are not gap-free and 1-based' - domain_error(series_index_sequence, 'Dataset'),
			'Declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'There are fewer than two observations' - domain_error(series_length, 'Dataset'),
			'There are fewer than two known observations' - domain_error(insufficient_known_observations, 'Dataset'),
			'The frequency is not an integer' - type_error(integer, 'Frequency'),
			'The frequency is not positive' - domain_error(positive_integer, 'Frequency'),
			'Forced seasonality needs a frequency greater than one' - domain_error(seasonal_frequency, 'Frequency'),
			'Forced seasonality has no dataset frequency' - domain_error(seasonal_frequency, 'Dataset'),
			'An explicit frequency accompanies no seasonality' - domain_error(theta_forecasting_option, 'Option'),
			'The series has fewer than two seasonal cycles' - domain_error(series_length, 'Series'),
			'A seasonal phase has no valid centered-window deviation' - domain_error(insufficient_seasonal_phase_observations, 'Phase'),
			'Multiplicative data or factors are not positive' - domain_error(positive_number, 'Value'),
			'Fitting arithmetic fails' - evaluation_error('Error')
		]
	]).

	prepare_learning(Dataset, theta_forecaster(theta_state(Level, Slope, Alpha, Correction, Seasonality), Diagnostics),
		theta_fit(Adjusted,Seasonality,Residuals), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(alpha(RequestedAlpha), Options),
		^^option(initialization(Initialization), Options),
		^^option(optimizer_options(OptimizerOptions), Options),
		^^option(missing_policy(Policy), Options),
		^^option(retain_residuals(RetainResiduals), Options),
		^^dataset_series(Dataset, Series),
		(	Policy == error ->
			^^check_series(Dataset, Series)
		;	true
		),
		^^series_observation_summary(Series, Length, Observed, _Sum),
		Missing is Length - Observed,
		^^check_series_length(Dataset, Series, 2),
		(	Observed >= 2 ->
			true
		;	domain_error(insufficient_known_observations, Dataset)
		),
		prepare_seasonality(Dataset, Series, UserOptions, Options, Adjusted, Seasonality, Mode, Frequency),
		^^indexed_series_observations(Adjusted, Indices, Known),
		Indices = [Anchor| ScoredIndices], anchor_series(Adjusted, FitSeries),
		linear_regression(Indices, Known, Slope, _Intercept),
		fit_level(
			fitting_series(FitSeries,Known), RequestedAlpha, Initialization, OptimizerOptions,
			Alpha, InitialLevel, Level, SSE, fit_errors(Residuals,Count), Fitting, Convergence, Iterations, Evaluations
		),
		theta_correction(Length, Alpha, Correction),
		training_totals(Series, Adjusted, Seasonality, training_errors(Residuals,Anchor), Totals, OriginalErrors),
		Totals = forecast_error_totals(Count, Absolute, Squared),
		Count =:= Observed - 1,
		^^forecast_error_metrics(Totals, MAE, RMSE),
		(	RetainResiduals == true ->
			RetainedErrors = OriginalErrors, RetainedIndices = ScoredIndices
		;	RetainedErrors = none, RetainedIndices = none
		),
		^^base_forecaster_diagnostics(theta, Length,
			[alpha(Alpha),initialization(Initialization),seasonal(Mode),frequency(Frequency),missing_policy(Policy),retain_residuals(RetainResiduals)], [
			observed_count(Observed), missing_count(Missing), update_count(0), initialization_index(Anchor),
			slope(Slope), seasonal_mode(Mode), frequency(Frequency), initial_level(InitialLevel),
			fitting(Fitting), optimizer_options(OptimizerOptions), convergence(Convergence),
			iterations(Iterations), evaluations(Evaluations), optimization_sum_squared_error(SSE),
			error_basis(ses_training_fit), scored_count(Count), sum_absolute_error(Absolute),
			sum_squared_error(Squared), mean_absolute_error(MAE), root_mean_squared_error(RMSE),
			residuals(RetainedErrors), residual_indices(RetainedIndices)
		], Diagnostics).

	:- private(prepare_seasonality/8).
	:- mode(prepare_seasonality(+object_identifier, +list, +list, +list, -list, -term, -atom, -integer), one_or_error).
	:- info(prepare_seasonality/8, [
		comment is 'Resolves the requested seasonal adjustment and dataset frequency.',
		argnames is ['Dataset', 'Series', 'UserOptions', 'Options', 'Adjusted', 'Seasonality', 'Mode', 'Frequency'],
		exceptions is [
			'The frequency is unbound' - instantiation_error,
			'The frequency is not an integer' - type_error(integer, 'Frequency'),
			'The frequency is not positive' - domain_error(positive_integer, 'Frequency'),
			'Forced seasonality has no dataset frequency' - domain_error(seasonal_frequency, 'Dataset'),
			'Forced seasonality has frequency one' - domain_error(seasonal_frequency, 'Frequency'),
			'A frequency option is supplied with no seasonality' - domain_error(theta_forecasting_option, 'Option'),
			'The series has fewer than two cycles' - domain_error(series_length, 'Series'),
			'A seasonal phase has no valid centered-window deviation' - domain_error(insufficient_seasonal_phase_observations, 'Phase'),
			'Multiplicative data or factors are not positive' - domain_error(positive_number, 'Value'),
			'Seasonal arithmetic fails' - evaluation_error('Error')
		]
	]).

	prepare_seasonality(Dataset, Series, UserOptions, Options, Adjusted, Seasonality, Mode, Frequency) :-
		^^option(seasonal(Request), Options),
		(	Request == none ->
			(	member(frequency(Value), UserOptions) ->
				domain_error(theta_forecasting_option, frequency(Value))
			;	true
			),
			Adjusted = Series, Seasonality = none, Mode = none, Frequency = 1
		;	^^option(frequency(FrequencyOption), Options),
			(	FrequencyOption == dataset ->
				(	Dataset::frequency(Period) ->
					true
				;	Request == auto ->
					Period = 1
				;	domain_error(seasonal_frequency, Dataset)
				)
			;	Period = FrequencyOption
			),
			^^check_frequency(Period),
			(	Request == auto ->
				^^seasonal_autocorrelation_test(Series, Period, _Statistic, Detected),
				(	Detected == false ->
					Mode = none
				;	positive_series(Series) ->
					Mode = multiplicative
				;	Mode = additive
				)
			;	Mode = Request
			),
			(	Mode == none ->
				Adjusted = Series, Seasonality = none, Frequency = 1
			;	^^classical_seasonal_adjustment(Series, Period, Mode, Adjusted, Factors),
				length(Series, Length), NextPhase is Length mod Period + 1,
				Frequency = Period, Seasonality = seasonal(Mode,Period,NextPhase,Factors)
			)
		).

	:- private(positive_series/1).
	:- mode(positive_series(+list), zero_or_one).
	:- info(positive_series/1, [
		comment is 'Checks whether all known numeric observations are strictly positive, ignoring unbound observations.',
		argnames is ['Series'],
		exceptions is [
			'Numeric comparison fails' - evaluation_error('Error')
		]
	]).

	positive_series([]).
	positive_series([Value| Values]) :-
		(	var(Value) ->
			true
		;	Value > 0
		),
		positive_series(Values).

	:- private(training_totals/6).
	:- mode(training_totals(+list, +list, +term, +compound, -compound, -list(number)), one_or_error).
	:- info(training_totals/6, [
		comment is 'Scores SES training fits after restoring the original seasonal scale and returns the same compact scoring errors.',
		argnames is ['Series', 'Adjusted', 'Seasonality', 'Errors', 'Totals', 'OriginalErrors'],
		exceptions is [
			'Training-error arithmetic fails' - evaluation_error('Error')
		]
	]).

	training_totals(_Series, _Adjusted, none, training_errors(Residuals,_Anchor), Totals, Residuals) :-
		!,
		residual_totals(Residuals, forecast_error_totals(0,0,0), Totals).
	training_totals(Series, Adjusted, seasonal(Mode,Frequency,_Phase,Factors), training_errors(Residuals,Anchor), Totals, OriginalErrors) :-
		anchor_series(Series, [_| Actual]), anchor_series(Adjusted, [_| Targets]),
		Skip is Anchor mod Frequency, length(Prefix, Skip), list::append(Prefix, Suffix, Factors),
		score_seasonal(Actual, Targets, Residuals, Mode, Suffix, Factors, forecast_error_totals(0,0,0), Totals, OriginalErrors).

	:- private(score_seasonal/9).
	:- mode(score_seasonal(+list, +list, +list(number), +atom, +list(number), +list(number), +compound, -compound, -list(number)), one_or_error).
	:- info(score_seasonal/9, [
		comment is 'Scores known targets on their original scale, advancing phases but not residuals at gaps.',
		argnames is [
			'Actuals', 'Adjusted', 'Residuals', 'Method', 'Factors', 'Cycle', 'Totals0', 'Totals',
			'OriginalErrors'
		],
		exceptions is [
			'Fit restoration or error accumulation fails' - evaluation_error('Error')
		]
	]).

	score_seasonal([], [], [], _, _, _, Totals, Totals, []) :-
		!.
	score_seasonal(Actual, Adjusted, Residuals, Mode, [], Cycle, Totals0, Totals, OriginalErrors) :-
		!,
		score_seasonal(Actual, Adjusted, Residuals, Mode, Cycle, Cycle, Totals0, Totals, OriginalErrors).
	score_seasonal([Actual| Actuals], [Value| Values], Residuals, Mode, [Factor| Factors], Cycle, Totals0, Totals, OriginalErrors) :-
		(	var(Actual) ->
			var(Value),
			Errors = Residuals,
			Totals1 = Totals0,
			OriginalErrors = RestErrors
		;	Residuals = [Error| Errors],
			Fit is Value - Error,
			(	Mode == additive ->
				Prediction is Fit + Factor
			;	Prediction is Fit * Factor
			),
			^^accumulate_forecast_error(Actual, Prediction, Totals0, Totals1),
			OriginalError is Actual - Prediction,
			OriginalErrors = [OriginalError| RestErrors]
		), score_seasonal(Actuals, Values, Errors, Mode, Factors, Cycle, Totals1, Totals, RestErrors).

	:- private(anchor_series/2).
	:- mode(anchor_series(+list, -list), one).
	:- info(anchor_series/2, [
		comment is 'Removes only leading missing positions before fitting the SES initialization anchor.',
		argnames is ['Series', 'FitSeries']
	]).

	anchor_series([Value| Values], FitSeries) :-
		(	var(Value) ->
			anchor_series(Values, FitSeries)
		;	FitSeries = [Value| Values]
		).

	:- private(fit_level/13).
	:- mode(fit_level(+compound, +term, +atom, +list, -number, -number, -number, -number, -compound, -atom, -atom, -integer, -integer), one_or_error).
	:- info(fit_level/13, [
		comment is 'Fits the SES level using the existing bounded optimization problem when needed.',
		argnames is [
			'Series', 'RequestedAlpha', 'Initialization', 'OptimizerOptions', 'Alpha', 'InitialLevel',
			'Level', 'SSE', 'Errors', 'Fitting', 'Convergence', 'Iterations', 'Evaluations'
		],
		exceptions is [
			'Optimizer options are invalid' - domain_error(option, 'Option'),
			'Fitting arithmetic fails' - evaluation_error('Error')
		]
	]).

	fit_level(fitting_series(Series,Known), RequestedAlpha, Initialization, OptimizerOptions,
		Alpha, InitialLevel, Level, SSE, fit_errors(Residuals,Count), Fitting, Convergence, Iterations, Evaluations) :-
		initialization_specification(Initialization, Specification),
		Parameters = [alpha(RequestedAlpha)],
		^^optimization_initial_point(simple, Known, none, Parameters, Specification, InitialPoint),
		(	InitialPoint == [] ->
			Alpha = RequestedAlpha, Effective = Specification,
			Fitting = fixed_parameters, Convergence = fixed_parameters, Iterations = 0, Evaluations = 0
		;	Problem = exponential_smoothing_problem(simple, Series, Known, none, Parameters, Specification),
			nelder_mead(Problem)::run(Point, _Objective, Statistics, [objective(minimize),updates(0)| OptimizerOptions]),
			^^optimization_components(simple, Known, none, Parameters, Specification, Point, [Alpha], Effective),
			Fitting = nelder_mead,
			memberchk(iterations(Iterations), Statistics), memberchk(evaluations(Evaluations), Statistics),
			maximum_iterations(OptimizerOptions, Maximum),
			(	Iterations >= Maximum ->
				Convergence = maximum_iterations
			;	Convergence = converged
			)
		),
		(	Initialization == first ->
			Series = [InitialLevel| _]
		;	Effective = initialization(optimized,2,level(InitialLevel))
		),
		^^fit_smoothing(simple, Series, none, [Alpha], Effective, level(Level), SSE, Count, Residuals).

	initialization_specification(first, initialization(two_cycles,2)).
	initialization_specification(optimized, initialization(optimized,2)).

	maximum_iterations(Options, Maximum) :-
		(	member(max_iterations(Maximum), Options) ->
			true
		;	nelder_mead(no_problem)::default_option(max_iterations(Maximum))
		).

	:- private(theta_correction/3).
	:- mode(theta_correction(+positive_integer, +number, -number), one_or_error).
	:- info(theta_correction/3, [
		comment is 'Computes the finite-sample correction, including the zero-alpha limit.',
		argnames is ['Length', 'Alpha', 'Correction'],
		exceptions is [
			'Correction arithmetic fails' - evaluation_error('Error')
		]
	]).

	theta_correction(Length, Alpha, Correction) :-
		RemainingWeight is 1.0 - Alpha,
		correction_steps(Length, RemainingWeight, 0.0, Correction).

	:- private(correction_steps/4).
	:- mode(correction_steps(+non_negative_integer, +number, +number, -number), one_or_error).
	:- info(correction_steps/4, [
		comment is 'Evaluates the geometric correction through a stable recurrence.',
		argnames is ['Remaining', 'Weight', 'Previous', 'Correction'],
		exceptions is [
			'Correction arithmetic fails' - evaluation_error('Error')
		]
	]).

	correction_steps(0, _, Correction, Correction) :-
		!.
	correction_steps(Length, Weight, Previous, Correction) :-
		Next is 1.0 + Weight * Previous,
		Remaining is Length - 1,
		correction_steps(Remaining, Weight, Next, Correction).

	:- private(residual_totals/3).
	:- mode(residual_totals(+list(number), +compound, -compound), one_or_error).
	:- info(residual_totals/3, [
		comment is 'Accumulates training residual error totals.',
		argnames is ['Residuals', 'Totals0', 'Totals'],
		exceptions is [
			'Error accumulation fails' - evaluation_error('Error')
		]
	]).

	residual_totals([], Totals, Totals).
	residual_totals([Error| Errors], Totals0, Totals) :-
		^^accumulate_forecast_error(Error, 0, Totals0, Totals1),
		residual_totals(Errors, Totals1, Totals).

	forecast(Forecaster, Horizon, Forecasts) :-
		check_forecaster(Forecaster),
		^^check_forecast_horizon(Horizon),
		(	Horizon =:= 0 ->
			Forecasts = []
		;	Forecaster = theta_forecaster(theta_state(Level, Slope, _Alpha, Correction, Seasonality), _),
			Drift is Slope / 2.0,
			Base is Level + Drift * (Correction - 1.0),
			^^linear_trend_forecast(Base, Drift, Horizon, Adjusted),
			(	Seasonality == none ->
				Forecasts = Adjusted
			;	Seasonality = seasonal(Mode,_Frequency,Phase,Factors),
				^^restore_seasonality(Mode, Factors, Phase, Adjusted, Forecasts)
			)
		).

	check_forecaster(Forecaster) :-
		(	var(Forecaster) ->
			instantiation_error
		; 	ground(Forecaster),
			valid_model(Forecaster) ->
			true
		;	domain_error(forecaster, Forecaster)
		).

	:- private(valid_model/1).
	:- mode(valid_model(+compound), zero_or_one).
	:- info(valid_model/1, [
		comment is 'Validates ground state and mandatory diagnostics.',
		argnames is ['Forecaster'],
		exceptions is [
			'Validation arithmetic fails' - evaluation_error('Error')
		]
	]).

	valid_model(theta_forecaster(theta_state(Level, Slope, Alpha, Correction, Seasonality), Diagnostics)) :-
		^^finite_number(Level),
		^^finite_number(Slope),
		valid_alpha(Alpha),
		^^finite_number(Correction),
		valid(list(compound), Diagnostics),
		unique_diagnostics(Diagnostics, []),
		^^valid_forecaster_metadata(theta, [alpha(Alpha),initialization(Initialization),seasonal(Mode),frequency(Frequency),missing_policy(Policy),retain_residuals(RetainResiduals)], Diagnostics),
		valid_option(initialization(Initialization)),
		valid_option(missing_policy(Policy)),
		valid_option(retain_residuals(RetainResiduals)),
		memberchk(training_series_length(Length), Diagnostics),
		integer(Length),
		Length >= 2,
		valid_seasonality(Seasonality, Length, Mode, Frequency),
		memberchk(observed_count(Observed), Diagnostics),
		integer(Observed),
		Observed >= 2,
		memberchk(missing_count(Missing), Diagnostics),
		integer(Missing),
		Missing >= 0,
		Observed + Missing =:= Length,
		memberchk(initialization_index(Anchor), Diagnostics),
		integer(Anchor),
		1 =< Anchor, Anchor =< Length - Observed + 1,
		(	Policy == error ->
			Missing =:= 0,
			Anchor =:= 1
		;	true
		),
		memberchk(update_count(0), Diagnostics),
		memberchk(slope(Slope), Diagnostics), memberchk(seasonal_mode(Mode), Diagnostics),
		memberchk(frequency(Frequency), Diagnostics), memberchk(initial_level(Initial), Diagnostics),
		^^finite_number(Initial),
		valid_fitting(Initialization, Diagnostics),
		memberchk(optimization_sum_squared_error(Objective), Diagnostics),
		^^finite_number(Objective), Objective >= 0,
		memberchk(error_basis(ses_training_fit), Diagnostics),
		memberchk(scored_count(Count), Diagnostics), integer(Count), Count =:= Observed - 1,
		memberchk(residuals(Residuals), Diagnostics),
		memberchk(residual_indices(ResidualIndices), Diagnostics),
		(	RetainResiduals == false ->
			Residuals == none, ResidualIndices == none
		;	^^valid_residual_history(Residuals, ResidualIndices, Anchor, Length, Count),
			finite_factors(Residuals)
		),
		memberchk(sum_absolute_error(Absolute), Diagnostics),
		memberchk(sum_squared_error(Squared), Diagnostics),
		memberchk(mean_absolute_error(MAE), Diagnostics),
		memberchk(root_mean_squared_error(RMSE), Diagnostics),
		^^finite_number(Absolute), Absolute >= 0,
		^^finite_number(Squared), Squared >= 0,
		^^finite_number(MAE), MAE >= 0, ^^finite_number(RMSE), RMSE >= 0,
		(	Mode == none ->
			Objective =:= Squared
		;	true
		),
		^^forecast_error_metrics(forecast_error_totals(Count,Absolute,Squared), ExpectedMAE, ExpectedRMSE),
		MAE =:= ExpectedMAE,
		RMSE =:= ExpectedRMSE,
		theta_correction(Length, Alpha, ExpectedCorrection),
		Correction =:= ExpectedCorrection.

	:- private(valid_seasonality/4).
	:- mode(valid_seasonality(+term, +positive_integer, ?atom, ?positive_integer), zero_or_one).
	:- info(valid_seasonality/4, [
		comment is 'Validates retained seasonal factors and next forecast phase.',
		argnames is ['Seasonality', 'Length', 'Mode', 'Frequency'],
		exceptions is [
			'Phase arithmetic fails' - evaluation_error('Error')
		]
	]).

	valid_seasonality(none, _Length, none, 1).
	valid_seasonality(seasonal(Mode,Frequency,Phase,Factors), Length, Mode, Frequency) :-
		memberchk(Mode, [additive, multiplicative]),
		valid(positive_integer, Frequency),
		Frequency >= 2,
		Length >= 2 * Frequency,
		integer(Phase),
		Phase =:= Length mod Frequency + 1,
		valid(list(number), Factors),
		length(Factors, Frequency),
		finite_factors(Factors),
		(	Mode == multiplicative ->
			positive_series(Factors)
		;	true
		).

	finite_factors([]).
	finite_factors([Factor| Factors]) :-
		^^finite_number(Factor),
		finite_factors(Factors).

	:- private(valid_fitting/2).
	:- mode(valid_fitting(+atom, +list(compound)), zero_or_one).
	:- info(valid_fitting/2, [
		comment is 'Validates fitting provenance and optimizer counters.',
		argnames is ['Initialization', 'Diagnostics'],
		exceptions is [
			'Counter comparison fails' - evaluation_error('Error')
		]
	]).

	valid_fitting(Initialization, Diagnostics) :-
		memberchk(optimizer_options(Options), Diagnostics),
		valid_optimizer_options(Options),
		memberchk(fitting(Fitting), Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(evaluations(Evaluations), Diagnostics),
		valid(non_negative_integer, Iterations),
		valid(non_negative_integer, Evaluations),
		(	Fitting == fixed_parameters ->
			Initialization == first,
			Convergence == fixed_parameters,
			Iterations == 0,
			Evaluations == 0
		;	Fitting == nelder_mead,
			Evaluations > 0,
			maximum_iterations(Options, Maximum),
			Iterations =< Maximum,
			(	Convergence == converged ->
				Iterations < Maximum
			;	Convergence == maximum_iterations,
				Iterations == Maximum
			)
		).

	unique_diagnostics([], _).
	unique_diagnostics([Diagnostic| Diagnostics], Seen) :-
		functor(Diagnostic, Name, 1),
		\+ member(Name, Seen),
		unique_diagnostics(Diagnostics, [Name| Seen]).

	valid_alpha(Alpha) :-
		^^finite_number(Alpha),
		0 =< Alpha, Alpha =< 1.

	valid_option(alpha(Alpha)) :-
		(	Alpha == auto ->
			true
		;	valid_alpha(Alpha)
		).
	valid_option(initialization(Initialization)) :-
		once((Initialization == first; Initialization == optimized)).
	valid_option(seasonal(Mode)) :-
		once((Mode == auto; Mode == none; Mode == additive; Mode == multiplicative)).
	valid_option(frequency(Frequency)) :-
		(	Frequency == dataset ->
			true
		;	valid(positive_integer, Frequency)
		).
	valid_option(optimizer_options(Options)) :-
		valid_optimizer_options(Options).
	valid_option(missing_policy(Policy)) :-
		once((Policy == skip_update; Policy == error)).
	valid_option(retain_residuals(Boolean)) :-
		once((Boolean == true; Boolean == false)).

	default_option(alpha(auto)).
	default_option(initialization(optimized)).
	default_option(seasonal(auto)).
	default_option(frequency(dataset)).
	default_option(optimizer_options([])).
	default_option(missing_policy(skip_update)).
	default_option(retain_residuals(false)).

	valid_optimizer_options(Options) :-
		valid(list(compound), Options),
		valid_optimizer_entries(Options, []).

	valid_optimizer_entries([], _).
	valid_optimizer_entries([Option| Options], Seen) :-
		functor(Option, Name, 1),
		ground(Option),
		memberchk(Name, [max_iterations, tol_x, tol_f, initial_step, adaptive]),
		\+ member(Name, Seen),
		nelder_mead(no_problem)::valid_option(Option),
		valid_optimizer_entries(Options, [Name| Seen]).

	forecaster_term_template(theta_forecaster(_, _), theta_forecaster('State','Diagnostics')).

	forecaster_export_template(_Dataset, Forecaster, Functor, Template) :-
		Template =.. [Functor, Forecaster].

	export_to_clauses(_Dataset, Forecaster, Functor, [Clause]) :-
		check_forecaster(Forecaster),
		context(Context), check(atom, Functor, Context),
		Clause =.. [Functor, Forecaster].

	export_to_file(Dataset, Forecaster, Functor, File) :-
		^^export_to_file(Dataset, Forecaster, Functor, File).

	print_forecaster(Forecaster) :-
		check_forecaster(Forecaster),
		^^print_forecaster_template(Forecaster),
		format('~q~n', [Forecaster]).

:- end_object.
