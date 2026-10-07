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


:- category(forecaster_common,
	implements(forecaster_protocol),
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Shared predicates for forecaster diagnostics, dataset validation, differencing, lag construction, error metrics, seasonal adjustment, and baselines.'
	]).

	:- uses(format, [
		format/2, format/3
	]).

	:- uses(integer, [
		sequence/3
	]).

	:- uses(list, [
		append/3, last/2, length/2, member/2, memberchk/2, reverse/2, same_length/2
	]).

	:- uses(numberlist, [
		sum/2
	]).

	:- uses(pairs, [
		keys/2, values/2
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	% hook predicates that concrete forecaster implementations must define

	:- protected(forecaster_diagnostics_data/2).
	:- mode(forecaster_diagnostics_data(+compound, -list(compound)), one).
	:- info(forecaster_diagnostics_data/2, [
		comment is 'Hook predicate that importing forecaster implementations must define in order to expose diagnostics metadata. A default implementation is provided that assumes the diagnostics list is the last argument of the forecaster term; concrete implementations following that convention do not need to override it.',
		argnames is ['Forecaster', 'Diagnostics']
	]).

	:- protected(forecaster_export_template/4).
	:- mode(forecaster_export_template(+object_identifier, +compound, +atom, -callable), one).
	:- info(forecaster_export_template/4, [
		comment is 'Hook predicate that importing forecaster implementations must define in order to expose the exported forecaster template for a given functor.',
		argnames is ['Dataset', 'Forecaster', 'Functor', 'Template']
	]).

	:- protected(forecaster_term_template/2).
	:- mode(forecaster_term_template(+compound, -callable), one).
	:- info(forecaster_term_template/2, [
		comment is 'Hook predicate that importing forecaster implementations must define in order to expose the learned forecaster term template used by pretty-printing helpers.',
		argnames is ['Forecaster', 'Template']
	]).

	% pretty-printing helper

	:- protected(print_forecaster_template/1).
	:- mode(print_forecaster_template(+compound), one).
	:- info(print_forecaster_template/1, [
		comment is 'Pretty-printing helper predicate used by importing forecaster implementations to show the learned forecaster term template.',
		argnames is ['Forecaster']
	]).

	% default protocol predicate implementations

	learn(Dataset, Forecaster) :-
		::learn(Dataset, Forecaster, []).

	check_forecaster(Forecaster) :-
		(	var(Forecaster) ->
			instantiation_error
		;	::forecaster_term_template(Forecaster, _Template),
			::forecaster_diagnostics_data(Forecaster, _Diagnostics) ->
			true
		;	domain_error(forecaster, Forecaster)
		).

	valid_forecaster(Forecaster) :-
		catch(::check_forecaster(Forecaster), _Error, fail).

	diagnostics(Forecaster, Diagnostics) :-
		::forecaster_diagnostics_data(Forecaster, Diagnostics).

	diagnostic(Forecaster, Diagnostic) :-
		::forecaster_diagnostics_data(Forecaster, Diagnostics),
		member(Diagnostic, Diagnostics).

	forecaster_options(Forecaster, Options) :-
		::forecaster_diagnostics_data(Forecaster, Diagnostics),
		memberchk(options(Options), Diagnostics).

	forecaster_diagnostics_data(Forecaster, Diagnostics) :-
		Forecaster =.. [_| Arguments],
		last(Arguments, Diagnostics).

	print_forecaster_template(Forecaster) :-
		::forecaster_term_template(Forecaster, Template),
		format('Template: ~w~n', [Template]).

	% dataset collection and validation

	:- protected(dataset_series/2).
	:- mode(dataset_series(+object_identifier, -list), one_or_error).
	:- info(dataset_series/2, [
		comment is 'Collects the dataset time-ordered observation values. Checks that the observation indices form a complete, gap-free, 1-based sequence and that the declared series length matches the observed length.',
		argnames is ['Dataset', 'Series'],
		exceptions is [
			'The dataset contains no observations' - domain_error(non_empty_series, 'Dataset'),
			'The observation indices do not form a complete, gap-free, 1-based sequence' - domain_error(series_index_sequence, 'Dataset'),
			'The declared series length is a variable' - instantiation_error,
			'The declared series length is neither a variable nor an integer' - type_error(integer, 'DeclaredLength'),
			'The declared series length is an integer but is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The declared and observed series lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength')
		]
	]).

	dataset_series(Dataset, Series) :-
		findall(
			Index-Value,
			Dataset::observation(Index, Value),
			Pairs0
		),
		(	Pairs0 == [] ->
			domain_error(non_empty_series, Dataset)
		;	true
		),
		keysort(Pairs0, Pairs),
		keys(Pairs, Indices),
		length(Indices, ObservedLength),
		(	sequence(1, ObservedLength, Indices) ->
			true
		;	domain_error(series_index_sequence, Dataset)
		),
		Dataset::series_length(DeclaredLength),
		context(Context),
		check(positive_integer, DeclaredLength, Context),
		(	DeclaredLength =:= ObservedLength ->
			values(Pairs, Series)
		;	consistency_error(series_length, DeclaredLength, ObservedLength)
		).

	:- protected(check_series/2).
	:- mode(check_series(+object_identifier, +list), one_or_error).
	:- info(check_series/2, [
		comment is 'Checks that a time series is non-empty and that every observation value is a number.',
		argnames is ['Dataset', 'Series'],
		exceptions is [
			'``Series`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'An element ``Value`` of the ``Series`` list is neither a variable nor a number' - type_error(number, 'Value'),
			'``Series`` is empty' - domain_error(non_empty_series, 'Dataset')
		]
	]).

	check_series(Dataset, Series) :-
		context(Context),
		(	Series == [] ->
			domain_error(non_empty_series, Dataset)
		;	true
		),
		check_series_values(Series, Context).

	check_series_values(Values, Context) :-
		check(list(number), Values, Context).

	:- protected(check_series/3).
	:- mode(check_series(+object_identifier, +list, +list), one_or_error).
	:- info(check_series/3, [
		comment is 'Checks that a time series is non-empty and that every observation value is of one of the given types.',
		argnames is ['Dataset', 'Series', 'Types'],
		exceptions is [
			'``Series`` is a partial list' - instantiation_error,
			'An element ``Value`` of the ``Series`` list is neither a variable nor a number' - domain_error(types('Types'), 'Value'),
			'``Series`` is empty' - domain_error(non_empty_series, 'Dataset')
		]
	]).

	check_series(Dataset, Series, Types) :-
		context(Context),
		(	Series == [] ->
			domain_error(non_empty_series, Dataset)
		;	true
		),
		check_series_values(Series, Types, Context).

	check_series_values(Values, Types, Context) :-
		check(list(types(Types)), Values, Context).

	:- protected(check_series_length/3).
	:- mode(check_series_length(+object_identifier, +list, +non_negative_integer), one_or_error).
	:- info(check_series_length/3, [
		comment is 'Checks that a time series has at least the given minimum number of observations.',
		argnames is ['Dataset', 'Series', 'MinimumLength'],
		exceptions is [
			'``MinimumLength`` is a variable' - instantiation_error,
			'``MinimumLength`` is neither a variable nor an integer' - type_error(integer, 'MinimumLength'),
			'``MinimumLength`` is an integer but is negative' - domain_error(non_negative_integer, 'MinimumLength'),
			'``Series`` is shorter than ``MinimumLength``' - domain_error(series_length, 'Dataset')
		]
	]).

	check_series_length(Dataset, Series, MinimumLength) :-
		context(Context),
		check(non_negative_integer, MinimumLength, Context),
		length(Series, Length),
		(	Length >= MinimumLength ->
			true
		;	domain_error(series_length, Dataset)
		).

	:- protected(check_observation/1).
	:- mode(check_observation(@term), one_or_error).
	:- info(check_observation/1, [
		comment is 'Checks that an observation is a number or an unbound variable representing a missing observation.',
		argnames is ['Observation'],
		exceptions is [
			'``Observation`` is neither a variable nor a number' - type_error(number, 'Observation')
		]
	]).

	check_observation(Observation) :-
		(	var(Observation) ->
			true
		;	number(Observation) ->
			true
		;	type_error(number, Observation)
		).

	:- protected(series_observation_summary/4).
	:- mode(series_observation_summary(+list, -non_negative_integer, -non_negative_integer, -number), one_or_error).
	:- info(series_observation_summary/4, [
		comment is 'Returns the elapsed length, numeric observation count, and numeric sum of a series. Unbound observations are counted in the length only and are not instantiated. An empty list has zero length, count, and sum.',
		argnames is ['Series', 'Length', 'ObservedCount', 'Sum'],
		exceptions is [
			'``Series`` is a variable or a partial list' - instantiation_error,
			'``Series`` is neither a variable nor a list' - type_error(list, 'Series'),
			'An observation is neither a variable nor a number' - type_error(number, 'Observation'),
			'Numeric summation raises an arithmetic evaluation error' - evaluation_error('Error')
		]
	]).

	series_observation_summary(Series, Length, ObservedCount, Sum) :-
		context(Context),
		check(list, Series, Context),
		observation_summary(Series, Length, ObservedCount, Sum).

	observation_summary(Series, Length, ObservedCount, Sum) :-
		observation_summary(Series, 0, 0, 0, Length, ObservedCount, Sum).

	observation_summary([], Length, ObservedCount, Sum, Length, ObservedCount, Sum).
	observation_summary([Observation| Observations], Length0, ObservedCount0, Sum0, Length, ObservedCount, Sum) :-
		check_observation(Observation),
		Length1 is Length0 + 1,
		(	var(Observation) ->
			ObservedCount1 = ObservedCount0,
			Sum1 = Sum0
		;	ObservedCount1 is ObservedCount0 + 1,
			Sum1 is Sum0 + Observation
		),
		observation_summary(Observations, Length1, ObservedCount1, Sum1, Length, ObservedCount, Sum).

	:- protected(normalize_missing_series/3).
	:- mode(normalize_missing_series(+list, -list, -term), one_or_error).
	:- info(normalize_missing_series/3, [
		comment is 'Copies numeric-or-unbound observations, sharing a fresh variable among missing positions without binding input variables.',
		argnames is ['Series', 'Normalized', 'Missing'],
		exceptions is [
			'The series is unbound or partial' - instantiation_error,
			'The series is not a list' - type_error(list, 'Series'),
			'A known observation is not numeric' - type_error(number, 'Value')
		]
	]).

	normalize_missing_series(Series, Normalized, Missing) :-
		context(Context), check(list, Series, Context), normalize_missing_values(Series, Missing, Context, Normalized).

	:- private(normalize_missing_values/4).
	:- mode(normalize_missing_values(+list, -term, +term, -list), one_or_error).
	:- info(normalize_missing_values/4, [
		comment is 'Copies known numbers and replaces unbound inputs with a fresh shared variable.',
		argnames is ['Series', 'Missing', 'Context', 'Normalized'],
		exceptions is [
			'A known observation is not numeric' - type_error(number, 'Value')
		]
	]).

	normalize_missing_values([], _, _, []).
	normalize_missing_values([Value| Values], Missing, Context, [Normalized| Rest]) :-
		(	var(Value) ->
			Normalized = Missing
		;	check(number, Value, Context),
			Normalized = Value
		),
		normalize_missing_values(Values, Missing, Context, Rest).

	:- protected(indexed_series_observations/3).
	:- mode(indexed_series_observations(+list, -list(positive_integer), -list(number)), one_or_error).
	:- info(indexed_series_observations/3, [
		comment is 'Collects known numeric values with their original one-based time indices.',
		argnames is ['Series', 'Indices', 'Values'],
		exceptions is [
			'The list is unbound or partial' - instantiation_error,
			'The series is not a list' - type_error(list, 'Series'),
			'A known value is not numeric' - type_error(number, 'Value'),
			'Index arithmetic fails' - evaluation_error('Error')
		]
	]).

	indexed_series_observations(Series, Indices, Values) :-
		context(Context),
		check(list, Series, Context),
		indexed_known_values(Series, 1, Context, Indices, Values).

	:- private(indexed_known_values/5).
	:- mode(indexed_known_values(+list, +positive_integer, +term, -list, -list), one_or_error).
	:- info(indexed_known_values/5, [
		comment is 'Extracts known observations while retaining elapsed positions.',
		argnames is ['Series', 'Index', 'Context', 'Indices', 'Values'],
		exceptions is [
			'A known value is not numeric' - type_error(number, 'Value'),
			'Index arithmetic fails' - evaluation_error('Error')
		]
	]).

	indexed_known_values([], _, _, [], []).
	indexed_known_values([Value| Values], Index, Context, Indices, Known) :-
		Next is Index + 1,
		(	var(Value) ->
			Indices = RestIndices,
			Known = RestKnown
		;	check(number, Value, Context),
			Indices = [Index| RestIndices],
			Known = [Value| RestKnown]
		),
		indexed_known_values(Values, Next, Context, RestIndices, RestKnown).

	:- protected(residual_fitted_values/3).
	:- mode(residual_fitted_values(+list, +list(number), -list), one_or_error).
	:- info(residual_fitted_values/3, [
		comment is 'Reconstructs aligned pre-update fits from numeric-or-unbound observations and residuals after the first known initialization anchor. Missing positions and the anchor have fresh independent unbound output placeholders.',
		argnames is ['Series', 'Residuals', 'Values'],
		exceptions is [
			'A list is unbound or partial, or a residual is unbound' - instantiation_error,
			'Series or residuals are not lists' - type_error(list, 'List'),
			'A known observation or residual is not numeric' - type_error(number, 'Value'),
			'The residual count differs from the known count minus the initialization anchor' - domain_error(residual_count, 'Residuals'),
			'Fit reconstruction arithmetic fails' - evaluation_error('Error')
		]
	]).

	residual_fitted_values(Series, Residuals, Values) :-
		context(Context),
		check(list, Series, Context),
		check(list, Residuals, Context),
		check_numeric_residuals(Residuals, Context),
		(	aligned_residual_fits(Series, Residuals, false, Context, Values) ->
			true
		;	domain_error(residual_count, Residuals)
		).

	:- private(check_numeric_residuals/2).
	:- mode(check_numeric_residuals(+list, +term), one_or_error).
	:- info(check_numeric_residuals/2, [
		comment is 'Checks residual numbers without completing lists or binding entries.',
		argnames is ['Residuals', 'Context'],
		exceptions is [
			'A residual is unbound' - instantiation_error,
			'A residual is not numeric' - type_error(number, 'Value')
		]
	]).

	check_numeric_residuals([], _).
	check_numeric_residuals([Residual| Residuals], Context) :-
		check(number, Residual, Context),
		check_numeric_residuals(Residuals, Context).

	:- private(aligned_residual_fits/5).
	:- mode(aligned_residual_fits(+list, +list(number), +boolean, +term, -list), zero_or_one_or_error).
	:- info(aligned_residual_fits/5, [
		comment is 'Consumes residuals only at known positions after the initialization anchor, failing on a count mismatch.',
		argnames is ['Series', 'Residuals', 'AnchorSeen', 'Context', 'Values'],
		exceptions is [
			'A known observation is not numeric' - type_error(number, 'Value'),
			'Fit reconstruction arithmetic fails' - evaluation_error('Error')
		]
	]).

	aligned_residual_fits([], [], _, _, []).
	aligned_residual_fits([Observation| Observations], Residuals, AnchorSeen, Context, [Fit| Fits]) :-
		(	var(Observation) ->
			Remaining = Residuals,
			NextAnchorSeen = AnchorSeen
		;	check(number, Observation, Context),
			(	AnchorSeen == false ->
				Remaining = Residuals
			;	Residuals = [Residual| Remaining],
				Fit is Observation - Residual
			),
			NextAnchorSeen = true
		),
		aligned_residual_fits(Observations, Remaining, NextAnchorSeen, Context, Fits).

	:- protected(valid_residual_history/5).
	:- mode(valid_residual_history(@list, @list, @positive_integer, @positive_integer, @non_negative_integer), zero_or_one_or_error).
	:- info(valid_residual_history/5, [
		comment is 'Validates ground parallel numeric residuals and strictly increasing original indices after the initialization anchor. Both lists must match the scored count. The caller validates finiteness; this predicate does not reconstruct error totals or certify historical observations.',
		argnames is ['Residuals', 'Indices', 'Anchor', 'Length', 'ScoredCount'],
		exceptions is [
			'Index or count arithmetic fails' - evaluation_error('Error')
		]
	]).

	valid_residual_history(Residuals, Indices, Anchor, Length, ScoredCount) :-
		ground(Residuals-Indices),
		valid(list(number), Residuals),
		valid(list(integer), Indices),
		integer(Anchor), Anchor >= 1,
		integer(Length), Length >= Anchor,
		integer(ScoredCount), ScoredCount >= 0, ScoredCount =< Length - Anchor,
		length(Residuals, ScoredCount),
		length(Indices, ScoredCount),
		valid_residual_positions(Indices, Anchor, Length).

	:- private(valid_residual_positions/3).
	:- mode(valid_residual_positions(+list(integer), +integer, +positive_integer), zero_or_one_or_error).
	:- info(valid_residual_positions/3, [
		comment is 'Checks original residual positions in strict chronological order within the elapsed length.',
		argnames is ['Indices', 'Previous', 'Length'],
		exceptions is [
			'Index comparison fails' - evaluation_error('Error')
		]
	]).

	valid_residual_positions([], _, _).
	valid_residual_positions([Index| Indices], Previous, Length) :-
		Index > Previous,
		Index =< Length,
		valid_residual_positions(Indices, Index, Length).

	% diagnostics helpers

	:- protected(base_forecaster_diagnostics/5).
	:- mode(base_forecaster_diagnostics(+atom, +integer, +list(compound), +list(compound), -list(compound)), one).
	:- info(base_forecaster_diagnostics/5, [
		comment is 'Builds the common part of a forecaster diagnostics metadata list, combined with implementation-specific extra diagnostics terms.',
		argnames is ['Model', 'TrainingSeriesLength', 'Options', 'ExtraDiagnostics', 'Diagnostics']
	]).

	base_forecaster_diagnostics(Model, TrainingSeriesLength, Options, ExtraDiagnostics, Diagnostics) :-
		Diagnostics = [
			model(Model),
			training_series_length(TrainingSeriesLength),
			options(Options)
		| ExtraDiagnostics
		].

	:- protected(valid_forecaster_metadata/2).
	:- mode(valid_forecaster_metadata(+atom, +list(compound)), zero_or_one).
	:- info(valid_forecaster_metadata/2, [
		comment is 'True when diagnostics metadata contains the expected model term.',
		argnames is ['Model', 'Diagnostics']
	]).

	valid_forecaster_metadata(Model, Diagnostics) :-
		valid(list(compound), Diagnostics),
		memberchk(model(Model), Diagnostics).

	:- protected(valid_forecaster_metadata/3).
	:- mode(valid_forecaster_metadata(+atom, +list(compound), +list(compound)), zero_or_one).
	:- info(valid_forecaster_metadata/3, [
		comment is 'True when diagnostics metadata contains the expected model term and records the given effective options.',
		argnames is ['Model', 'Options', 'Diagnostics']
	]).

	valid_forecaster_metadata(Model, Options, Diagnostics) :-
		valid_forecaster_metadata(Model, Diagnostics),
		memberchk(options(Options), Diagnostics).

	:- protected(replace_diagnostic/4).
	:- mode(replace_diagnostic(+atom, +term, +list(compound), -list(compound)), zero_or_one).
	:- info(replace_diagnostic/4, [
		comment is 'Replaces the value of the first unary diagnostic with the given name, preserving order. Fails when no such diagnostic exists.',
		argnames is ['Name', 'Value', 'Diagnostics', 'UpdatedDiagnostics']
	]).

	replace_diagnostic(Name, Value, [Diagnostic| Diagnostics], [UpdatedDiagnostic| Diagnostics]) :-
		functor(Diagnostic, Name, 1),
		!,
		UpdatedDiagnostic =.. [Name, Value].
	replace_diagnostic(Name, Value, [Diagnostic| Diagnostics], [Diagnostic| UpdatedDiagnostics]) :-
		replace_diagnostic(Name, Value, Diagnostics, UpdatedDiagnostics).

	:- protected(updated_observation_diagnostics/3).
	:- mode(updated_observation_diagnostics(+list(compound), @term, -list(compound)), one_or_error).
	:- info(updated_observation_diagnostics/3, [
		comment is 'Updates the training length, update count, observed count, and missing count of validated diagnostics after a numeric or missing observation, preserving metadata order and all other diagnostics. Requires existing non-negative integer counters.',
		argnames is ['Diagnostics', 'Observation', 'UpdatedDiagnostics'],
		exceptions is [
			'``Observation`` is neither a variable nor a number' - type_error(number, 'Observation'),
			'Counter arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	updated_observation_diagnostics(Diagnostics, Observation, UpdatedDiagnostics) :-
		check_observation(Observation),
		memberchk(training_series_length(Length0), Diagnostics),
		memberchk(update_count(Updates0), Diagnostics),
		memberchk(observed_count(Count0), Diagnostics),
		memberchk(missing_count(Missing0), Diagnostics),
		Length is Length0 + 1,
		Updates is Updates0 + 1,
		(	var(Observation) ->
			Count = Count0, Missing is Missing0 + 1
		;	Count is Count0 + 1, Missing = Missing0
		),
		replace_diagnostic(training_series_length, Length, Diagnostics, Diagnostics1),
		replace_diagnostic(update_count, Updates, Diagnostics1, Diagnostics2),
		replace_diagnostic(observed_count, Count, Diagnostics2, Diagnostics3),
		replace_diagnostic(missing_count, Missing, Diagnostics3, UpdatedDiagnostics).

	export_to_file(Dataset, Forecaster, Functor, File) :-
		::export_to_clauses(Dataset, Forecaster, Functor, Clauses),
		open(File, write, Stream),
		(	catch(
			(	write_comment_header(Dataset, Functor, Forecaster, Stream),
				write_clauses(Clauses, Stream)
			),
			Error,
			(safe_close_stream(Stream), throw(Error))
		) ->
			close(Stream)
		;	safe_close_stream(Stream),
			fail
		).

	safe_close_stream(Stream) :-
		catch(close(Stream), _, true).

	write_comment_header(Dataset, Functor, Forecaster, Stream) :-
		::forecaster_export_template(Dataset, Forecaster, Functor, Template),
		functor(Template, _, Arity),
		format(Stream, '% exported forecaster predicate: ~q/~d~n', [Functor, Arity]),
		format(Stream, '% training dataset: ~q~n', [Dataset]),
		::dataset_series(Dataset, Series),
		length(Series, Length),
		format(Stream, '% training series length: ~d~n', [Length]),
		(	::diagnostics(Forecaster, Diagnostics) ->
			format(Stream, '% diagnostics: ~q~n', [Diagnostics])
		;	true
		),
		format(Stream, '% ~w~n', [Template]).

	write_clauses([], _Stream).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

	% differencing and reconstruction

	:- protected(difference_series/2).
	:- mode(difference_series(+list(number), -list(number)), one_or_error).
	:- info(difference_series/2, [
		comment is 'Computes the first-order differences of a series: ``Differences[i] = Series[i+1] - Series[i]``. The result has one fewer element than ``Series``.',
		argnames is ['Series', 'Differences'],
		exceptions is [
			'``Series`` is empty' - domain_error(non_empty_series, 'Series')
		]
	]).

	difference_series([], _) :-
		domain_error(non_empty_series, []).
	difference_series([First| Rest], Differences) :-
		difference_series(Rest, First, Differences).

	difference_series([], _, []).
	difference_series([Value| Values], Previous, [Difference| Differences]) :-
		Difference is Value - Previous,
		difference_series(Values, Value, Differences).

	:- protected(integrate_series/3).
	:- mode(integrate_series(+list(number), +number, -list(number)), one).
	:- info(integrate_series/3, [
		comment is 'Reconstructs a series of levels from a series of differences and a starting base value (typically the last observed original value), by cumulative summing. Used to undo ``difference_series/2`` on forecasted differences.',
		argnames is ['Differences', 'Base', 'Series']
	]).

	integrate_series([], _, []).
	integrate_series([Difference| Differences], Previous, [Value| Values]) :-
		Value is Previous + Difference,
		integrate_series(Differences, Value, Values).

	% lag construction for autoregressive-style fitting

	:- protected(lagged_rows/3).
	:- mode(lagged_rows(+list(number), +positive_integer, -list(pair)), one_or_error).
	:- info(lagged_rows/3, [
		comment is 'Builds a list of ``Lags-Target`` rows from a series, where ``Lags`` is a list of the ``Order`` most recent values preceding ``Target``, most recent first (``[X(t-1), X(t-2), ..., X(t-Order)]-X(t)``). Requires the series to have more than ``Order`` observations.',
		argnames is ['Series', 'Order', 'Rows'],
		exceptions is [
			'``Order`` is a variable' - instantiation_error,
			'``Order`` is neither a variable nor an integer' - type_error(integer, 'Order'),
			'``Order`` is an integer but is not positive' - domain_error(positive_integer, 'Order'),
			'``Series`` has ``Order`` or fewer observations' - domain_error(series_length, 'Series')
		]
	]).

	lagged_rows(Series, Order, Rows) :-
		context(Context),
		check(positive_integer, Order, Context),
		length(InitialLags0, Order),
		(	append(InitialLags0, Rest, Series),
			Rest \== [] ->
			true
		;	domain_error(series_length, Series)
		),
		reverse(InitialLags0, InitialLags),
		lagged_rows_(Rest, InitialLags, Order, Rows).

	lagged_rows_([], _, _, []).
	lagged_rows_([Target| Values], Lags, Order, [Lags-Target| Rows]) :-
		update_lags(Lags, Target, Order, NewLags),
		lagged_rows_(Values, NewLags, Order, Rows).

	update_lags(Lags, Target, Order, [Target| Trimmed]) :-
		Order1 is Order - 1,
		length(Trimmed, Order1),
		append(Trimmed, [_], Lags).

	% forecast error metrics

	:- protected(accumulate_forecast_error/4).
	:- mode(accumulate_forecast_error(+number, +number, +compound, -compound), one_or_error).
	:- info(accumulate_forecast_error/4, [
		comment is 'Adds one numeric actual/prediction error to validated forecast_error_totals(Count, AbsoluteSum, SquaredSum), without retaining observations. Requires a non-negative integer count and non-negative numeric sums; forecast_error_totals(0,0,0) initializes accumulation. Missing observations must be skipped by the caller.',
		argnames is ['Actual', 'Prediction', 'Totals', 'UpdatedTotals'],
		exceptions is [
			'``Actual`` or ``Prediction`` is a variable' - instantiation_error,
			'``Actual`` or ``Prediction`` is not a number' - type_error(number, 'Value'),
			'Error or total arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	accumulate_forecast_error(Actual, Prediction, forecast_error_totals(Count0, Absolute0, Squared0), forecast_error_totals(Count, Absolute, Squared)) :-
		context(Context),
		check(number, Actual, Context),
		check(number, Prediction, Context),
		Error is Actual - Prediction,
		Count is Count0 + 1,
		Absolute is Absolute0 + abs(Error),
		Squared is Squared0 + Error * Error.

	:- protected(forecast_error_metrics/3).
	:- mode(forecast_error_metrics(+compound, -float, -float), one_or_error).
	:- info(forecast_error_metrics/3, [
		comment is 'Computes MAE and RMSE from validated forecast_error_totals(Count, AbsoluteSum, SquaredSum). Requires non-negative numeric sums and a positive integer count. Does not retain or reconstruct actual/predicted observations.',
		argnames is ['Totals', 'MAE', 'RMSE'],
		exceptions is [
			'The scored count is a variable' - instantiation_error,
			'The scored count is not an integer' - type_error(integer, 'Count'),
			'The scored count is not positive' - domain_error(positive_integer, 'Count'),
			'Metric arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	forecast_error_metrics(forecast_error_totals(Count, Absolute, Squared), MAE, RMSE) :-
		context(Context),
		check(positive_integer, Count, Context),
		MAE is float(Absolute / Count),
		RMSE is float(sqrt(Squared / Count)).

	:- protected(mean_absolute_error/3).
	:- mode(mean_absolute_error(+list(number), +list(number), -float), one_or_error).
	:- info(mean_absolute_error/3, [
		comment is 'Computes the mean absolute error (MAE) between actual and predicted value lists of matching length.',
		argnames is ['Actual', 'Predicted', 'MAE'],
		exceptions is [
			'An input series is empty' - domain_error(non_empty_series, 'Series'),
			'The actual and predicted series have different lengths' - consistency_error(same_length, 'Actual', 'Predicted')
		]
	]).

	mean_absolute_error(Actual, Predicted, MAE) :-
		check_metric_series(Actual, Predicted),
		absolute_differences(Actual, Predicted, AbsoluteDifferences),
		length(AbsoluteDifferences, N),
		N > 0,
		sum(AbsoluteDifferences, Sum),
		MAE is float(Sum / N).

	absolute_differences([], [], []).
	absolute_differences([A| As], [P| Ps], [D| Ds]) :-
		D is abs(A - P),
		absolute_differences(As, Ps, Ds).

	:- protected(root_mean_squared_error/3).
	:- mode(root_mean_squared_error(+list(number), +list(number), -float), one_or_error).
	:- info(root_mean_squared_error/3, [
		comment is 'Computes the root mean squared error (RMSE) between actual and predicted value lists of matching length.',
		argnames is ['Actual', 'Predicted', 'RMSE'],
		exceptions is [
			'An input series is empty' - domain_error(non_empty_series, 'Series'),
			'The actual and predicted series have different lengths' - consistency_error(same_length, 'Actual', 'Predicted')
		]
	]).

	root_mean_squared_error(Actual, Predicted, RMSE) :-
		check_metric_series(Actual, Predicted),
		squared_differences(Actual, Predicted, SquaredDifferences),
		length(SquaredDifferences, N),
		N > 0,
		sum(SquaredDifferences, Sum),
		MeanSquaredError is Sum / N,
		RMSE is float(sqrt(MeanSquaredError)).

	squared_differences([], [], []).
	squared_differences([A| As], [P| Ps], [D| Ds]) :-
		D is (A - P) ** 2,
		squared_differences(As, Ps, Ds).

	:- protected(mean_absolute_percentage_error/3).
	:- mode(mean_absolute_percentage_error(+list(number), +list(number), -float), one_or_error).
	:- info(mean_absolute_percentage_error/3, [
		comment is 'Computes the mean absolute percentage error (MAPE), as a percentage, between actual and predicted value lists of matching length.',
		argnames is ['Actual', 'Predicted', 'MAPE'],
		exceptions is [
			'An input series is empty' - domain_error(non_empty_series, 'Series'),
			'The actual and predicted series have different lengths' - consistency_error(same_length, 'Actual', 'Predicted'),
			'An actual value is zero' - evaluation_error(zero_divisor)
		]
	]).

	mean_absolute_percentage_error(Actual, Predicted, MAPE) :-
		check_metric_series(Actual, Predicted),
		percentage_errors(Actual, Predicted, PercentageErrors),
		length(PercentageErrors, N),
		N > 0,
		sum(PercentageErrors, Sum),
		MAPE is float((Sum / N) * 100).

	percentage_errors([], [], []).
	percentage_errors([A| As], [P| Ps], [E| Es]) :-
		(	A =:= 0 ->
			evaluation_error(zero_divisor)
		;	E is abs((A - P) / A)
		),
		percentage_errors(As, Ps, Es).

	check_metric_series(Actual, Predicted) :-
		context(Context),
		check(list(number), Actual, Context),
		check(list(number), Predicted, Context),
		(	Actual == [] ->
			domain_error(non_empty_series, Actual)
		;	Predicted == [] ->
			domain_error(non_empty_series, Predicted)
		;	same_length(Actual, Predicted) ->
			true
		;	consistency_error(same_length, Actual, Predicted)
		).

	% naive baselines, also useful as fallbacks and building blocks

	:- protected(naive_forecast/3).
	:- mode(naive_forecast(+list(number), +non_negative_integer, -list(number)), one_or_error).
	:- info(naive_forecast/3, [
		comment is 'Repeats the last observed series value ``Horizon`` times (the naive/persistence baseline forecast). A zero horizon returns an empty list.',
		argnames is ['Series', 'Horizon', 'Forecasts'],
		exceptions is [
			'``Horizon`` is a variable' - instantiation_error,
			'``Horizon`` is neither a variable nor an integer' - type_error(integer, 'Horizon'),
			'``Horizon`` is an integer but is negative' - domain_error(non_negative_integer, 'Horizon')
		]
	]).

	naive_forecast(Series, Horizon, Forecasts) :-
		check_forecast_horizon(Horizon),
		last(Series, LastValue),
		constant_forecast(LastValue, Horizon, Forecasts).

	:- protected(constant_forecast/3).
	:- mode(constant_forecast(+number, +non_negative_integer, -list(number)), one_or_error).
	:- info(constant_forecast/3, [
		comment is 'Repeats a value for the given forecast horizon. A zero horizon returns an empty list.',
		argnames is ['Value', 'Horizon', 'Forecasts'],
		exceptions is [
			'``Horizon`` is a variable' - instantiation_error,
			'``Horizon`` is neither a variable nor an integer' - type_error(integer, 'Horizon'),
			'``Horizon`` is an integer but is negative' - domain_error(non_negative_integer, 'Horizon')
		]
	]).

	constant_forecast(Value, Horizon, Forecasts) :-
		check_forecast_horizon(Horizon),
		length(Forecasts, Horizon),
		repeat_value(Forecasts, Value).

	:- protected(linear_trend_forecast/4).
	:- mode(linear_trend_forecast(+number, +number, +non_negative_integer, -list(number)), one_or_error).
	:- info(linear_trend_forecast/4, [
		comment is 'Forecasts ``Last + Step * Slope`` for steps one through the horizon. A zero horizon returns an empty list.',
		argnames is ['Last', 'Slope', 'Horizon', 'Forecasts'],
		exceptions is [
			'``Horizon``, ``Last``, or ``Slope`` is a variable' - instantiation_error,
			'``Horizon`` is neither a variable nor an integer' - type_error(integer, 'Horizon'),
			'``Horizon`` is an integer but is negative' - domain_error(non_negative_integer, 'Horizon'),
			'``Last`` is neither a variable nor a number' - type_error(number, 'Last'),
			'``Slope`` is neither a variable nor a number' - type_error(number, 'Slope'),
			'Trend extrapolation raises an arithmetic evaluation error' - evaluation_error('Error')
		]
	]).

	linear_trend_forecast(Last, Slope, Horizon, Forecasts) :-
		check_forecast_horizon(Horizon),
		context(Context),
		check(number, Last, Context),
		check(number, Slope, Context),
		trend_forecasts(Horizon, 1, Last, Slope, Forecasts).

	trend_forecasts(0, _, _, _, []) :-
		!.
	trend_forecasts(Remaining, Step, Last, Slope, [Value| Values]) :-
		Value is Last + Step * Slope,
		NextStep is Step + 1,
		NextRemaining is Remaining - 1,
		trend_forecasts(NextRemaining, NextStep, Last, Slope, Values).

	:- protected(check_forecast_horizon/1).
	:- mode(check_forecast_horizon(+non_negative_integer), one_or_error).
	:- info(check_forecast_horizon/1, [
		comment is 'Checks that a forecast horizon is a non-negative integer.',
		argnames is ['Horizon'],
		exceptions is [
			'``Horizon`` is a variable' - instantiation_error,
			'``Horizon`` is neither a variable nor an integer' - type_error(integer, 'Horizon'),
			'``Horizon`` is an integer but is negative' - domain_error(non_negative_integer, 'Horizon')
		]
	]).

	check_forecast_horizon(Horizon) :-
		context(Context),
		check(non_negative_integer, Horizon, Context).

	repeat_value([], _).
	repeat_value([Value| Values], Value) :-
		repeat_value(Values, Value).

	:- protected(seasonal_naive_forecast/4).
	:- mode(seasonal_naive_forecast(+list(number), +positive_integer, +non_negative_integer, -list(number)), one_or_error).
	:- info(seasonal_naive_forecast/4, [
		comment is 'Repeats the last full seasonal cycle of ``Frequency`` observations, cycling as needed to cover ``Horizon`` forecasts (the seasonal naive baseline forecast).',
		argnames is ['Series', 'Frequency', 'Horizon', 'Forecasts'],
		exceptions is [
			'``Frequency`` is a variable' - instantiation_error,
			'``Frequency`` is neither a variable nor an integer' - type_error(integer, 'Frequency'),
			'``Frequency`` is an integer but is not positive' - domain_error(positive_integer, 'Frequency'),
			'``Horizon`` is a variable' - instantiation_error,
			'``Horizon`` is neither a variable nor an integer' - type_error(integer, 'Horizon'),
			'``Horizon`` is an integer but is negative' - domain_error(non_negative_integer, 'Horizon'),
			'``Series`` has fewer than ``Frequency`` observations' - domain_error(series_length, 'Series')
		]
	]).

	seasonal_naive_forecast(Series, Frequency, Horizon, Forecasts) :-
		check_frequency(Frequency),
		check_forecast_horizon(Horizon),
		length(Series, Length),
		PrefixLength is Length - Frequency,
		(	PrefixLength >= 0 ->
			true
		;	domain_error(series_length, Series)
		),
		length(Prefix, PrefixLength),
		append(Prefix, LastSeason, Series),
		cycle_take(LastSeason, LastSeason, Horizon, Forecasts).

	:- protected(check_frequency/1).
	:- mode(check_frequency(+positive_integer), one_or_error).
	:- info(check_frequency/1, [
		comment is 'Checks that a seasonal frequency is a positive integer.',
		argnames is ['Frequency'],
		exceptions is [
			'``Frequency`` is a variable' - instantiation_error,
			'``Frequency`` is neither a variable nor an integer' - type_error(integer, 'Frequency'),
			'``Frequency`` is an integer but is not positive' - domain_error(positive_integer, 'Frequency')
		]
	]).

	check_frequency(Frequency) :-
		context(Context),
		check(positive_integer, Frequency, Context).

	:- private(seasonal_centered_values/5).
	:- mode(seasonal_centered_values(+list(number), +number, -list(number), +number, -number), one_or_error).
	:- info(seasonal_centered_values/5, [
		comment is 'Centers validated observations and accumulates squared deviations.',
		argnames is ['Series', 'Mean', 'Centered', 'Variance0', 'Variance'],
		exceptions is [
			'Centering arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- private(seasonal_lag_scores/7).
	:- mode(seasonal_lag_scores(+positive_integer, +positive_integer, +list(number), +number, +number, -number, -number), one_or_error).
	:- info(seasonal_lag_scores/7, [
		comment is 'Accumulates earlier squared autocorrelations and the seasonal-lag score.',
		argnames is ['Lag', 'Frequency', 'Centered', 'Variance', 'Squares0', 'Squares', 'Last'],
		exceptions is [
			'Autocorrelation arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- private(seasonal_covariance/4).
	:- mode(seasonal_covariance(+list(number), +list(number), +number, -number), one_or_error).
	:- info(seasonal_covariance/4, [
		comment is 'Accumulates products of aligned centered observations.',
		argnames is ['Suffix', 'Series', 'Covariance0', 'Covariance'],
		exceptions is [
			'Covariance arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- private(seasonal_check_method/1).
	:- mode(seasonal_check_method(@term), one_or_error).
	:- info(seasonal_check_method/1, [
		comment is 'Checks the seasonal adjustment method without binding it.',
		argnames is ['Method'],
		exceptions is [
			'The method is unbound' - instantiation_error,
			'The method is invalid' - domain_error(seasonal_adjustment_method, 'Method')
		]
	]).

	:- private(seasonal_positive_values/1).
	:- mode(seasonal_positive_values(+list(number)), one_or_error).
	:- info(seasonal_positive_values/1, [
		comment is 'Requires strictly positive validated numeric values.',
		argnames is ['Values'],
		exceptions is [
			'A value is not positive' - domain_error(positive_number, 'Value'),
			'Numeric comparison fails' - evaluation_error('Error')
		]
	]).

	:- private(seasonal_window_means/5).
	:- mode(seasonal_window_means(+list(number), +list(number), +positive_integer, +number, -list(number)), one_or_error).
	:- info(seasonal_window_means/5, [
		comment is 'Computes successive moving averages with a rolling sum.',
		argnames is ['Entering', 'Leaving', 'Frequency', 'Sum', 'Means'],
		exceptions is [
			'Moving-average arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- private(seasonal_center_even/2).
	:- mode(seasonal_center_even(+list(number), -list(number)), one_or_error).
	:- info(seasonal_center_even/2, [
		comment is 'Centers even-period moving averages using adjacent means.',
		argnames is ['Means', 'Centered'],
		exceptions is [
			'Centering arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- private(seasonal_deviations/4).
	:- mode(seasonal_deviations(+list(number), +list(number), +atom, -list(number)), one_or_error).
	:- info(seasonal_deviations/4, [
		comment is 'Computes interior differences or ratios against centered trend estimates.',
		argnames is ['Trends', 'Values', 'Method', 'Deviations'],
		exceptions is [
			'A multiplicative trend is not positive' - domain_error(positive_number, 'Value'),
			'Detrending arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- private(seasonal_fold_cycles/3).
	:- mode(seasonal_fold_cycles(+list, +list(compound), -list(compound)), one_or_error).
	:- info(seasonal_fold_cycles/3, [
		comment is 'Accumulates complete and partial cycles into phase buckets.',
		argnames is ['Values', 'Buckets0', 'Buckets'],
		exceptions is [
			'Phase accumulation fails' - evaluation_error('Error')
		]
	]).

	:- private(seasonal_bucket_means/2).
	:- mode(seasonal_bucket_means(+list(compound), -list(number)), one_or_error).
	:- info(seasonal_bucket_means/2, [
		comment is 'Computes phase means from nonempty phase totals.',
		argnames is ['Buckets', 'Means'],
		exceptions is [
			'Phase-mean arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- private(seasonal_normalize/4).
	:- mode(seasonal_normalize(+list(number), +atom, +number, -list(number)), one_or_error).
	:- info(seasonal_normalize/4, [
		comment is 'Normalizes phase factors to zero or unit mean.',
		argnames is ['Values', 'Method', 'Mean', 'Factors'],
		exceptions is [
			'A multiplicative factor is not positive' - domain_error(positive_number, 'Value'),
			'Normalization arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- private(seasonal_inverse_factors/3).
	:- mode(seasonal_inverse_factors(+list(number), +atom, -list(number)), one_or_error).
	:- info(seasonal_inverse_factors/3, [
		comment is 'Negates additive factors or reciprocates positive multiplicative factors.',
		argnames is ['Factors', 'Method', 'Inverses'],
		exceptions is [
			'Factor inversion fails' - evaluation_error('Error')
		]
	]).

	:- private(seasonal_restore_values/5).
	:- mode(seasonal_restore_values(+list, +atom, +list(number), +list(number), -list), one_or_error).
	:- info(seasonal_restore_values/5, [
		comment is 'Restores validated values while cycling through phase factors.',
		argnames is ['Values', 'Method', 'Factors', 'Cycle', 'Restored'],
		exceptions is [
			'Restoration arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- protected(seasonal_autocorrelation_test/4).
	:- mode(seasonal_autocorrelation_test(+list, +positive_integer, -number, -boolean), one_or_error).
	:- info(seasonal_autocorrelation_test/4, [
		comment is 'Tests original-lag available-pair autocorrelations using the known observation count and requiring two known pairs at every tested lag.',
		argnames is ['Series', 'Frequency', 'Statistic', 'Seasonal'],
		exceptions is [
			'The series or frequency is unbound' - instantiation_error,
			'A series value is not numeric' - type_error(number, 'Value'),
			'The series is not a list' - type_error(list, 'Series'),
			'The series is empty' - domain_error(non_empty_series, 'Series'),
			'The frequency is not an integer' - type_error(integer, 'Frequency'),
			'The frequency is not positive' - domain_error(positive_integer, 'Frequency'),
			'Seasonality test arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- private(seasonal_complete_autocorrelation_test/4).
	:- mode(seasonal_complete_autocorrelation_test(+list(number), +positive_integer, -number, -boolean), one_or_error).
	:- info(seasonal_complete_autocorrelation_test/4, [
		comment is 'Computes the original complete-data seasonality statistic for validated inputs.',
		argnames is ['Series', 'Frequency', 'Statistic', 'Seasonal'],
		exceptions is [
			'Autocorrelation arithmetic fails' - evaluation_error('Error')
		]
	]).

	seasonal_complete_autocorrelation_test(Series, Frequency, Statistic, Seasonal) :-
		length(Series, Length),
		(	Frequency > 1,
			Length > 2 * Frequency ->
			sum(Series, Sum), Mean is Sum / Length,
			seasonal_centered_values(Series, Mean, Centered, 0.0, Variance),
			(	Variance > 0 ->
				seasonal_lag_scores(1, Frequency, Centered, Variance, 0.0, EarlierSquares, Last),
				Statistic is abs(Last) / sqrt((1.0 + 2.0 * EarlierSquares) / Length),
				(	Statistic > 1.6448536269514722 ->
					Seasonal = true
				;	Seasonal = false
				)
			;	Statistic = 0.0,
				Seasonal = false
			)
		;	Statistic = 0.0,
			Seasonal = false
		).

	seasonal_centered_values([], _, [], Variance, Variance).
	seasonal_centered_values([Value| Values], Mean, [Centered| Rest], Variance0, Variance) :-
		Centered is Value - Mean,
		Variance1 is Variance0 + Centered * Centered,
		seasonal_centered_values(Values, Mean, Rest, Variance1, Variance).

	seasonal_lag_scores(Lag, Frequency, Centered, Variance, Squares0, Squares, Last) :-
		length(Skip, Lag),
		append(Skip, Suffix, Centered),
		seasonal_covariance(Suffix, Centered, 0.0, Covariance),
		Score is Covariance / Variance,
		(	Lag =:= Frequency ->
			Squares = Squares0,
			Last = Score
		;	Squares1 is Squares0 + Score * Score,
			NextLag is Lag + 1,
			seasonal_lag_scores(NextLag, Frequency, Centered, Variance, Squares1, Squares, Last)
		).

	seasonal_covariance([], _, Covariance, Covariance).
	seasonal_covariance([First| Firsts], [Second| Seconds], Covariance0, Covariance) :-
		Covariance1 is Covariance0 + First * Second,
		seasonal_covariance(Firsts, Seconds, Covariance1, Covariance).

	:- protected(classical_seasonal_adjustment/5).
	:- mode(classical_seasonal_adjustment(+list, +positive_integer, +atom, -list, -list(number)), one_or_error).
	:- info(classical_seasonal_adjustment/5, [
		comment is 'Deseasonalizes known values using complete centered windows and normalized phase factors, preserving missing observations as unbound variables.',
		argnames is ['Series', 'Frequency', 'Method', 'AdjustedSeries', 'Factors'],
		exceptions is [
			'The series, frequency, or method is unbound' - instantiation_error,
			'A value is not numeric' - type_error(number, 'Value'),
			'The series is not a list' - type_error(list, 'Series'),
			'The series is empty' - domain_error(non_empty_series, 'Series'),
			'The frequency is not an integer' - type_error(integer, 'Frequency'),
			'The frequency is not positive' - domain_error(positive_integer, 'Frequency'),
			'The frequency is less than two' - domain_error(seasonal_frequency, 'Frequency'),
			'The series has fewer than two cycles' - domain_error(series_length, 'Series'),
			'The method is invalid' - domain_error(seasonal_adjustment_method, 'Method'),
			'A phase has no valid centered-window deviation' - domain_error(insufficient_seasonal_phase_observations, 'Phase'),
			'Multiplicative data or factors are not positive' - domain_error(positive_number, 'Value'),
			'Seasonal adjustment arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- private(classical_complete_seasonal_adjustment/5).
	:- mode(classical_complete_seasonal_adjustment(+list(number), +positive_integer, +atom, -list(number), -list(number)), one_or_error).
	:- info(classical_complete_seasonal_adjustment/5, [
		comment is 'Computes classical seasonal factors for validated complete data.',
		argnames is ['Series', 'Frequency', 'Method', 'Adjusted', 'Factors'],
		exceptions is [
			'Multiplicative data, trends, or factors are not positive' - domain_error(positive_number, 'Value'),
			'Seasonal arithmetic fails' - evaluation_error('Error')
		]
	]).

	classical_complete_seasonal_adjustment(Series, Frequency, Method, Adjusted, Factors) :-
		(	Method == multiplicative ->
			seasonal_positive_values(Series)
		;	true
		),
		length(Window, Frequency), append(Window, AfterWindow, Series), sum(Window, WindowSum),
		FirstMean is WindowSum / Frequency,
		seasonal_window_means(AfterWindow, Series, Frequency, WindowSum, RemainingMeans),
		(	Frequency mod 2 =:= 0 ->
			seasonal_center_even([FirstMean| RemainingMeans], Trends)
		;	Trends = [FirstMean| RemainingMeans]
		),
		Half is Frequency // 2,
		length(Prefix, Half),
		append(Prefix, Interior, Series),
		seasonal_deviations(Trends, Interior, Method, Deviations),
		length(InitialBuckets, Frequency),
		seasonal_empty_buckets(InitialBuckets),
		seasonal_fold_cycles(Deviations, InitialBuckets, Buckets),
		seasonal_bucket_means(Buckets, RawFactors),
		sum(RawFactors, FactorSum),
		FactorMean is FactorSum / Frequency,
		seasonal_normalize(RawFactors, Method, FactorMean, Normalized),
		StartPhase is Half mod Frequency + 1,
		Split is Frequency - StartPhase + 1,
		length(Leading, Split),
		append(Leading, Trailing, Normalized),
		append(Trailing, Leading, Factors),
		seasonal_inverse_factors(Factors, Method, Inverse),
		restore_seasonality(Method, Inverse, 1, Series, Adjusted).

	seasonal_check_method(Method) :-
		(	var(Method) ->
			instantiation_error
		;	member(Method, [additive,multiplicative]) ->
			true
		;	domain_error(seasonal_adjustment_method, Method)
		).

	seasonal_positive_values([]).
	seasonal_positive_values([Value| Values]) :-
		(	Value > 0 ->
			true
		;	domain_error(positive_number, Value)
		),
		seasonal_positive_values(Values).

	seasonal_window_means([], _, _, _, []).
	seasonal_window_means([New| News], [Old| Olds], Frequency, Sum0, [Mean| Means]) :-
		Sum1 is Sum0 - Old + New,
		Mean is Sum1 / Frequency,
		seasonal_window_means(News, Olds, Frequency, Sum1, Means).

	seasonal_center_even([_], []) :-
		!.
	seasonal_center_even([First,Second| Values], [Mean| Means]) :-
		Mean is (First + Second) / 2.0,
		seasonal_center_even([Second| Values], Means).

	seasonal_deviations([], _, _, []).
	seasonal_deviations([Trend| Trends], [Value| Values], Method, [Deviation| Deviations]) :-
		(	Method == additive ->
			Deviation is Value - Trend
		;	seasonal_positive_values([Trend]),
			Deviation is Value / Trend
		),
		seasonal_deviations(Trends, Values, Method, Deviations).

	seasonal_empty_buckets([]).
	seasonal_empty_buckets([bucket(0.0,0)| Buckets]) :-
		seasonal_empty_buckets(Buckets).

	seasonal_bucket_means([], []).
	seasonal_bucket_means([bucket(Sum,Count)| Buckets], [Mean| Means]) :-
		Mean is Sum / Count,
		seasonal_bucket_means(Buckets, Means).

	seasonal_normalize([], _, _, []).
	seasonal_normalize([Value| Values], Method, Mean, [Factor| Factors]) :-
		(	Method == additive ->
			Factor is Value - Mean
		;	Factor is Value / Mean,
			seasonal_positive_values([Factor])
		),
		seasonal_normalize(Values, Method, Mean, Factors).

	seasonal_inverse_factors([], _, []).
	seasonal_inverse_factors([Factor| Factors], Method, [Inverse| Inverses]) :-
		(	Method == additive ->
			Inverse is -Factor
		;	Inverse is 1.0 / Factor
		),
		seasonal_inverse_factors(Factors, Method, Inverses).

	:- protected(restore_seasonality/5).
	:- mode(restore_seasonality(+atom, +list(number), +positive_integer, +list, -list), one_or_error).
	:- info(restore_seasonality/5, [
		comment is 'Adds or multiplies a seasonal cycle, advancing phase through missing positions without instantiating them.',
		argnames is ['Method', 'Factors', 'StartPhase', 'Values', 'RestoredValues'],
		exceptions is [
			'The method, phase, factor, or values list is unbound' - instantiation_error,
			'A factor or value is not numeric' - type_error(number, 'Value'),
			'Factors or values are not lists' - type_error(list, 'List'),
			'The factors are empty' - domain_error(non_empty_series, 'Factors'),
			'The method is invalid' - domain_error(seasonal_adjustment_method, 'Method'),
			'The phase is not an integer' - type_error(integer, 'StartPhase'),
			'The phase is not positive' - domain_error(positive_integer, 'StartPhase'),
			'The phase exceeds the cycle length' - domain_error(seasonal_phase, 'StartPhase'),
			'A multiplicative factor is not positive' - domain_error(positive_number, 'Value'),
			'Restoration arithmetic fails' - evaluation_error('Error')
		]
	]).

	restore_seasonality(Method, Factors, StartPhase, Values, Restored) :-
		seasonal_check_method(Method), check_series(Factors, Factors),
		context(Context), check(positive_integer, StartPhase, Context),
		normalize_missing_series(Values, Normalized, _Missing),
		length(Factors, Frequency),
		(	StartPhase =< Frequency ->
			true
		;	domain_error(seasonal_phase, StartPhase)
		),
		(	Method == multiplicative ->
			seasonal_positive_values(Factors)
		;	true
		),
		Skip is StartPhase - 1,
		length(Prefix, Skip),
		append(Prefix, Suffix, Factors),
		seasonal_restore_values(Normalized, Method, Suffix, Factors, Restored).

	seasonal_autocorrelation_test(Series, Frequency, Statistic, Seasonal) :-
		series_observation_summary(Series, Length, KnownCount, Sum),
		(	Length > 0 ->
			true
		;	domain_error(non_empty_series, Series)
		),
		check_frequency(Frequency),
		(	KnownCount =:= Length ->
			seasonal_complete_autocorrelation_test(Series, Frequency, Statistic, Seasonal)
		;	(	Frequency > 1, Length > 2 * Frequency, KnownCount > 2 * Frequency ->
				Mean is Sum / KnownCount,
				seasonal_gap_centered(Series, Mean, Centered, Variance),
				(	Variance > 0, seasonal_gap_lags(1, Frequency, Centered, Variance, 0.0, Earlier, Last) ->
					Statistic is abs(Last) / sqrt((1.0 + 2.0 * Earlier) / KnownCount),
					( Statistic > 1.6448536269514722 -> Seasonal = true; Seasonal = false )
				;	Statistic = 0.0, Seasonal = false
				)
			;	Statistic = 0.0, Seasonal = false
			)
		).

	:- private(seasonal_gap_centered/4).
	:- mode(seasonal_gap_centered(+list, +number, -list, -number), one_or_error).
	:- info(seasonal_gap_centered/4, [
		comment is 'Centers known values without removing gaps.',
		argnames is ['Series', 'Mean', 'Centered', 'Variance'],
		exceptions is [
			'Centering arithmetic fails' - evaluation_error('Error')
		]
	]).

	seasonal_gap_centered([], _, [], 0.0) :-
		!.
	seasonal_gap_centered([Value| Values], Mean, [Centered| Rest], Variance) :-
		seasonal_gap_centered(Values, Mean, Rest, Variance0),
		(	var(Value) ->
			Variance = Variance0
		;	Centered is Value - Mean,
			Variance is Variance0 + Centered * Centered
		).

	:- private(seasonal_gap_lags/7).
	:- mode(seasonal_gap_lags(+positive_integer, +positive_integer, +list, +number, +number, -number, -number), zero_or_one_or_error).
	:- info(seasonal_gap_lags/7, [
		comment is 'Accumulates original-lag scores, failing when fewer than two pairs are available.',
		argnames is ['Lag', 'Frequency', 'Centered', 'Variance', 'Squares0', 'Squares', 'Last'],
		exceptions is [
			'Autocorrelation arithmetic fails' - evaluation_error('Error')
		]
	]).

	seasonal_gap_lags(Lag, Frequency, Centered, Variance, Squares0, Squares, Last) :-
		length(Skip, Lag),
		append(Skip, Suffix, Centered),
		seasonal_gap_covariance(Suffix, Centered, Covariance, Pairs), Pairs >= 2,
		Score is Covariance / Variance,
		(	Lag =:= Frequency ->
			Squares = Squares0,
			Last = Score
		;	Squares1 is Squares0 + Score * Score,
			Next is Lag + 1,
			seasonal_gap_lags(Next, Frequency, Centered, Variance, Squares1, Squares, Last)
		).

	:- private(seasonal_gap_covariance/4).
	:- mode(seasonal_gap_covariance(+list, +list, -number, -non_negative_integer), one_or_error).
	:- info(seasonal_gap_covariance/4, [
		comment is 'Sums centered products and counts pairs with both positions known.',
		argnames is ['Suffix', 'Series', 'Covariance', 'Pairs'],
		exceptions is [
			'Covariance arithmetic fails' - evaluation_error('Error')
		]
	]).

	seasonal_gap_covariance([], _, 0.0, 0) :-
		!.
	seasonal_gap_covariance([First| Firsts], [Second| Seconds], Covariance, Pairs) :-
		seasonal_gap_covariance(Firsts, Seconds, Covariance0, Pairs0),
		(	(var(First); var(Second)) ->
			Covariance = Covariance0,
			Pairs = Pairs0
		;	Covariance is Covariance0 + First * Second,
			Pairs is Pairs0 + 1
		).

	classical_seasonal_adjustment(Series, Frequency, Method, Adjusted, Factors) :-
		series_observation_summary(Series, Length, KnownCount, _Sum),
		(	Length > 0 ->
			true
		;	domain_error(non_empty_series, Series)
		),
		check_frequency(Frequency), seasonal_check_method(Method),
		(	Frequency >= 2 ->
			true
		;	domain_error(seasonal_frequency, Frequency)
		),
		Minimum is 2 * Frequency, check_series_length(Series, Series, Minimum),
		(	KnownCount =:= Length ->
			classical_complete_seasonal_adjustment(Series, Frequency, Method, Adjusted, Factors)
		;	indexed_series_observations(Series, _, Known),
			(	Method == multiplicative ->
				seasonal_positive_values(Known)
			;	true
			),
			length(Window, Frequency),
			append(Window, AfterWindow, Series),
			indexed_series_observations(Window, _, KnownWindow),
			sum(KnownWindow, WindowSum),
			length(KnownWindow, WindowCount),
			WindowMissing is Frequency - WindowCount,
			seasonal_gap_windows(AfterWindow, Series, Frequency, window(WindowSum,WindowMissing), Means),
			(	Frequency mod 2 =:= 0 ->
				seasonal_gap_even(Means, Trends)
			;	Trends = Means
			),
			Half is Frequency // 2, length(Prefix, Half),
			append(Prefix, Interior, Series),
			seasonal_gap_deviations(Trends, Interior, Method, Deviations),
			length(InitialBuckets, Frequency),
			seasonal_empty_buckets(InitialBuckets),
			seasonal_fold_cycles(Deviations, InitialBuckets, Buckets),
			Split is Frequency - Half,
			length(Leading, Split),
			append(Leading, Trailing, Buckets),
			append(Trailing, Leading, PhaseBuckets),
			seasonal_check_buckets(PhaseBuckets, 1),
			seasonal_bucket_means(PhaseBuckets, RawFactors),
			sum(RawFactors, FactorSum),
			FactorMean is FactorSum / Frequency,
			seasonal_normalize(RawFactors, Method, FactorMean, Factors),
			seasonal_inverse_factors(Factors, Method, Inverse),
			restore_seasonality(Method, Inverse, 1, Series, Adjusted)
		).

	:- private(seasonal_gap_windows/5).
	:- mode(seasonal_gap_windows(+list, +list, +positive_integer, +compound, -list), one_or_error).
	:- info(seasonal_gap_windows/5, [
		comment is 'Computes moving averages with rolling sums and missing counts, marking incomplete windows.',
		argnames is ['Entering', 'Leaving', 'Frequency', 'Window', 'Means'],
		exceptions is [
			'Moving-average arithmetic fails' - evaluation_error('Error')
		]
	]).

	seasonal_gap_windows(Entering, Leaving, Frequency, window(Sum,Missing), [Mean| Means]) :-
		(	Missing =:= 0 ->
			Mean is Sum / Frequency
		;	true
		),
		(	Entering == [] ->
			Means = []
		;	Entering = [New| News],
			Leaving = [Old| Olds],
			(	var(Old) ->
				OldValue = 0,
				OldMissing = 1
			;	OldValue = Old,
				OldMissing = 0
			),
			(	var(New) ->
				NewValue = 0,
				NewMissing = 1
			;	NewValue = New,
				NewMissing = 0
			),
			Sum1 is Sum - OldValue + NewValue,
			Missing1 is Missing - OldMissing + NewMissing,
			seasonal_gap_windows(News, Olds, Frequency, window(Sum1,Missing1), Means)
		).

	:- private(seasonal_gap_even/2).
	:- mode(seasonal_gap_even(+list, -list), one_or_error).
	:- info(seasonal_gap_even/2, [
		comment is 'Centers adjacent complete windows, requiring the full even-period support.',
		argnames is ['Means', 'Trends'],
		exceptions is [
			'Centering arithmetic fails' - evaluation_error('Error')
		]
	]).

	seasonal_gap_even([_], []) :-
		!.
	seasonal_gap_even([First,Second| Values], [Mean| Means]) :-
		(	(var(First); var(Second)) ->
			true
		;	Mean is (First + Second) / 2.0
		),
		seasonal_gap_even([Second| Values], Means).

	:- private(seasonal_gap_deviations/4).
	:- mode(seasonal_gap_deviations(+list, +list, +atom, -list), one_or_error).
	:- info(seasonal_gap_deviations/4, [
		comment is 'Computes only known deviations with complete trend support, retaining phase positions.',
		argnames is ['Trends', 'Values', 'Method', 'Deviations'],
		exceptions is [
			'A multiplicative trend is not positive' - domain_error(positive_number, 'Value'),
			'Detrending arithmetic fails' - evaluation_error('Error')
		]
	]).

	seasonal_gap_deviations([], _, _, []).
	seasonal_gap_deviations([Trend| Trends], [Value| Values], Method, [Deviation| Deviations]) :-
		(	(var(Trend); var(Value)) ->
			true
		;	Method == additive ->
			Deviation is Value - Trend
		;	seasonal_positive_values([Trend]),
			Deviation is Value / Trend
		),
		seasonal_gap_deviations(Trends, Values, Method, Deviations).

	seasonal_fold_cycles([], Buckets, Buckets) :-
		!.
	seasonal_fold_cycles(Values, Buckets0, Buckets) :-
		seasonal_accumulate_cycle(Values, Buckets0, Buckets1, Rest),
		seasonal_fold_cycles(Rest, Buckets1, Buckets).

	:- private(seasonal_accumulate_cycle/4).
	:- mode(seasonal_accumulate_cycle(+list, +list(compound), -list(compound), -list), one_or_error).
	:- info(seasonal_accumulate_cycle/4, [
		comment is 'Advances a phase bucket for every position, skipping missing contributions.',
		argnames is ['Values', 'Buckets', 'Updated', 'Rest'],
		exceptions is [
			'Phase accumulation fails' - evaluation_error('Error')
		]
	]).

	seasonal_accumulate_cycle([], Buckets, Buckets, []) :-
		!.
	seasonal_accumulate_cycle(Values, [], [], Values) :-
		!.
	seasonal_accumulate_cycle([Value| Values], [bucket(Sum,Count)| Buckets], [bucket(Sum1,Count1)| Updated], Rest) :-
		(	var(Value) ->
			Sum1 = Sum,
			Count1 = Count
		;	Sum1 is Sum + Value,
			Count1 is Count + 1
		),
		seasonal_accumulate_cycle(Values, Buckets, Updated, Rest).

	:- private(seasonal_check_buckets/2).
	:- mode(seasonal_check_buckets(+list(compound), +positive_integer), one_or_error).
	:- info(seasonal_check_buckets/2, [
		comment is 'Reports the lowest absolute phase lacking an estimable seasonal deviation.',
		argnames is ['Buckets', 'Phase'],
		exceptions is [
			'A phase has no valid deviation' - domain_error(insufficient_seasonal_phase_observations, 'Phase'),
			'Phase arithmetic fails' - evaluation_error('Error')
		]
	]).

	seasonal_check_buckets([], _).
	seasonal_check_buckets([bucket(_,Count)| Buckets], Phase) :-
		(	Count > 0 ->
			true
		;	domain_error(insufficient_seasonal_phase_observations, Phase)
		),
		Next is Phase + 1,
		seasonal_check_buckets(Buckets, Next).

	seasonal_restore_values([], _, _, _, []) :-
		!.
	seasonal_restore_values(Values, Method, [], Cycle, Restored) :-
		!,
		seasonal_restore_values(Values, Method, Cycle, Cycle, Restored).
	seasonal_restore_values([Value| Values], Method, [Factor| Factors], Cycle, [Restored| Rest]) :-
		(	var(Value) ->
			true
		;	Method == additive ->
			Restored is Value + Factor
		;	Restored is Value * Factor
		),
		seasonal_restore_values(Values, Method, Factors, Cycle, Rest).

	cycle_take(_, _, 0, []) :-
		!.
	cycle_take([], Cycle, Horizon, Forecasts) :-
		!,
		cycle_take(Cycle, Cycle, Horizon, Forecasts).
	cycle_take([Value| Rest], Cycle, Horizon, [Value| Forecasts]) :-
		Horizon > 0,
		Horizon1 is Horizon - 1,
		cycle_take(Rest, Cycle, Horizon1, Forecasts).

:- end_category.
