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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-01,
		comment is 'Tests for the baseline forecasting library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, variant/2
	]).

	:- uses(list, [
		memberchk/2, append/3
	]).

	cover(baseline_forecasting).

	cleanup :-
		^^clean_file('test_output.pl').

	test(naive, deterministic(Forecasts == [20, 20, 20])) :-
		baseline_forecasting::learn(linear_trend, Forecaster),
		baseline_forecasting::forecast(Forecaster, 3, Forecasts).

	test(mean, deterministic) :-
		baseline_forecasting::learn(linear_trend, Forecaster, [model(mean)]),
		baseline_forecasting::forecast(Forecaster, 3, [First, Second, Third]),
		First =~= 15.0, Second =~= 15.0, Third =~= 15.0.

	test(drift, deterministic) :-
		baseline_forecasting::learn(linear_trend, Forecaster, [model(drift)]),
		baseline_forecasting::forecast(Forecaster, 3, [First, Second, Third]),
		First =~= 22.0, Second =~= 24.0, Third =~= 26.0.

	test(seasonal_naive, deterministic(Forecasts == [10, 20, 15, 5, 10, 20])) :-
		baseline_forecasting::learn(seasonal_series, Forecaster, [model(seasonal_naive)]),
		baseline_forecasting::forecast(Forecaster, 6, Forecasts).

	test(seasonal_offset, deterministic(Forecasts == [20, 15, 5, 10, 20])) :-
		baseline_forecasting::learn(baseline_series([10,20,15,5,10], 4), Forecaster, [model(seasonal_naive)]),
		baseline_forecasting::forecast(Forecaster, 5, Forecasts).

	test(frequency_override, deterministic(Forecasts == [15, 5, 15, 5])) :-
		baseline_forecasting::learn(seasonal_series, Forecaster, [model(seasonal_naive), frequency(2)]),
		baseline_forecasting::forecast(Forecaster, 4, Forecasts),
		baseline_forecasting::forecaster_options(Forecaster, [model(seasonal_naive), frequency(2)]).

	test(frequency_one, deterministic(Forecasts == [3,3])) :-
		baseline_forecasting::learn(baseline_series([3], bad), Forecaster, [model(seasonal_naive), frequency(1)]),
		baseline_forecasting::forecast(Forecaster, 2, Forecasts).

	test(missing_frequency, error(domain_error(seasonal_frequency, linear_trend))) :-
		baseline_forecasting::learn(linear_trend, _, [model(seasonal_naive)]).

	test(invalid_dataset_frequency, error(type_error(integer, bad))) :-
		baseline_forecasting::learn(baseline_series([1,2], bad), _, [model(seasonal_naive)]).

	test(irrelevant_frequency, error(domain_error(baseline_forecasting_option, frequency(2)))) :-
		baseline_forecasting::learn(linear_trend, _, [frequency(2)]).

	test(singleton, deterministic(Forecasts == [7])) :-
		baseline_forecasting::learn(baseline_series([7], none), Forecaster),
		baseline_forecasting::forecast(Forecaster, 1, Forecasts).

	test(drift_short, error(domain_error(series_length, baseline_series([7], none)))) :-
		baseline_forecasting::learn(baseline_series([7], none), _, [model(drift)]).

	test(seasonal_short, error(domain_error(series_length, linear_trend))) :-
		baseline_forecasting::learn(linear_trend, _, [model(seasonal_naive), frequency(7)]).

	test(mean_missing, deterministic(Value =~= 12.0)) :-
		baseline_forecasting::learn(baseline_series([10,_,14], none), Forecaster, [model(mean)]),
		baseline_forecasting::forecast(Forecaster, 1, [Value]).

	test(drift_missing_interior, deterministic(Value =~= 16.0)) :-
		baseline_forecasting::learn(baseline_series([10,_,14], none), Forecaster, [model(drift)]),
		baseline_forecasting::forecast(Forecaster, 1, [Value]).

	test(all_missing, error(domain_error(insufficient_observations, baseline_series([_,_], none)))) :-
		baseline_forecasting::learn(baseline_series([_,_], none), _).

	test(missing_zero_horizon, deterministic(Forecasts == [])) :-
		baseline_forecasting::learn(baseline_series([10,_], none), Forecaster),
		baseline_forecasting::forecast(Forecaster, 0, Forecasts).

	test(missing_final, error(domain_error(missing_observation, _))) :-
		baseline_forecasting::learn(baseline_series([10,_], none), Forecaster),
		baseline_forecasting::forecast(Forecaster, 1, _).

	test(seasonal_partial_horizon, deterministic(Forecasts == [10,20])) :-
		baseline_forecasting::learn(baseline_series([10,20,_,5], 4), Forecaster, [model(seasonal_naive)]),
		baseline_forecasting::forecast(Forecaster, 2, Forecasts).

	test(seasonal_missing_slot, error(domain_error(missing_observation, _))) :-
		baseline_forecasting::learn(baseline_series([10,20,_,5], 4), Forecaster, [model(seasonal_naive)]),
		baseline_forecasting::forecast(Forecaster, 3, _).

	test(update_naive, deterministic(Forecasts == [22,22])) :-
		baseline_forecasting::learn(linear_trend, Original),
		baseline_forecasting::update(Original, 22, Updated),
		baseline_forecasting::forecast(Updated, 2, Forecasts),
		baseline_forecasting::forecast(Original, 1, [20]).

	test(update_mean, deterministic(Value =~= 16.0)) :-
		baseline_forecasting::learn(linear_trend, Original, [model(mean)]),
		baseline_forecasting::update(Original, 22, Updated),
		baseline_forecasting::forecast(Updated, 1, [Value]).

	test(update_drift, deterministic(Value =~= 24.0)) :-
		baseline_forecasting::learn(linear_trend, Original, [model(drift)]),
		baseline_forecasting::update(Original, 22, Updated),
		baseline_forecasting::forecast(Updated, 1, [Value]).

	test(update_seasonal, deterministic(Forecasts == [20,15,5,10])) :-
		baseline_forecasting::learn(seasonal_series, Original, [model(seasonal_naive)]),
		baseline_forecasting::update(Original, 10, Updated),
		baseline_forecasting::forecast(Updated, 4, Forecasts).

	test(update_missing_counts, deterministic) :-
		baseline_forecasting::learn(linear_trend, Original, [model(mean)]),
		baseline_forecasting::update(Original, Missing, Updated),
		var(Missing),
		baseline_forecasting::diagnostics(Updated, Diagnostics),
		memberchk(observed_count(6), Diagnostics),
		memberchk(missing_count(1), Diagnostics),
		memberchk(training_series_length(7), Diagnostics),
		memberchk(update_count(1), Diagnostics).

	test(missing_recovery, deterministic(Forecasts == [14])) :-
		baseline_forecasting::learn(baseline_series([10,_], none), Original),
		baseline_forecasting::update(Original, 14, Updated),
		baseline_forecasting::forecast(Updated, 1, Forecasts).

	test(drift_missing_first, error(domain_error(missing_observation, _))) :-
		baseline_forecasting::learn(baseline_series([_,12,14], none), Original, [model(drift)]),
		baseline_forecasting::update(Original, 16, Updated),
		baseline_forecasting::forecast(Updated, 1, _).

	test(negative_horizon, error(domain_error(non_negative_integer, -1))) :-
		baseline_forecasting::learn(linear_trend, Forecaster),
		baseline_forecasting::forecast(Forecaster, -1, _).

	test(variable_forecaster, error(instantiation_error)) :-
		baseline_forecasting::check_forecaster(_).

	test(invalid_forecaster, fail) :-
		baseline_forecasting::valid_forecaster(baseline_forecaster(naive, naive_state(bad), [])).

	test(option_hooks, deterministic) :-
		baseline_forecasting::default_option(model(naive)),
		baseline_forecasting::valid_option(model(drift)),
		baseline_forecasting::valid_option(frequency(4)).

	test(export_clauses, deterministic(Clause == saved_model(Forecaster))) :-
		baseline_forecasting::learn(linear_trend, Forecaster),
		baseline_forecasting::export_to_clauses(linear_trend, Forecaster, saved_model, [Clause]).

	test(print_forecaster, deterministic) :-
		^^suppress_text_output,
		baseline_forecasting::learn(linear_trend, Forecaster),
		baseline_forecasting::print_forecaster(Forecaster).

	test(gap_index, error(domain_error(series_index_sequence, gap_index))) :-
		baseline_forecasting::learn(gap_index, _).

	test(non_numeric, error(type_error(number, bad))) :-
		baseline_forecasting::learn(non_numeric_value, _).

	test(empty_series, error(domain_error(non_empty_series, baseline_series([], none)))) :-
		baseline_forecasting::learn(baseline_series([], none), _).

	test(inconsistent_length, error(consistency_error(series_length, 4, 3))) :-
		baseline_forecasting::learn(inconsistent_series_length, _).

	test(zero_declared_length, error(domain_error(positive_integer, 0))) :-
		baseline_forecasting::learn(zero_series_length, _).

	test(non_integer_declared_length, error(type_error(integer, one))) :-
		baseline_forecasting::learn(non_integer_series_length, _).

	test(constant_drift, deterministic(Value =~= -2.5)) :-
		baseline_forecasting::learn(baseline_series([-2.5,-2.5], none), Forecaster, [model(drift)]),
		baseline_forecasting::forecast(Forecaster, 1, [Value]).

	test(fractional_mean, deterministic(Value =~= -0.75)) :-
		baseline_forecasting::learn(baseline_series([-2,0.5], none), Forecaster, [model(mean)]),
		baseline_forecasting::forecast(Forecaster, 1, [Value]).

	test(singleton_mean, deterministic(Value =~= 7.0)) :-
		baseline_forecasting::learn(baseline_series([7], none), Forecaster, [model(mean)]),
		baseline_forecasting::forecast(Forecaster, 1, [Value]).

	test(drift_minimum, deterministic(Value =~= 14.0)) :-
		baseline_forecasting::learn(baseline_series([10,12], none), Forecaster, [model(drift)]),
		baseline_forecasting::forecast(Forecaster, 1, [Value]).

	test(variable_horizon, error(instantiation_error)) :-
		baseline_forecasting::learn(linear_trend, Forecaster),
		baseline_forecasting::forecast(Forecaster, _, _).

	test(non_integer_horizon, error(type_error(integer, one))) :-
		baseline_forecasting::learn(linear_trend, Forecaster),
		baseline_forecasting::forecast(Forecaster, one, _).

	test(invalid_model, error(domain_error(option, model(other)))) :-
		baseline_forecasting::learn(linear_trend, _, [model(other)]).

	test(invalid_frequency_option, error(domain_error(option, frequency(0)))) :-
		baseline_forecasting::learn(linear_trend, _, [model(seasonal_naive), frequency(0)]).

	test(zero_dataset_frequency, error(domain_error(positive_integer, 0))) :-
		baseline_forecasting::learn(baseline_series([1,2], 0), _, [model(seasonal_naive)]).

	test(variable_dataset_frequency, error(instantiation_error)) :-
		baseline_forecasting::learn(baseline_series([1,2], _), _, [model(seasonal_naive)]).

	test(variable_options, error(instantiation_error)) :-
		baseline_forecasting::learn(linear_trend, _, _).

	test(partial_options, error(instantiation_error)) :-
		baseline_forecasting::learn(linear_trend, _, [model(mean)| _]).

	test(non_list_options, error(type_error(list, bad))) :-
		baseline_forecasting::learn(linear_trend, _, bad).

	test(variable_option, error(instantiation_error)) :-
		baseline_forecasting::learn(linear_trend, _, [_]).

	test(non_compound_option, error(type_error(compound, bad))) :-
		baseline_forecasting::learn(linear_trend, _, [bad]).

	test(update_invalid_observation, error(type_error(number, bad))) :-
		baseline_forecasting::learn(linear_trend, Forecaster),
		baseline_forecasting::update(Forecaster, bad, _).

	test(update_no_options_arity, fail) :-
		baseline_forecasting::current_predicate(update/4).

	test(missing_input_preserved, deterministic) :-
		baseline_forecasting::learn(baseline_series([10,Missing,14], none), Forecaster),
		var(Missing),
		baseline_forecasting::check_forecaster(Forecaster),
		baseline_forecasting::forecast(Forecaster, 1, [14]),
		var(Missing).

	test(validation_preserves_missing_state, deterministic) :-
		baseline_forecasting::learn(baseline_series([10,_], none), Forecaster),
		Forecaster = baseline_forecaster(naive, naive_state(Missing), _),
		baseline_forecasting::check_forecaster(Forecaster),
		var(Missing).

	test(update_missing_isolation, deterministic) :-
		baseline_forecasting::learn(linear_trend, Forecaster),
		baseline_forecasting::update(Forecaster, Missing, Updated),
		Updated = baseline_forecaster(naive, naive_state(Stored), _),
		var(Missing), var(Stored), Stored \== Missing.

	test(update_original_missing_isolation, deterministic) :-
		baseline_forecasting::learn(baseline_series([_,12], none), Forecaster, [model(drift)]),
		Forecaster = baseline_forecaster(drift, drift_state(First, _), _),
		baseline_forecasting::update(Forecaster, 14, Updated),
		Updated = baseline_forecaster(drift, drift_state(NewFirst, _), _),
		var(First), var(NewFirst), First \== NewFirst.

	test(drift_final_recovery, deterministic(Value =~= 18.0)) :-
		baseline_forecasting::learn(baseline_series([10,_], none), Forecaster, [model(drift)]),
		baseline_forecasting::update(Forecaster, 14, Updated),
		baseline_forecasting::update(Updated, 16, Final),
		baseline_forecasting::forecast(Final, 1, [Value]).

	test(seasonal_missing_update_phase, deterministic(Forecasts == [20,15,5])) :-
		baseline_forecasting::learn(seasonal_series, Forecaster, [model(seasonal_naive)]),
		baseline_forecasting::update(Forecaster, _, Updated),
		baseline_forecasting::forecast(Updated, 3, Forecasts).

	test(seasonal_missing_update_required, error(domain_error(missing_observation, _))) :-
		baseline_forecasting::learn(seasonal_series, Forecaster, [model(seasonal_naive)]),
		baseline_forecasting::update(Forecaster, _, Updated),
		baseline_forecasting::forecast(Updated, 4, _).

	test(update_equals_learning_naive, deterministic) :-
		check_update_equivalence(naive).

	test(update_equals_learning_mean, deterministic) :-
		check_update_equivalence(mean).

	test(update_equals_learning_drift, deterministic) :-
		check_update_equivalence(drift).

	test(update_equals_learning_seasonal, deterministic) :-
		check_update_equivalence(seasonal_naive).

	test(all_models_zero, deterministic) :-
		zero_horizons([naive, mean, drift, seasonal_naive]).

	test(invalid_state_unbound_structure, deterministic(var(State))) :-
		baseline_forecasting::learn(linear_trend, baseline_forecaster(Method, _, Diagnostics)),
		\+ baseline_forecasting::valid_forecaster(baseline_forecaster(Method, State, Diagnostics)).

	test(invalid_method_unbound, deterministic(var(Method))) :-
		baseline_forecasting::learn(linear_trend, baseline_forecaster(_, State, Diagnostics)),
		\+ baseline_forecasting::valid_forecaster(baseline_forecaster(Method, State, Diagnostics)).

	test(invalid_metadata_unbound, deterministic(var(Diagnostics))) :-
		\+ baseline_forecasting::valid_forecaster(baseline_forecaster(naive, naive_state(20), Diagnostics)).

	test(invalid_counts, fail) :-
		baseline_forecasting::valid_forecaster(baseline_forecaster(naive, naive_state(20), [
			model(baseline_forecasting), training_series_length(6), options([model(naive)]),
			method(naive), observed_count(5), missing_count(0), update_count(0)
		])).

	test(invalid_mean_state, fail) :-
		baseline_forecasting::learn(linear_trend, baseline_forecaster(mean, _, Diagnostics), [model(mean)]),
		baseline_forecasting::valid_forecaster(baseline_forecaster(mean, mean_state(90, 0), Diagnostics)).

	test(invalid_seasonal_state, fail) :-
		baseline_forecasting::learn(seasonal_series, baseline_forecaster(seasonal_naive, _, Diagnostics), [model(seasonal_naive)]),
		baseline_forecasting::valid_forecaster(baseline_forecaster(seasonal_naive, seasonal_naive_state(4, [1,2]), Diagnostics)).

	test(invalid_missing_count, fail) :-
		baseline_forecasting::learn(linear_trend, baseline_forecaster(naive, _, Diagnostics)),
		baseline_forecasting::valid_forecaster(baseline_forecaster(naive, naive_state(_), Diagnostics)).

	test(invalid_forecast_zero, error(domain_error(forecaster, bad))) :-
		baseline_forecasting::forecast(bad, 0, _).

	test(invalid_update_forecaster, error(domain_error(forecaster, bad))) :-
		baseline_forecasting::update(bad, 22, _).

	test(diagnostic_enumeration, deterministic(Diagnostics == Enumerated)) :-
		baseline_forecasting::learn(linear_trend, Forecaster),
		baseline_forecasting::diagnostics(Forecaster, Diagnostics),
		findall(Diagnostic, baseline_forecasting::diagnostic(Forecaster, Diagnostic), Enumerated).

	test(valid_forecaster, deterministic) :-
		baseline_forecasting::learn(linear_trend, Forecaster),
		baseline_forecasting::valid_forecaster(Forecaster).

	test(export_invalid_functor, error(type_error(atom, 1))) :-
		baseline_forecasting::learn(linear_trend, Forecaster),
		baseline_forecasting::export_to_clauses(linear_trend, Forecaster, 1, _).

	test(export_file_roundtrip, deterministic) :-
		baseline_forecasting::learn(linear_trend, Forecaster),
		check_export_roundtrip(linear_trend, Forecaster).

	test(export_missing_roundtrip, deterministic) :-
		Dataset = baseline_series([10,_], none),
		baseline_forecasting::learn(Dataset, Forecaster),
		check_export_roundtrip(Dataset, Forecaster).

	check_update_equivalence(Method) :-
		Values = [10,12,14,16],
		baseline_forecasting::learn(baseline_series(Values, 2), Forecaster, [model(Method)]),
		baseline_forecasting::update(Forecaster, Missing, Updated),
		baseline_forecasting::update(Updated, 20, Final),
		append(Values, [Missing,20], FullValues),
		baseline_forecasting::learn(baseline_series(FullValues, 2), Relearned, [model(Method)]),
		Final = baseline_forecaster(Method, State, Diagnostics),
		Relearned = baseline_forecaster(Method, NewState, NewDiagnostics),
		variant(State, NewState),
		without_update_count(Diagnostics, Comparable),
		without_update_count(NewDiagnostics, Comparable),
		var(Missing).

	without_update_count([], []).
	without_update_count([update_count(_)| Diagnostics], Rest) :-
		!,
		without_update_count(Diagnostics, Rest).
	without_update_count([Diagnostic| Diagnostics], [Diagnostic| Rest]) :-
		without_update_count(Diagnostics, Rest).

	zero_horizons([]).
	zero_horizons([Method| Methods]) :-
		baseline_forecasting::learn(baseline_series([10,_], 2), Forecaster, [model(Method)]),
		baseline_forecasting::forecast(Forecaster, 0, []),
		zero_horizons(Methods).

	check_export_roundtrip(Dataset, Forecaster) :-
		^^file_path('test_output.pl', File),
		baseline_forecasting::export_to_file(Dataset, Forecaster, saved_baseline, File),
		open(File, read, Stream),
		catch(read(Stream, Clause), Error, (close(Stream), throw(Error))),
		close(Stream),
		Clause = saved_baseline(Loaded),
		variant(Loaded, Forecaster),
		baseline_forecasting::check_forecaster(Loaded).

:- end_object.
