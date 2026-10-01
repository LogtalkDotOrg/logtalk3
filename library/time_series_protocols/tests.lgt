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
	extends(lgtunit),
	imports(forecaster_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-29,
		comment is 'Smoke tests for the "time_series_protocols" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	cover(forecaster_common).

	cleanup :-
		^^clean_file('test_output.pl').

	% time_series_dataset_protocol tests

	test(linear_trend_observation_2, deterministic(Observations == [1-10, 2-12, 3-14, 4-16, 5-18, 6-20])) :-
		findall(Index-Value, linear_trend::observation(Index, Value), Observations).

	test(linear_trend_series_length_1, deterministic(Length == 6)) :-
		linear_trend::series_length(Length).

	test(linear_trend_frequency_1, fail) :-
		linear_trend::frequency(_Frequency).

	test(seasonal_series_frequency_1, deterministic(Frequency == 4)) :-
		seasonal_series::frequency(Frequency).

	% forecaster_common shared helper tests

	test(dataset_series_linear_trend, deterministic(Series == [10, 12, 14, 16, 18, 20])) :-
		^^dataset_series(linear_trend, Series).

	test(dataset_series_gap_index, error(domain_error(series_index_sequence, gap_index))) :-
		^^dataset_series(gap_index, _Series).

	test(dataset_series_inconsistent_length, error(consistency_error(series_length, 4, 3))) :-
		^^dataset_series(inconsistent_series_length, _Series).

	test(dataset_series_zero_length, error(domain_error(positive_integer, 0))) :-
		^^dataset_series(zero_series_length, _Series).

	test(dataset_series_non_integer_length, error(type_error(integer, one))) :-
		^^dataset_series(non_integer_series_length, _Series).

	test(check_series_non_numeric_value_invalid, error(type_error(number, bad))) :-
		^^dataset_series(non_numeric_value, Series),
		^^check_series(non_numeric_value, Series).

	test(check_series_non_numeric_value_valid, deterministic) :-
		^^dataset_series(non_numeric_value, Series),
		^^check_series(non_numeric_value, Series, [number,atom]).

	test(check_series_length_short_series, error(domain_error(series_length, short_series))) :-
		^^dataset_series(short_series, Series),
		^^check_series_length(short_series, Series, 3).

	test(check_series_length_negative_minimum, error(domain_error(non_negative_integer, -1))) :-
		^^check_series_length(linear_trend, [10, 12], -1).

	test(difference_series_1, deterministic(Differences == [2, 2, 2, 2, 2])) :-
		^^difference_series([10, 12, 14, 16, 18, 20], Differences).

	test(difference_series_empty, error(domain_error(non_empty_series, []))) :-
		^^difference_series([], _Differences).

	test(integrate_series_1, deterministic(Series == [22, 24, 26])) :-
		^^integrate_series([2, 2, 2], 20, Series).

	test(lagged_rows_order_2, deterministic(Rows == [[12, 10]-14, [14, 12]-16, [16, 14]-18, [18, 16]-20])) :-
		^^lagged_rows([10, 12, 14, 16, 18, 20], 2, Rows).

	test(lagged_rows_too_short, error(domain_error(series_length, [10, 12]))) :-
		^^lagged_rows([10, 12], 2, _Rows).

	test(lagged_rows_maximum_valid_order, deterministic(Rows == [[18, 16, 14, 12, 10]-20])) :-
		^^lagged_rows([10, 12, 14, 16, 18, 20], 5, Rows).

	test(lagged_rows_zero_order, error(domain_error(positive_integer, 0))) :-
		^^lagged_rows([10, 12], 0, _Rows).

	test(lagged_rows_negative_order, error(domain_error(positive_integer, -1))) :-
		^^lagged_rows([10, 12], -1, _Rows).

	test(lagged_rows_non_integer_order, error(type_error(integer, one))) :-
		^^lagged_rows([10, 12], one, _Rows).

	test(mean_absolute_error_1, deterministic(MAE =~= 1.0)) :-
		^^mean_absolute_error([1, 2, 3, 4], [2, 2, 2, 2], MAE).

	test(mean_absolute_error_empty, error(domain_error(non_empty_series, []))) :-
		^^mean_absolute_error([], [], _MAE).

	test(mean_absolute_error_length_mismatch, error(consistency_error(same_length, [1, 2], [1]))) :-
		^^mean_absolute_error([1, 2], [1], _MAE).

	test(mean_absolute_error_non_numeric, error(type_error(number, bad))) :-
		^^mean_absolute_error([1, bad], [1, 2], _MAE).

	test(root_mean_squared_error_1, true) :-
		^^root_mean_squared_error([1, 2, 3, 4], [2, 2, 2, 2], RMSE),
		Delta is abs(RMSE - 1.224744871391589),
		Delta < 0.000001.

	test(root_mean_squared_error_empty, error(domain_error(non_empty_series, []))) :-
		^^root_mean_squared_error([1], [], _RMSE).

	test(root_mean_squared_error_length_mismatch, error(consistency_error(same_length, [1], [1, 2]))) :-
		^^root_mean_squared_error([1], [1, 2], _RMSE).

	test(root_mean_squared_error_partial_list, error(instantiation_error)) :-
		^^root_mean_squared_error([1| _], [1], _RMSE).

	test(mean_absolute_percentage_error_1, deterministic(MAPE =~= 25.0)) :-
		^^mean_absolute_percentage_error([100, 200, 400, 800], [125, 150, 500, 600], MAPE).

	test(mean_absolute_percentage_error_zero_divisor, error(evaluation_error(zero_divisor))) :-
		^^mean_absolute_percentage_error([0, 1], [1, 1], _MAPE).

	test(mean_absolute_percentage_error_empty, error(domain_error(non_empty_series, []))) :-
		^^mean_absolute_percentage_error([], [], _MAPE).

	test(mean_absolute_percentage_error_length_mismatch, error(consistency_error(same_length, [1, 2], [1]))) :-
		^^mean_absolute_percentage_error([1, 2], [1], _MAPE).

	test(naive_forecast_1, deterministic(Forecasts == [20, 20, 20])) :-
		^^naive_forecast([10, 12, 14, 16, 18, 20], 3, Forecasts).

	test(constant_forecast, deterministic(Forecasts == [-2, -2, -2])) :-
		^^constant_forecast(-2, 3, Forecasts).

	test(constant_forecast_zero, deterministic(Forecasts == [])) :-
		^^constant_forecast(1.5, 0, Forecasts).

	test(constant_forecast_singleton, deterministic(Value =~= 1.5)) :-
		^^constant_forecast(1.5, 1, [Value]).

	test(constant_forecast_negative, error(domain_error(non_negative_integer, -1))) :-
		^^constant_forecast(1, -1, _).

	test(linear_trend_forecast, deterministic(Forecasts == [22, 24, 26])) :-
		^^linear_trend_forecast(20, 2, 3, Forecasts).

	test(linear_trend_forecast_zero, deterministic(Forecasts == [])) :-
		^^linear_trend_forecast(20, 2, 0, Forecasts).

	test(linear_trend_forecast_fractional, deterministic(Value =~= -2.5)) :-
		^^linear_trend_forecast(-2.0, -0.5, 1, [Value]).

	test(linear_trend_forecast_variable_horizon, error(instantiation_error)) :-
		^^linear_trend_forecast(20, 2, _, _).

	test(linear_trend_forecast_non_integer, error(type_error(integer, one))) :-
		^^linear_trend_forecast(20, 2, one, _).

	test(observation_summary_empty, deterministic) :-
		^^series_observation_summary([], 0, 0, 0).

	test(observation_summary_missing, deterministic) :-
		^^series_observation_summary([10, Missing, 14], 3, 2, 24),
		var(Missing).

	test(observation_summary_all_missing, deterministic) :-
		^^series_observation_summary([First, Last], 2, 0, 0),
		var(First), var(Last).

	test(observation_summary_fractional, deterministic(Sum =~= -1.5)) :-
		^^series_observation_summary([-2, 0.5], 2, 2, Sum).

	test(observation_summary_long, deterministic) :-
		^^constant_forecast(1, 10000, Series),
		^^series_observation_summary(Series, 10000, 10000, 10000).

	test(linear_trend_forecast_invalid_slope, error(type_error(number, bad))) :-
		^^linear_trend_forecast(20, bad, 1, _).

	test(observation_summary_partial, error(instantiation_error)) :-
		^^series_observation_summary([1| _], _, _, _).

	test(observation_summary_invalid, error(type_error(number, bad))) :-
		^^series_observation_summary([1, bad], _, _, _).

	test(check_observation_missing, deterministic(var(Missing))) :-
		^^check_observation(Missing).

	test(replace_diagnostic, deterministic(Updated == [first(1), count(3), last(2)])) :-
		^^replace_diagnostic(count, 3, [first(1), count(2), last(2)], Updated).

	test(replace_diagnostic_absent, fail) :-
		^^replace_diagnostic(count, 3, [first(1)], _).

	test(naive_forecast_zero_horizon, deterministic(Forecasts == [])) :-
		^^naive_forecast([10, 12], 0, Forecasts).

	test(naive_forecast_negative_horizon, error(domain_error(non_negative_integer, -1))) :-
		^^naive_forecast([10, 12], -1, _Forecasts).

	test(naive_forecast_non_integer_horizon, error(type_error(integer, one))) :-
		^^naive_forecast([10, 12], one, _Forecasts).

	test(seasonal_naive_forecast_1, deterministic(Forecasts == [10, 20, 15, 5, 10, 20])) :-
		^^dataset_series(seasonal_series, Series),
		^^seasonal_naive_forecast(Series, 4, 6, Forecasts).

	test(seasonal_naive_forecast_too_short, error(domain_error(series_length, [10, 12]))) :-
		^^seasonal_naive_forecast([10, 12], 4, 2, _Forecasts).

	test(seasonal_naive_forecast_zero_horizon, deterministic(Forecasts == [])) :-
		^^seasonal_naive_forecast([10, 20, 15, 5], 4, 0, Forecasts).

	test(seasonal_naive_forecast_zero_frequency, error(domain_error(positive_integer, 0))) :-
		^^seasonal_naive_forecast([10, 20], 0, 2, _Forecasts).

	test(seasonal_naive_forecast_negative_frequency, error(domain_error(positive_integer, -1))) :-
		^^seasonal_naive_forecast([10, 20], -1, 2, _Forecasts).

	test(seasonal_naive_forecast_non_integer_frequency, error(type_error(integer, one))) :-
		^^seasonal_naive_forecast([10, 20], one, 2, _Forecasts).

	test(base_forecaster_diagnostics_1, deterministic(Diagnostics == [model(sample_forecaster), training_series_length(6), options([sample_option(enabled)])])) :-
		^^base_forecaster_diagnostics(sample_forecaster, 6, [sample_option(enabled)], [], Diagnostics).

	test(valid_forecaster_metadata_2_true, deterministic) :-
		^^valid_forecaster_metadata(sample_forecaster, [model(sample_forecaster), training_series_length(6), options([])]).

	test(valid_forecaster_metadata_2_false, fail) :-
		^^valid_forecaster_metadata(other_model, [model(sample_forecaster)]).

	% sample_forecaster end-to-end tests

	test(sample_forecaster_learn_2, deterministic(ground(Forecaster))) :-
		sample_forecaster::learn(linear_trend, Forecaster).

	test(sample_forecaster_learn_3, deterministic(ground(Forecaster))) :-
		sample_forecaster::learn(linear_trend, Forecaster, [sample_option(enabled)]).

	test(sample_forecaster_valid_forecaster_1, deterministic(sample_forecaster::valid_forecaster(Forecaster))) :-
		sample_forecaster::learn(linear_trend, Forecaster).

	test(sample_forecaster_invalid_forecaster_1, fail) :-
		sample_forecaster::valid_forecaster(sample_forecaster(not_a_list, [model(sample_forecaster), training_series_length(6), options([sample_option(enabled)])])).

	test(sample_forecaster_variable, error(instantiation_error)) :-
		sample_forecaster::check_forecaster(_Forecaster).

	test(sample_forecaster_non_numeric_series, fail) :-
		sample_forecaster::valid_forecaster(sample_forecaster([10, bad], [model(sample_forecaster), training_series_length(2), options([sample_option(enabled)])])).

	test(sample_forecaster_missing_model_metadata, fail) :-
		sample_forecaster::valid_forecaster(sample_forecaster([10, 12], [training_series_length(2), options([sample_option(enabled)])])).

	test(sample_forecaster_missing_options_metadata, fail) :-
		sample_forecaster::valid_forecaster(sample_forecaster([10, 12], [model(sample_forecaster), training_series_length(2)])).

	test(sample_forecaster_forecast_3, deterministic(Forecasts == [20, 20, 20])) :-
		sample_forecaster::learn(linear_trend, Forecaster),
		sample_forecaster::forecast(Forecaster, 3, Forecasts).

	test(sample_forecaster_forecast_zero_horizon, deterministic(Forecasts == [])) :-
		sample_forecaster::learn(linear_trend, Forecaster),
		sample_forecaster::forecast(Forecaster, 0, Forecasts).

	test(sample_forecaster_forecast_negative_horizon, error(domain_error(non_negative_integer, -1))) :-
		sample_forecaster::learn(linear_trend, Forecaster),
		sample_forecaster::forecast(Forecaster, -1, _Forecasts).

	test(sample_forecaster_diagnostics_2, true(Options == [sample_option(enabled)])) :-
		sample_forecaster::learn(linear_trend, Forecaster),
		sample_forecaster::diagnostics(Forecaster, _Diagnostics),
		sample_forecaster::diagnostic(Forecaster, model(sample_forecaster)),
		sample_forecaster::diagnostic(Forecaster, training_series_length(6)),
		sample_forecaster::forecaster_options(Forecaster, Options).

	test(sample_forecaster_diagnostic_2, true) :-
		sample_forecaster::learn(linear_trend, Forecaster),
		sample_forecaster::diagnostic(Forecaster, model(sample_forecaster)).

	test(sample_forecaster_forecaster_options_2, deterministic(Options == [sample_option(enabled)])) :-
		sample_forecaster::learn(linear_trend, Forecaster),
		sample_forecaster::forecaster_options(Forecaster, Options).

	test(sample_forecaster_export_to_clauses_4, deterministic(Clause == forecast_model(sample_forecaster([10, 12, 14, 16, 18, 20], [model(sample_forecaster), training_series_length(6), options([sample_option(enabled)])])))) :-
		sample_forecaster::learn(linear_trend, Forecaster),
		sample_forecaster::export_to_clauses(linear_trend, Forecaster, forecast_model, [Clause]).

	test(sample_forecaster_export_to_file_4_header, deterministic(HeaderLine == '% exported forecaster predicate: forecast_model/1')) :-
		^^file_path('test_output.pl', File),
		sample_forecaster::learn(linear_trend, Forecaster),
		sample_forecaster::export_to_file(linear_trend, Forecaster, forecast_model, File),
		first_header_line(File, HeaderLine).

	test(sample_forecaster_export_to_file_4_metadata, deterministic(HeaderLines == ['% exported forecaster predicate: forecast_model/1', '% training dataset: linear_trend', '% training series length: 6', '% diagnostics: [model(sample_forecaster),training_series_length(6),options([sample_option(enabled)])]', '% forecast_model(Forecaster)'])) :-
		^^file_path('test_output.pl', File),
		sample_forecaster::learn(linear_trend, Forecaster),
		sample_forecaster::export_to_file(linear_trend, Forecaster, forecast_model, File),
		header_lines(File, HeaderLines).

	test(sample_forecaster_export_to_file_4_loadable, deterministic(LoadedForecaster == sample_forecaster([10, 12, 14, 16, 18, 20], [model(sample_forecaster), training_series_length(6), options([sample_option(enabled)])]))) :-
		^^file_path('test_output.pl', File),
		sample_forecaster::learn(linear_trend, Forecaster),
		sample_forecaster::export_to_file(linear_trend, Forecaster, forecast_model, File),
		logtalk_load(File),
		{forecast_model(LoadedForecaster)}.

	test(sample_forecaster_export_to_file_4_closes_on_error, error(domain_error(series_index_sequence, gap_index))) :-
		^^file_path('test_output.pl', File),
		sample_forecaster::learn(linear_trend, Forecaster),
		catch(
			sample_forecaster::export_to_file(gap_index, Forecaster, forecast_model, File),
			Error,
			(open(File, append, Stream), close(Stream), throw(Error))
		).

	test(sample_forecaster_print_forecaster_1, deterministic) :-
		^^suppress_text_output,
		sample_forecaster::learn(linear_trend, Forecaster),
		sample_forecaster::print_forecaster(Forecaster).

	test(sample_forecaster_learn_2_non_numeric_value, error(type_error(number, bad))) :-
		sample_forecaster::learn(non_numeric_value, _Forecaster).

	test(sample_forecaster_learn_2_gap_index, error(domain_error(series_index_sequence, gap_index))) :-
		sample_forecaster::learn(gap_index, _Forecaster).

	% auxiliary predicates

	header_lines(File, Lines) :-
		open(File, read, Stream),
		read_header_lines(Stream, Lines),
		close(Stream).

	first_header_line(File, Line) :-
		header_lines(File, [Line| _]).

	read_header_lines(Stream, Lines) :-
		read_line_atom(Stream, Line),
		(	Line == end_of_file ->
			Lines = []
		;	sub_atom(Line, 0, 1, _, '%') ->
			Lines = [Line| RestLines],
			read_header_lines(Stream, RestLines)
		;	Lines = []
		).

	read_line_atom(Stream, Line) :-
		get_code(Stream, Code),
		(	Code == -1 ->
			Line = end_of_file
		;	read_line_codes(Code, Stream, Codes),
			atom_codes(Line, Codes)
		).

	read_line_codes(-1, _Stream, []) :-
		!.
	read_line_codes(10, _Stream, []) :-
		!.
	read_line_codes(13, Stream, Codes) :-
		!,
		get_code(Stream, NextCode),
		(	NextCode == 10 ->
			Codes = []
		;	read_line_codes(NextCode, Stream, Codes)
		).
	read_line_codes(Code, Stream, [Code| Codes]) :-
		get_code(Stream, NextCode),
		read_line_codes(NextCode, Stream, Codes).

:- end_object.
