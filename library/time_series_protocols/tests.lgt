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
		date is 2026-10-02,
		comment is 'Smoke tests for the "time_series_protocols" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1, variant/2
	]).

	cover(forecaster_common).

	cleanup :-
		^^clean_file('test_output.pl').

	test(normalize_missing_variables, deterministic) :-
		Series = [10,Missing,14,16], copy_term(Series, Before),
		^^normalize_missing_series(Series, [10,Internal,14,16], Shared),
		var(Internal), Internal == Shared, Internal \== Missing, var(Missing), variant(Series, Before),
		^^indexed_series_observations(Series, [1,3,4], [10,14,16]).

	test(normalize_shared_missing, deterministic) :-
		^^normalize_missing_series([Value,2,Value], [First,2,Last], Shared),
		var(Value), var(Shared), First == Shared, Last == Shared, Value \== Shared.

	test(normalize_empty, deterministic(Normalized == [])) :-
		^^normalize_missing_series([], Normalized, _).

	test(normalize_numeric, deterministic(Normalized == [1,2])) :-
		^^normalize_missing_series([1,2], Normalized, _).

	test(normalize_atom_rejected, error(type_error(number, missing))) :-
		^^normalize_missing_series([1,missing], _, _).

	test(normalize_compound_rejected, error(type_error(number, absent(value)))) :-
		^^normalize_missing_series([1,absent(value)], _, _).

	test(indexed_missing_positions, deterministic) :-
		^^indexed_series_observations([_,10,_,14,_], [2,4], [10,14]).

	test(indexed_missing_all, deterministic) :-
		^^indexed_series_observations([_,_], [], []).

	test(indexed_atom_rejected, error(type_error(number, missing))) :-
		^^indexed_series_observations([1,missing], _, _).

	test(residual_fits_complete, deterministic) :-
		^^residual_fitted_values([10,12,14,16], [2,3,3.5], [Anchor,Second,Third,Fourth]),
		var(Anchor), Second =~= 10.0, Third =~= 11.0, Fourth =~= 12.5.

	test(residual_fits_gaps, deterministic) :-
		Series = [Leading,10,Gap,14,16,Trailing], copy_term(Series, Before),
		^^residual_fitted_values(Series, [4,4], Values),
		Values = [First,Anchor,Missing,Fourth,Fifth,Last],
		variant(Series, Before), variant(Values, [_,_,_,10,12,_]),
		var(First), var(Anchor), var(Missing), var(Last),
		First \== Anchor, First \== Missing, First \== Last,
		Anchor \== Missing, Anchor \== Last, Missing \== Last,
		First \== Leading, Missing \== Gap, Last \== Trailing,
		Fourth =~= 10.0, Fifth =~= 12.0.

	test(residual_fits_shared_input, deterministic) :-
		^^residual_fitted_values([Missing,10,Missing,14,Missing], [4], [First,Anchor,Gap,Fit,Last]),
		var(Missing), var(First), var(Anchor), var(Gap), var(Last),
		First \== Missing, Gap \== Missing, Last \== Missing,
		First \== Gap, First \== Last, Gap \== Last, Fit =~= 10.0.

	test(residual_fits_empty, deterministic(Values == [])) :-
		^^residual_fitted_values([], [], Values).

	test(residual_fits_all_missing, deterministic) :-
		^^residual_fitted_values([Input,Input], [], [First,Second]),
		var(Input), var(First), var(Second), First \== Second,
		First \== Input, Second \== Input.

	test(residual_fits_single_known, deterministic) :-
		^^residual_fitted_values([_,10,_], [], Values), variant(Values, [_,_,_]).

	test(residual_fits_negative_error, deterministic(Fit =~= 12.0)) :-
		^^residual_fitted_values([12,10], [-2], [_,Fit]).

	test(residual_fits_too_few, error(domain_error(residual_count, []))) :-
		^^residual_fitted_values([10,12], [], _).

	test(residual_fits_too_many, error(domain_error(residual_count, [2,3]))) :-
		^^residual_fitted_values([10,12], [2,3], _).

	test(residual_fits_empty_extra, error(domain_error(residual_count, [1]))) :-
		^^residual_fitted_values([], [1], _).

	test(residual_fits_missing_extra, error(domain_error(residual_count, [1]))) :-
		^^residual_fitted_values([_,_], [1], _).

	test(residual_fits_variable_series, error(instantiation_error)) :-
		^^residual_fitted_values(_, [], _).

	test(residual_fits_open_series, error(instantiation_error)) :-
		^^residual_fitted_values([10| _], [], _).

	test(residual_fits_improper_series, error(type_error(list, [10|bad]))) :-
		^^residual_fitted_values([10|bad], [], _).

	test(residual_fits_atom_observation, error(type_error(number, missing))) :-
		^^residual_fitted_values([10,missing], [], _).

	test(residual_fits_compound_observation, error(type_error(number, absent(value)))) :-
		^^residual_fitted_values([10,absent(value)], [], _).

	test(residual_fits_variable_residuals, error(instantiation_error)) :-
		^^residual_fitted_values([10,12], _, _).

	test(residual_fits_open_residuals, error(instantiation_error)) :-
		^^residual_fitted_values([10,12], [2| _], _).

	test(residual_fits_variable_residual, error(instantiation_error)) :-
		^^residual_fitted_values([10,12], [_], _).

	test(residual_fits_atom_residual, error(type_error(number, bad))) :-
		^^residual_fitted_values([10,12], [bad], _).

	test(residual_history_complete, deterministic) :-
		^^valid_residual_history([2,3,3.5], [2,3,4], 1, 4, 3).

	test(residual_history_gaps, deterministic) :-
		^^valid_residual_history([4,4], [3,4], 1, 5, 2).

	test(residual_history_leading, deterministic) :-
		^^valid_residual_history([-2], [4], 2, 5, 1).

	test(residual_history_empty, deterministic) :-
		^^valid_residual_history([], [], 1, 1, 0).

	test(residual_history_negative_errors, deterministic) :-
		^^valid_residual_history([-2,0,3], [2,3,4], 1, 4, 3).

	test(residual_history_unequal_lengths, fail) :-
		^^valid_residual_history([4,4], [3], 1, 4, 2).

	test(residual_history_wrong_count, fail) :-
		^^valid_residual_history([4,4], [3,4], 1, 4, 1).

	test(residual_history_pairs_rejected, fail) :-
		^^valid_residual_history([3-4], [3], 1, 4, 1).

	test(residual_history_bad_error, fail) :-
		^^valid_residual_history([bad], [2], 1, 2, 1).

	test(residual_history_bad_index, fail) :-
		^^valid_residual_history([2], [2.0], 1, 2, 1).

	test(residual_history_anchor_index, fail) :-
		^^valid_residual_history([2], [1], 1, 2, 1).

	test(residual_history_beyond_length, fail) :-
		^^valid_residual_history([2], [3], 1, 2, 1).

	test(residual_history_duplicate_index, fail) :-
		^^valid_residual_history([2,3], [2,2], 1, 3, 2).

	test(residual_history_decreasing_index, fail) :-
		^^valid_residual_history([2,3], [3,2], 1, 3, 2).

	test(residual_history_bad_controls, deterministic) :-
		\+ ^^valid_residual_history([], [], 0, 1, 0),
		\+ ^^valid_residual_history([], [], 2, 1, 0),
		\+ ^^valid_residual_history([], [], 1, 1, -1),
		\+ ^^valid_residual_history([], [], bad, 1, 0),
		\+ ^^valid_residual_history([], [], 1, bad, 0),
		\+ ^^valid_residual_history([], [], 1, 1, bad).

	test(residual_history_nonbinding, deterministic) :-
		History = history([Error], [Index], Anchor, Length, Count), copy_term(History, Before),
		\+ ^^valid_residual_history([Error], [Index], Anchor, Length, Count), variant(History, Before),
		\+ ^^valid_residual_history([2], [2], Anchor, 2, 1), var(Anchor),
		\+ ^^valid_residual_history([2], [2], 1, Length, 1), var(Length),
		\+ ^^valid_residual_history([2], [2], 1, 2, Count), var(Count).

	test(residual_history_open_lists, deterministic) :-
		History = history([2| Errors], [2| Indices]), copy_term(History, Before),
		\+ ^^valid_residual_history([2| Errors], [2], 1, 2, 1),
		\+ ^^valid_residual_history([2], [2| Indices], 1, 2, 1), variant(History, Before).

	test(residual_history_improper_lists, deterministic) :-
		\+ ^^valid_residual_history([2|bad], [2], 1, 2, 1),
		\+ ^^valid_residual_history([2], [2|bad], 1, 2, 1).

	test(classical_additive_even, deterministic) :-
		^^classical_seasonal_adjustment([8,12,8,12,8,12], 2, additive, Adjusted, [First,Second]),
		First =~= -2.0, Second =~= 2.0, check_ten(Adjusted).

	test(classical_multiplicative_even, deterministic) :-
		^^classical_seasonal_adjustment([5,15,5,15,5,15], 2, multiplicative, Adjusted, [First,Second]),
		First =~= 0.5, Second =~= 1.5, check_ten(Adjusted).

	test(classical_additive_odd, deterministic) :-
		^^classical_seasonal_adjustment([8,10,12,8,10,12,8], 3, additive, Adjusted, [First,Second,Third]),
		First =~= -2.0, Second =~= 0.0, Third =~= 2.0, check_ten(Adjusted).

	test(classical_two_cycles, deterministic) :-
		^^classical_seasonal_adjustment([8,12,8,12], 2, additive, Adjusted, _), check_ten(Adjusted).

	test(classical_short, error(domain_error(series_length, [1,2,3]))) :-
		^^classical_seasonal_adjustment([1,2,3], 2, additive, _, _).

	test(classical_nonpositive, error(domain_error(positive_number, 0))) :-
		^^classical_seasonal_adjustment([0,2,0,2], 2, multiplicative, _, _).

	test(classical_frequency_one, error(domain_error(seasonal_frequency, 1))) :-
		^^classical_seasonal_adjustment([1,2], 1, additive, _, _).

	test(classical_invalid_method, error(domain_error(seasonal_adjustment_method, bad))) :-
		^^classical_seasonal_adjustment([1,2,1,2], 2, bad, _, _).

	test(seasonal_restore_offset, deterministic) :-
		^^restore_seasonality(additive, [-2,0,2], 2, [10,10,10,10], [First,Second,Third,Fourth]),
		First =~= 10.0, Second =~= 12.0, Third =~= 8.0, Fourth =~= 10.0.

	test(seasonal_restore_multiply, deterministic) :-
		^^restore_seasonality(multiplicative, [0.5,1.5], 2, [10,10,10], [First,Second,Third]),
		First =~= 15.0, Second =~= 5.0, Third =~= 15.0.

	test(seasonal_restore_empty, deterministic(Restored == [])) :-
		^^restore_seasonality(additive, [1,2], 1, [], Restored).

	test(seasonal_restore_bad_phase, error(domain_error(seasonal_phase, 3))) :-
		^^restore_seasonality(additive, [1,2], 3, [10], _).

	test(seasonal_test_constant, deterministic(Seasonal == false)) :-
		^^seasonal_autocorrelation_test([7,7,7,7,7,7], 2, _, Seasonal).

	test(seasonal_test_short, deterministic(Seasonal == false)) :-
		^^seasonal_autocorrelation_test([8,12,8,12], 2, _, Seasonal).

	test(seasonal_test_periodic, deterministic(Seasonal == true)) :-
		^^seasonal_autocorrelation_test([8,12,8,12,8,12,8,12,8,12,8,12], 2, _, Seasonal).

	test(seasonal_test_frequency_one, deterministic(Seasonal == false)) :-
		^^seasonal_autocorrelation_test([1,2,3], 1, _, Seasonal).

	test(seasonal_test_statistic, deterministic) :-
		^^seasonal_autocorrelation_test([8,12,8,12,8,12,8,12,8,12,8,12], 2, Statistic, true),
		Expected is (10/12) / sqrt((1 + 2*(11/12)*(11/12))/12), Statistic =~= Expected.

	test(seasonal_test_nonseasonal, deterministic(Seasonal == false)) :-
		^^seasonal_autocorrelation_test([1,2,3,4,5,6], 2, _, Seasonal).

	test(classical_additive_trend, deterministic) :-
		^^classical_seasonal_adjustment([0,3,2,5,4,7], 2, additive, [First,Second,Third,Fourth,Fifth,Sixth], [Low,High]),
		Low =~= -1.0, High =~= 1.0,
		First =~= 1.0, Second =~= 2.0, Third =~= 3.0, Fourth =~= 4.0, Fifth =~= 5.0, Sixth =~= 6.0.

	test(classical_four_phases, deterministic) :-
		^^classical_seasonal_adjustment([7,9,11,13,7,9,11,13,7], 4, additive, Adjusted, [First,Second,Third,Fourth]),
		First =~= -3.0, Second =~= -1.0, Third =~= 1.0, Fourth =~= 3.0, check_ten(Adjusted).

	test(classical_multiplicative_odd, deterministic) :-
		^^classical_seasonal_adjustment([5,10,15,5,10,15], 3, multiplicative, Adjusted, [First,Second,Third]),
		First =~= 0.5, Second =~= 1.0, Third =~= 1.5, check_ten(Adjusted).

	test(seasonal_variable_method, error(instantiation_error)) :-
		^^classical_seasonal_adjustment([1,2,1,2], 2, _, _, _).

	test(seasonal_variable_frequency, error(instantiation_error)) :-
		^^seasonal_autocorrelation_test([1,2,3], _, _, _).

	test(seasonal_nonnumeric_value, error(type_error(number, bad))) :-
		^^seasonal_autocorrelation_test([1,bad,3], 2, _, _).

	test(seasonal_empty_series, error(domain_error(non_empty_series, []))) :-
		^^seasonal_autocorrelation_test([], 2, _, _).

	test(seasonal_restore_empty_factors, error(domain_error(non_empty_series, []))) :-
		^^restore_seasonality(additive, [], 1, [1], _).

	test(seasonal_restore_nonpositive_factor, error(domain_error(positive_number, 0))) :-
		^^restore_seasonality(multiplicative, [0,1], 1, [1], _).

	test(seasonal_restore_zero_phase, error(domain_error(positive_integer, 0))) :-
		^^restore_seasonality(additive, [1,2], 0, [1], _).

	test(seasonal_missing_acf_oracle, deterministic) :-
		^^seasonal_autocorrelation_test([8,_,8,12,8,12], 2, Statistic, false),
		Expected is (17/30) / sqrt((1 + 2*(3/5)*(3/5))/5), Statistic =~= Expected.

	test(seasonal_missing_no_pairs, deterministic(Statistic =~= 0.0)) :-
		^^seasonal_autocorrelation_test([1,_,2,_,3,_,4,_,5], 2, Statistic, false).

	test(seasonal_missing_later_lag_no_pairs, deterministic(Statistic =~= 0.0)) :-
		^^seasonal_autocorrelation_test([1,2,3,_,_,4,_,_,5,_,_,_], 2, Statistic, false).

	test(seasonal_missing_constant, deterministic(Statistic =~= 0.0)) :-
		^^seasonal_autocorrelation_test([7,_,7,7,7,7], 2, Statistic, false).

	test(seasonal_missing_few_known, deterministic(Statistic =~= 0.0)) :-
		^^seasonal_autocorrelation_test([8,12,_,_,8,12,_], 2, Statistic, false).

	test(seasonal_missing_all, deterministic(Statistic =~= 0.0)) :-
		^^seasonal_autocorrelation_test([_,_,_,_,_,_], 2, Statistic, false).

	test(classical_missing_even, deterministic) :-
		^^classical_seasonal_adjustment([8,12,8,12,_,12,8,12,8,12,8,12], 2, additive, Adjusted, [First,Second]),
		First =~= -2.0, Second =~= 2.0, check_missing_ten(Adjusted).

	test(classical_missing_odd, deterministic) :-
		^^classical_seasonal_adjustment([8,10,12,8,10,12,_,10,12,8,10,12,8,10,12], 3, additive, Adjusted, [First,Second,Third]),
		First =~= -2.0, Second =~= 0.0, Third =~= 2.0, check_missing_ten(Adjusted).

	test(classical_missing_four, deterministic) :-
		^^classical_seasonal_adjustment([7,9,11,13,7,9,11,13,_,9,11,13,7,9,11,13,7,9,11,13], 4, additive, Adjusted, [First,Second,Third,Fourth]),
		First =~= -3.0, Second =~= -1.0, Third =~= 1.0, Fourth =~= 3.0, check_missing_ten(Adjusted).

	test(classical_missing_multiplicative, deterministic) :-
		^^classical_seasonal_adjustment([5,15,5,15,_,15,5,15,5,15,5,15], 2, multiplicative, Adjusted, [First,Second]),
		First =~= 0.5, Second =~= 1.5, check_missing_ten(Adjusted).

	test(classical_missing_unbound_nonbinding, deterministic) :-
		Series = [8,12,8,12,Value,12,8,12,8,12], copy_term(Series, Before),
		^^classical_seasonal_adjustment(Series, 2, additive, Adjusted, _),
		var(Value), variant(Series, Before), check_missing_ten(Adjusted).

	test(classical_missing_no_window, error(domain_error(insufficient_seasonal_phase_observations, 1))) :-
		^^classical_seasonal_adjustment([8,_,8,12], 2, additive, _, _).

	test(classical_missing_unestimable_phase, error(domain_error(insufficient_seasonal_phase_observations, 2))) :-
		^^classical_seasonal_adjustment([8,12,_,12,8,12], 2, additive, _, _).

	test(classical_missing_zero, error(domain_error(positive_number, 0))) :-
		^^classical_seasonal_adjustment([0,_,0,2,0,2], 2, multiplicative, _, _).

	test(classical_missing_short, error(domain_error(series_length, [1,_,2]))) :-
		^^classical_seasonal_adjustment([1,_,2], 2, additive, _, _).

	test(restore_missing_phase, deterministic) :-
		^^restore_seasonality(additive, [-2,0,2], 2, [10,Gap,10,10], [First,RestoredGap,Third,Fourth]),
		var(Gap), var(RestoredGap),
		First =~= 10.0, Third =~= 8.0, Fourth =~= 10.0.

	test(restore_missing_multiplicative, deterministic) :-
		^^restore_seasonality(multiplicative, [0.5,1.5], 1, [Gap,10,10], [RestoredGap,Second,Third]),
		var(Gap), var(RestoredGap),
		Second =~= 15.0, Third =~= 5.0.

	test(restore_missing_empty_invalid_phase, error(domain_error(seasonal_phase, 3))) :-
		^^restore_seasonality(additive, [-2,2], 3, [], _).

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

	test(accumulate_forecast_error_initial, deterministic(Totals == forecast_error_totals(1,6,36))) :-
		^^accumulate_forecast_error(6, 0, forecast_error_totals(0,0,0), Totals).

	test(accumulate_forecast_error_negative, deterministic(Totals == forecast_error_totals(2,9,45))) :-
		^^accumulate_forecast_error(0, 3, forecast_error_totals(1,6,36), Totals).

	test(accumulate_forecast_error_zero, deterministic(Totals == forecast_error_totals(1,0,0))) :-
		^^accumulate_forecast_error(0, 0, forecast_error_totals(0,0,0), Totals).

	test(forecast_error_metrics_zero, deterministic) :-
		^^forecast_error_metrics(forecast_error_totals(3,0,0), MAE, RMSE),
		MAE =~= 0.0, RMSE =~= 0.0.

	test(forecast_error_metrics_list_equivalence, deterministic) :-
		^^accumulate_forecast_error(0, 0, forecast_error_totals(0,0,0), Totals1),
		^^accumulate_forecast_error(6, 0, Totals1, Totals2),
		^^accumulate_forecast_error(0, 3, Totals2, Totals3),
		^^accumulate_forecast_error(10, 3, Totals3, Totals),
		Totals == forecast_error_totals(4,16,94),
		^^forecast_error_metrics(Totals, MAE, RMSE),
		^^mean_absolute_error([0,6,0,10], [0,0,3,3], ListMAE),
		^^root_mean_squared_error([0,6,0,10], [0,0,3,3], ListRMSE),
		MAE =~= ListMAE, RMSE =~= ListRMSE.

	test(accumulate_forecast_error_fractional, deterministic) :-
		^^accumulate_forecast_error(-1.5, 0.5, forecast_error_totals(0,0,0), forecast_error_totals(1,Absolute,Squared)),
		Absolute =~= 2.0, Squared =~= 4.0.

	test(accumulate_forecast_error_invalid_actual, error(type_error(number, bad))) :-
		^^accumulate_forecast_error(bad, 0, forecast_error_totals(0,0,0), _).

	test(accumulate_forecast_error_invalid_prediction, error(type_error(number, bad))) :-
		^^accumulate_forecast_error(0, bad, forecast_error_totals(0,0,0), _).

	test(accumulate_forecast_error_missing, error(instantiation_error)) :-
		^^accumulate_forecast_error(_, 0, forecast_error_totals(0,0,0), _).

	test(forecast_error_metrics_empty, error(domain_error(positive_integer, 0))) :-
		^^forecast_error_metrics(forecast_error_totals(0,0,0), _, _).

	test(forecast_error_metrics_non_integer_count, error(type_error(integer, one))) :-
		^^forecast_error_metrics(forecast_error_totals(one,0,0), _, _).

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

	test(updated_observation_diagnostics_numeric, deterministic) :-
		Diagnostics = [first(kept), training_series_length(3), options([]), observed_count(2), missing_count(1), update_count(0), last(kept)],
		^^updated_observation_diagnostics(Diagnostics, 5, Updated),
		assertion(Updated == [first(kept), training_series_length(4), options([]), observed_count(3), missing_count(1), update_count(1), last(kept)]),
		assertion(Diagnostics == [first(kept), training_series_length(3), options([]), observed_count(2), missing_count(1), update_count(0), last(kept)]).

	test(updated_observation_diagnostics_missing, deterministic) :-
		Diagnostics = [training_series_length(3), update_count(1), observed_count(2), missing_count(1)],
		^^updated_observation_diagnostics(Diagnostics, Missing, Updated),
		var(Missing),
		assertion(Updated == [training_series_length(4), update_count(2), observed_count(2), missing_count(2)]),
		assertion(Diagnostics == [training_series_length(3), update_count(1), observed_count(2), missing_count(1)]).

	test(updated_observation_diagnostics_invalid, error(type_error(number, bad))) :-
		^^updated_observation_diagnostics([training_series_length(1), update_count(0), observed_count(1), missing_count(0)], bad, _).

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

	check_missing_ten([]) :-
		!.
	check_missing_ten([Value| Values]) :-
		(	var(Value) ->
			true
		;	Value =~= 10.0
		),
		check_missing_ten(Values).

	check_ten([]).
	check_ten([Value| Values]) :-
		Value =~= 10.0, check_ten(Values).

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
