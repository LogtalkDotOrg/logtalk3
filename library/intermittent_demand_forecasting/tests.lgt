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
		date is 2026-10-02,
		comment is 'Tests for the intermittent-demand forecasting library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		member/2, memberchk/2, length/2, append/3
	]).

	cover(intermittent_demand_forecasting).

	cleanup :-
		^^clean_file('test_output.pl').

	test(default_sba, deterministic(Value =~= 2.096551724137931)) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,0,6,0,10]), Forecaster),
		intermittent_demand_forecasting::forecaster_options(Forecaster, [model(sba), alpha(0.1), beta(0.1), missing(skip)]),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [Value]).

	test(fitted_croston, deterministic((First =~= 0.0, Second =~= 0.0, Third =~= 3.0, Fourth =~= 3.0))) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([0,6,0,10]), [First,Second,Third,Fourth], [model(croston),alpha(0.5),beta(0.5)]).

	test(fitted_sba, deterministic((First =~= 0.0, Second =~= 0.0, Third =~= 2.25, Fourth =~= 2.25))) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([0,6,0,10]), [First,Second,Third,Fourth], [model(sba),alpha(0.5),beta(0.5)]).

	test(fitted_tsb, deterministic((First =~= 0.0, Second =~= 0.0, Third =~= 3.0, Fourth =~= 1.5))) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([0,6,0,10]), [First,Second,Third,Fourth], [model(tsb),alpha(0.5),beta(0.5)]).

	test(fitted_default, deterministic((First =~= 0.0, Second =~= 5.7))) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([6,0]), Values),
		intermittent_demand_forecasting::fitted_values(intermittent_series([6,0]), Values, []),
		Values = [First,Second].

	test(fitted_cold_start, deterministic(Values == [0])) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([6]), Values).

	test(fitted_all_zero, deterministic(Values == [0,0,0])) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([0,0,0]), Values).

	test(fitted_croston_missing_skip, deterministic) :-
		check_fitted_missing(croston, skip, 3.0, 3.0).

	test(fitted_croston_missing_elapsed, deterministic) :-
		check_fitted_missing(croston, elapsed, 2.0, 2.0).

	test(fitted_sba_missing_skip, deterministic) :-
		check_fitted_missing(sba, skip, 2.25, 2.25).

	test(fitted_sba_missing_elapsed, deterministic) :-
		check_fitted_missing(sba, elapsed, 1.5, 1.5).

	test(fitted_tsb_missing_skip, deterministic) :-
		check_fitted_missing(tsb, skip, 3.0, 1.5).

	test(fitted_tsb_missing_elapsed, deterministic) :-
		check_fitted_missing(tsb, elapsed, 2.0, 1.0).

	test(fitted_croston_errors, deterministic) :-
		check_fitted_errors([0,6,0,10], croston, skip).

	test(fitted_sba_errors, deterministic) :-
		check_fitted_errors([0,6,0,10], sba, skip).

	test(fitted_tsb_errors, deterministic) :-
		check_fitted_errors([0,6,0,10], tsb, skip).

	test(fitted_auto_croston, deterministic) :-
		check_fitted_auto([0,6,0,10], croston).

	test(fitted_auto_sba, deterministic) :-
		check_fitted_auto([6,0,6], sba).

	test(fitted_auto_tsb, deterministic) :-
		check_fitted_auto([6,0,0,0], tsb).

	test(fitted_auto_errors, deterministic) :-
		check_fitted_errors([0,_,6,_,0,10], auto, elapsed).

	test(fitted_shared_missing_inputs, deterministic) :-
		Dataset = intermittent_series([Missing,0,Missing]),
		intermittent_demand_forecasting::fitted_values(Dataset, [First,0,Last]),
		var(Missing), var(First), var(Last),
		First \== Missing, Last \== Missing, First \== Last.

	test(fitted_existing_concrete_options, deterministic((First =~= 0.0, Second =~= 4.5, Third =~= 4.5, Fourth =~= 4.5))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Original, [model(auto),alpha(0.5),beta(0.5)]),
		apply_updates([0,0,0], Original, Updated),
		intermittent_demand_forecasting::forecaster_options(Updated, Options),
		intermittent_demand_forecasting::fitted_values(intermittent_series([6,0,0,0]), [First,Second,Third,Fourth], Options).

	test(fitted_empty, error(domain_error(non_empty_series, intermittent_series([])))) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([]), _).

	test(fitted_all_missing, error(domain_error(insufficient_observations, intermittent_series([_,_])))) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([_,_]), _, [model(auto)]).

	test(fitted_negative_demand, error(domain_error(non_negative_number, -1))) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([0,-1,6]), _).

	test(fitted_nonnumeric_demand, error(type_error(number, bad))) :-
		intermittent_demand_forecasting::fitted_values(non_numeric_value, _).

	test(fitted_gap_indices, error(domain_error(series_index_sequence, gap_index))) :-
		intermittent_demand_forecasting::fitted_values(gap_index, _).

	test(fitted_inconsistent_length, error(consistency_error(series_length, 4, 3))) :-
		intermittent_demand_forecasting::fitted_values(inconsistent_series_length, _).

	test(fitted_zero_length, error(domain_error(positive_integer, 0))) :-
		intermittent_demand_forecasting::fitted_values(zero_series_length, _).

	test(fitted_noninteger_length, error(type_error(integer, one))) :-
		intermittent_demand_forecasting::fitted_values(non_integer_series_length, _).

	test(fitted_variable_options, error(instantiation_error)) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([6]), _, _).

	test(fitted_partial_options, error(instantiation_error)) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([6]), _, [model(tsb)| _]).

	test(fitted_variable_option, error(instantiation_error)) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([6]), _, [_]).

	test(fitted_nonlist_options, error(type_error(list, bad))) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([6]), _, bad).

	test(fitted_noncompound_option, error(type_error(compound, bad))) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([6]), _, [bad]).

	test(fitted_invalid_model, error(domain_error(option, model(other)))) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([6]), _, [model(other)]).

	test(fitted_auto_alpha, deterministic(Values == [0])) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([6]), Values, [alpha(auto)]).

	test(fitted_invalid_beta, error(domain_error(option, beta(0)))) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([6]), _, [beta(0)]).

	test(fitted_invalid_missing, error(domain_error(option, missing(zero)))) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([6]), _, [missing(zero)]).

	test(auto_beta_lower_boundary, deterministic(Squared =~= 72.36)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6,0,6]), Forecaster, [model(tsb),alpha(0.37),beta(auto)]),
		intermittent_demand_forecasting::forecaster_options(Forecaster, [model(tsb),alpha(0.37),beta(0.1),missing(skip)]),
		intermittent_demand_forecasting::diagnostics(Forecaster, Diagnostics),
		memberchk(sum_squared_error(Squared), Diagnostics).

	test(auto_joint_croston, deterministic(Forecaster == Expected)) :-
		Dataset = intermittent_series([6,10,10]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [model(croston),alpha(auto),beta(auto)]),
		intermittent_demand_forecasting::learn(Dataset, Expected, [model(croston),alpha(1.0),beta(0.1)]).

	test(auto_joint_sba, deterministic(Forecaster == Expected)) :-
		Dataset = intermittent_series([6,10,10]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [alpha(auto),beta(auto)]),
		intermittent_demand_forecasting::learn(Dataset, Expected, [alpha(1.0),beta(0.1)]).

	test(auto_joint_tsb, deterministic(Forecaster == Expected)) :-
		Dataset = intermittent_series([6,0,0]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [model(tsb),alpha(auto),beta(auto)]),
		intermittent_demand_forecasting::learn(Dataset, Expected, [model(tsb),alpha(0.1),beta(1.0)]).

	test(auto_all_parameters, deterministic(Squared =~= 54.0)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6,0,0]), Forecaster, [model(auto),alpha(auto),beta(auto)]),
		intermittent_demand_forecasting::forecaster_options(Forecaster, [model(sba),alpha(0.1),beta(1.0),missing(skip)]),
		intermittent_demand_forecasting::diagnostics(Forecaster, Diagnostics),
		memberchk(sum_squared_error(Squared), Diagnostics).

	test(auto_all_zero_parameters_tie, deterministic) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,0,0]), Forecaster, [model(auto),alpha(auto),beta(auto)]),
		intermittent_demand_forecasting::forecaster_options(Forecaster, [model(sba),alpha(0.1),beta(0.1),missing(skip)]).

	test(auto_single_positive_parameters_tie, deterministic(Forecaster == Expected)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Forecaster, [model(auto),alpha(auto),beta(auto)]),
		intermittent_demand_forecasting::learn(intermittent_series([6]), Expected).

	test(auto_preserves_integer_coefficient, deterministic) :-
		intermittent_demand_forecasting::learn(intermittent_series([6,0,0]), Forecaster, [model(tsb),alpha(1),beta(auto)]),
		intermittent_demand_forecasting::forecaster_options(Forecaster, [model(tsb),alpha(1),beta(1.0),missing(skip)]).

	test(auto_coefficient_option_hooks, deterministic) :-
		intermittent_demand_forecasting::valid_option(alpha(auto)),
		intermittent_demand_forecasting::valid_option(beta(auto)),
		intermittent_demand_forecasting::default_option(alpha(0.1)),
		intermittent_demand_forecasting::default_option(beta(0.1)).

	test(auto_grid_exhaustive_skip, deterministic) :-
		check_coefficient_grid(skip).

	test(auto_grid_exhaustive_elapsed, deterministic) :-
		check_coefficient_grid(elapsed).

	test(custom_grid_tsb_lower_beta, deterministic(Squared =~= 72.0036)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6,0,6]), Forecaster, [model(tsb),alpha(0.37),beta(auto),coefficient_grid([0.2,0.01,0.1])]),
		intermittent_demand_forecasting::forecaster_options(Forecaster, [model(tsb),alpha(0.37),beta(0.01),missing(skip)]),
		intermittent_demand_forecasting::diagnostics(Forecaster, Diagnostics),
		memberchk(sum_squared_error(Squared), Diagnostics).

	test(custom_grid_explicit_default, deterministic(Forecaster == Expected)) :-
		Dataset = intermittent_series([0,6,0,10]),
		intermittent_demand_forecasting::learn(Dataset, Expected, [model(auto),alpha(auto),beta(auto)]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [model(auto),alpha(auto),beta(auto),coefficient_grid([0.1,0.2,0.5,0.8,1.0])]).

	test(custom_grid_croston_alpha, deterministic(Squared =~= 53.0)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6,10,10]), Forecaster, [model(croston),alpha(auto),beta(0.37),coefficient_grid([0.75,0.25])]),
		intermittent_demand_forecasting::forecaster_options(Forecaster, [model(croston),alpha(0.75),beta(0.37),missing(skip)]),
		intermittent_demand_forecasting::diagnostics(Forecaster, Diagnostics),
		memberchk(sum_squared_error(Squared), Diagnostics).

	test(custom_grid_joint_croston, deterministic(Forecaster == Expected)) :-
		Dataset = intermittent_series([6,10,10]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [model(croston),alpha(auto),beta(auto),coefficient_grid([0.75,0.25])]),
		intermittent_demand_forecasting::learn(Dataset, Expected, [model(croston),alpha(0.75),beta(0.25)]).

	test(custom_grid_joint_sba, deterministic(Forecaster == Expected)) :-
		Dataset = intermittent_series([6,0,0]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [alpha(auto),beta(auto),coefficient_grid([0.8,0.2])]),
		intermittent_demand_forecasting::learn(Dataset, Expected, [alpha(0.2),beta(0.8)]).

	test(custom_grid_joint_tsb, deterministic(Forecaster == Expected)) :-
		Dataset = intermittent_series([6,0,0]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [model(tsb),alpha(auto),beta(auto),coefficient_grid([0.8,0.2])]),
		intermittent_demand_forecasting::learn(Dataset, Expected, [model(tsb),alpha(0.2),beta(0.8)]).

	test(custom_grid_normalized_tie, deterministic(Grid == Snapshot)) :-
		Grid = [0.8,1,0.01,1.0,0.8], Snapshot = Grid,
		intermittent_demand_forecasting::learn(intermittent_series([0,0,0]), Forecaster, [model(auto),alpha(auto),beta(auto),coefficient_grid(Grid)]),
		intermittent_demand_forecasting::forecaster_options(Forecaster, [model(sba),alpha(0.01),beta(0.01),missing(skip)]).

	test(custom_grid_duplicates, deterministic(Forecaster == Expected)) :-
		Dataset = intermittent_series([6,0,6]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [model(auto),alpha(auto),beta(auto),coefficient_grid([0.5,1,0.1,1.0,0.5])]),
		intermittent_demand_forecasting::learn(Dataset, Expected, [model(auto),alpha(auto),beta(auto),coefficient_grid([0.1,0.5,1.0])]).

	test(custom_grid_singleton_integer, deterministic(Forecaster == Expected)) :-
		Dataset = intermittent_series([6,0,6]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [model(auto),alpha(auto),beta(auto),coefficient_grid([1])]),
		intermittent_demand_forecasting::learn(Dataset, Expected, [model(auto),alpha(1.0),beta(1.0)]).

	test(custom_grid_preserves_fixed_integer, deterministic) :-
		intermittent_demand_forecasting::learn(intermittent_series([6,0,0]), Forecaster, [model(tsb),alpha(1),beta(auto),coefficient_grid([0.8,0.01])]),
		intermittent_demand_forecasting::forecaster_options(Forecaster, [model(tsb),alpha(1),beta(0.8),missing(skip)]).

	test(custom_grid_unused_concrete, deterministic(Forecaster == Expected)) :-
		Dataset = intermittent_series([6,0,6]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [model(croston),alpha(1),beta(0.37),coefficient_grid([0.01])]),
		intermittent_demand_forecasting::learn(Dataset, Expected, [model(croston),alpha(1),beta(0.37)]).

	test(custom_grid_unused_auto_method, deterministic(Forecaster == Expected)) :-
		Dataset = intermittent_series([6,0,6]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [model(auto),coefficient_grid([0.01])]),
		intermittent_demand_forecasting::learn(Dataset, Expected, [model(auto)]).

	test(custom_grid_option_hooks, deterministic) :-
		intermittent_demand_forecasting::valid_option(coefficient_grid([0.8,1,0.01,1.0,0.8])),
		intermittent_demand_forecasting::default_option(coefficient_grid([0.1,0.2,0.5,0.8,1.0])),
		intermittent_demand_forecasting::default_options([model(sba),alpha(0.1),beta(0.1),missing(skip),coefficient_grid([0.1,0.2,0.5,0.8,1.0])]).

	test(custom_grid_exhaustive_skip, deterministic) :-
		check_coefficient_grid(skip, [0.01,0.25,0.75], [coefficient_grid([0.75,0.01,0.25,0.01])]).

	test(custom_grid_exhaustive_elapsed, deterministic) :-
		check_coefficient_grid(elapsed, [0.01,0.25,0.75], [coefficient_grid([0.75,0.01,0.25,0.01])]).

	test(custom_grid_more_than_default_candidates, deterministic) :-
		check_coefficient_grid(skip, [0.01,0.1,0.2,0.5,0.8,1.0], [coefficient_grid([0.01,0.1,0.2,0.5,0.8,1])]).

	test(custom_grid_single_positive_tie, deterministic) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Forecaster, [model(auto),alpha(auto),beta(auto),coefficient_grid([0.8,0.01])]),
		intermittent_demand_forecasting::forecaster_options(Forecaster, [model(sba),alpha(0.01),beta(0.01),missing(skip)]).

	test(custom_grid_fitted_values, deterministic((First =~= 0.0, Second =~= 6.0, Third =~= 9.0))) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([6,10,10]), [First,Second,Third], [model(croston),alpha(auto),beta(0.37),coefficient_grid([0.75,0.25])]).

	test(custom_grid_update, deterministic) :-
		intermittent_demand_forecasting::learn(intermittent_series([6,10,10]), Original, [model(croston),alpha(auto),beta(0.37),coefficient_grid([0.75,0.25])]),
		copy_term(Original, Snapshot),
		intermittent_demand_forecasting::update(Original, 20, Updated),
		Original == Snapshot,
		intermittent_demand_forecasting::forecaster_options(Updated, Options),
		Options = [model(croston),alpha(0.75),beta(0.37),missing(skip)],
		intermittent_demand_forecasting::learn(intermittent_series([6,10,10,20]), Expected, Options),
		Updated = intermittent_demand_forecaster(croston, State, Diagnostics),
		Expected = intermittent_demand_forecaster(croston, State, ExpectedDiagnostics),
		without_update_count(Diagnostics, Comparable),
		without_update_count(ExpectedDiagnostics, Comparable),
		intermittent_demand_forecasting::check_forecaster(Updated).

	test(custom_grid_export, deterministic) :-
		Dataset = intermittent_series([6,0,6]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [model(auto),alpha(auto),beta(auto),coefficient_grid([0.8,0.01])]),
		check_export_roundtrip(Dataset, Forecaster).

	test(custom_grid_stored_option_rejected, fail) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), intermittent_demand_forecaster(Method,State,[Model,Length,options(Options)| Rest])),
		append(Options, [coefficient_grid([0.1])], ChangedOptions),
		Changed = intermittent_demand_forecaster(Method,State,[Model,Length,options(ChangedOptions)| Rest]),
		intermittent_demand_forecasting::valid_forecaster(Changed).

	test(custom_grid_invalid_empty, error(domain_error(option, coefficient_grid([])))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [alpha(auto),coefficient_grid([])]).

	test(custom_grid_invalid_variable, error(domain_error(option, coefficient_grid(_)))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [coefficient_grid(_)]).

	test(custom_grid_invalid_atom, error(domain_error(option, coefficient_grid(bad)))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [coefficient_grid(bad)]).

	test(custom_grid_invalid_improper, error(domain_error(option, coefficient_grid([0.1| bad])))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [coefficient_grid([0.1| bad])]).

	test(custom_grid_invalid_entry, error(domain_error(option, coefficient_grid([0.1,bad])))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [coefficient_grid([0.1,bad])]).

	test(custom_grid_invalid_zero, error(domain_error(option, coefficient_grid([0])))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [coefficient_grid([0])]).

	test(custom_grid_invalid_negative, error(domain_error(option, coefficient_grid([-0.1])))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [coefficient_grid([-0.1])]).

	test(custom_grid_invalid_above_one, error(domain_error(option, coefficient_grid([1.1])))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [coefficient_grid([1.1])]).

	test(custom_grid_invalid_fitted, error(domain_error(option, coefficient_grid([])))) :-
		intermittent_demand_forecasting::fitted_values(intermittent_series([6]), _, [beta(auto),coefficient_grid([])]).

	test(custom_grid_variable_not_bound, deterministic) :-
		check_invalid_grid(_).

	test(custom_grid_open_tail_not_bound, deterministic) :-
		check_invalid_grid([0.1| _]).

	test(custom_grid_variable_entry_not_bound, deterministic) :-
		check_invalid_grid([0.1,_]).

	test(auto_stored_alpha_pending, fail) :-
		stored_coefficients([0], auto, 0.1, Forecaster),
		intermittent_demand_forecasting::valid_forecaster(Forecaster).

	test(auto_stored_beta_pending, fail) :-
		stored_coefficients([0], 0.1, auto, Forecaster),
		intermittent_demand_forecasting::valid_forecaster(Forecaster).

	test(auto_stored_coefficients_initialized, fail) :-
		stored_coefficients([6], auto, auto, Forecaster),
		intermittent_demand_forecasting::valid_forecaster(Forecaster).

	test(auto_stored_coefficient_update, error(domain_error(forecaster, _))) :-
		stored_coefficients([6], auto, 0.1, Forecaster),
		intermittent_demand_forecasting::update(Forecaster, 0, _).

	test(auto_partial_coefficient_not_bound, deterministic) :-
		stored_coefficients([6], Alpha, 0.1, Forecaster),
		copy_term(Forecaster, Snapshot),
		\+ intermittent_demand_forecasting::valid_forecaster(Forecaster),
		var(Alpha), lgtunit::variant(Forecaster, Snapshot).

	test(auto_coefficient_fitted_replay, deterministic((First =~= 0.0, Second =~= 6.0, Third =~= 10.0))) :-
		Dataset = intermittent_series([6,10,10]),
		intermittent_demand_forecasting::fitted_values(Dataset, [First,Second,Third], [model(croston),alpha(auto),beta(0.37)]).

	test(auto_coefficient_update, deterministic) :-
		intermittent_demand_forecasting::learn(intermittent_series([6,10,10]), Original, [model(croston),alpha(auto),beta(0.37)]),
		copy_term(Original, Snapshot),
		intermittent_demand_forecasting::update(Original, 20, Updated),
		Original == Snapshot,
		intermittent_demand_forecasting::forecaster_options(Updated, [model(croston),alpha(1.0),beta(0.37),missing(skip)]),
		intermittent_demand_forecasting::learn(intermittent_series([6,10,10,20]), Expected, [model(croston),alpha(1.0),beta(0.37)]),
		Updated = intermittent_demand_forecaster(croston, State, Diagnostics),
		Expected = intermittent_demand_forecaster(croston, State, ExpectedDiagnostics),
		without_update_count(Diagnostics, Comparable),
		without_update_count(ExpectedDiagnostics, Comparable),
		intermittent_demand_forecasting::check_forecaster(Updated).

	test(auto_coefficient_export, deterministic) :-
		Dataset = intermittent_series([6,10,10]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [model(auto),alpha(auto),beta(auto)]),
		check_export_roundtrip(Dataset, Forecaster).

	test(auto_coefficient_all_missing, error(domain_error(insufficient_observations, intermittent_series([_,_])))) :-
		intermittent_demand_forecasting::learn(intermittent_series([_,_]), _, [model(auto),alpha(auto),beta(auto)]).

	test(auto_coefficient_negative_data, error(domain_error(non_negative_number, -1))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6,-1]), _, [alpha(auto),beta(auto)]).

	test(auto_croston_winner, deterministic) :-
		check_auto_selection([0,6,0,10], croston, 94.0).

	test(auto_sba_winner, deterministic) :-
		check_auto_selection([6,0,6], sba, 58.5).

	test(auto_tsb_winner, deterministic) :-
		check_auto_selection([6,0,0,0], tsb, 83.25).

	test(auto_all_zero_tie, deterministic) :-
		check_auto_selection([0,0,0], sba, 0.0).

	test(auto_single_positive_tie, deterministic) :-
		check_auto_selection([6], sba, 36.0).

	test(auto_croston_tsb_tie, deterministic) :-
		check_auto_selection([6,6], croston, 36.0).

	test(auto_missing_skip, deterministic) :-
		check_auto_missing(skip, 94.0).

	test(auto_missing_elapsed, deterministic) :-
		check_auto_missing(elapsed, 104.0).

	test(auto_default_coefficients, deterministic) :-
		intermittent_demand_forecasting::learn(intermittent_series([6,6]), Forecaster, [model(auto)]),
		intermittent_demand_forecasting::forecaster_options(Forecaster, [model(croston),alpha(0.1),beta(0.1),missing(skip)]).

	test(auto_public_option_hook, deterministic) :-
		intermittent_demand_forecasting::valid_option(model(auto)),
		intermittent_demand_forecasting::default_option(model(sba)).

	test(auto_stored_pending_rejected, fail) :-
		stored_auto_forecaster([0], Forecaster),
		intermittent_demand_forecasting::valid_forecaster(Forecaster).

	test(auto_stored_initialized_rejected, fail) :-
		stored_auto_forecaster([6], Forecaster),
		intermittent_demand_forecasting::valid_forecaster(Forecaster).

	test(auto_stored_update_rejected, error(domain_error(forecaster, _))) :-
		stored_auto_forecaster([6], Forecaster),
		intermittent_demand_forecasting::update(Forecaster, 0, _).

	test(auto_updates_preserve_method, deterministic(Value =~= 4.5)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Original, [model(auto),alpha(0.5),beta(0.5)]),
		copy_term(Original, Snapshot),
		apply_updates([0,0,0], Original, Updated),
		Original == Snapshot,
		intermittent_demand_forecasting::forecaster_options(Updated, [model(sba),alpha(0.5),beta(0.5),missing(skip)]),
		intermittent_demand_forecasting::forecast(Updated, 1, [Value]),
		intermittent_demand_forecasting::learn(intermittent_series([6,0,0,0]), Relearned, [model(auto),alpha(0.5),beta(0.5)]),
		intermittent_demand_forecasting::forecaster_options(Relearned, [model(tsb),alpha(0.5),beta(0.5),missing(skip)]),
		intermittent_demand_forecasting::learn(intermittent_series([6,0,0,0]), Explicit, [model(sba),alpha(0.5),beta(0.5)]),
		Updated = intermittent_demand_forecaster(sba, State, Diagnostics),
		Explicit = intermittent_demand_forecaster(sba, State, ExpectedDiagnostics),
		without_update_count(Diagnostics, Comparable),
		without_update_count(ExpectedDiagnostics, Comparable).

	test(auto_export_croston, deterministic) :-
		check_auto_export([0,6,0,10]).

	test(auto_export_sba, deterministic) :-
		check_auto_export([6,0,6]).

	test(auto_export_tsb, deterministic) :-
		check_auto_export([6,0,0,0]).

	test(auto_export_pending, deterministic) :-
		check_auto_export([0,_,0]).

	test(auto_empty_series, error(domain_error(non_empty_series, intermittent_series([])))) :-
		intermittent_demand_forecasting::learn(intermittent_series([]), _, [model(auto)]).

	test(auto_all_missing, error(domain_error(insufficient_observations, intermittent_series([_,_])))) :-
		intermittent_demand_forecasting::learn(intermittent_series([_,_]), _, [model(auto)]).

	test(auto_negative_demand, error(domain_error(non_negative_number, -1))) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,-1,6]), _, [model(auto)]).

	test(auto_nonnumeric_demand, error(type_error(number, bad))) :-
		intermittent_demand_forecasting::learn(non_numeric_value, _, [model(auto)]).

	test(auto_gap_indices, error(domain_error(series_index_sequence, gap_index))) :-
		intermittent_demand_forecasting::learn(gap_index, _, [model(auto)]).

	test(auto_alpha_tie, deterministic) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Forecaster, [model(auto),alpha(auto)]),
		intermittent_demand_forecasting::forecaster_options(Forecaster, [model(sba),alpha(0.1),beta(0.1),missing(skip)]).

	test(auto_invalid_beta, error(domain_error(option, beta(0)))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [model(auto),beta(0)]).

	test(auto_invalid_missing_policy, error(domain_error(option, missing(zero)))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [model(auto),missing(zero)]).

	test(croston_hand_calculated, deterministic((First =~= 3.2, Second =~= 3.2, Third =~= 3.2))) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,0,6,0,10]), Forecaster, [model(croston), alpha(0.5), beta(0.5)]),
		intermittent_demand_forecasting::forecast(Forecaster, 3, [First, Second, Third]).

	test(sba_hand_calculated, deterministic(Value =~= 2.4)) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,0,6,0,10]), Forecaster, [alpha(0.5), beta(0.5)]),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [Value]).

	test(sba_interval_coefficient, deterministic(Value =~= 2.04)) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,0,6,0,10]), Forecaster, [alpha(0.2), beta(0.5)]),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [Value]).

	test(tsb_hand_calculated, deterministic((First =~= 4.666666666666667, Second =~= First, Third =~= First))) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,0,6,0,10]), Forecaster, [model(tsb), alpha(0.5), beta(0.5)]),
		intermittent_demand_forecasting::forecast(Forecaster, 3, [First, Second, Third]).

	test(tsb_all_zero, deterministic(Forecasts == [0,0])) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,0,0]), Forecaster, [model(tsb)]),
		intermittent_demand_forecasting::forecast(Forecaster, 2, Forecasts).

	test(tsb_pending_update, deterministic(Value =~= 2.0)) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,0]), Original, [model(tsb)]),
		intermittent_demand_forecasting::update(Original, 6, Updated),
		intermittent_demand_forecasting::forecast(Updated, 1, [Value]).

	test(tsb_zero_decay, deterministic((Value =~= 1.5, OriginalValue =~= 6.0))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Original, [model(tsb), beta(0.5)]),
		intermittent_demand_forecasting::update(Original, 0, First),
		intermittent_demand_forecasting::update(First, 0, Updated),
		intermittent_demand_forecasting::forecast(Updated, 1, [Value]),
		intermittent_demand_forecasting::forecast(Original, 1, [OriginalValue]).

	test(tsb_beta_one_zero_probability, deterministic(Value =~= 0.0)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6,0]), Forecaster, [model(tsb), beta(1)]),
		intermittent_demand_forecasting::check_forecaster(Forecaster),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [Value]).

	test(tsb_probability_recovery, deterministic(Value =~= 10.0)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6,0]), Original, [model(tsb), alpha(1), beta(1)]),
		intermittent_demand_forecasting::update(Original, 10, Updated),
		intermittent_demand_forecasting::check_forecaster(Updated),
		intermittent_demand_forecasting::forecast(Updated, 1, [Value]).

	test(tsb_missing_skip, deterministic(Value =~= 5.0)) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,_,6,_,0,10]), Forecaster, [model(tsb), alpha(0.5), beta(0.5)]),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [Value]).

	test(tsb_missing_elapsed, deterministic(Value =~= 4.666666666666667)) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,_,6,_,0,10]), Forecaster, [model(tsb), alpha(0.5), beta(0.5), missing(elapsed)]),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [Value]).

	test(tsb_elapsed_missing_preserves_state, deterministic) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Original, [model(tsb), missing(elapsed)]),
		intermittent_demand_forecasting::update(Original, Missing, Updated),
		Original = intermittent_demand_forecaster(tsb, State, _),
		Updated = intermittent_demand_forecaster(tsb, State, _),
		var(Missing), intermittent_demand_forecasting::check_forecaster(Updated).

	test(single_positive, deterministic(Value =~= 5.7)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Forecaster),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [Value]).

	test(all_zero, deterministic(Forecasts == [0,0,0])) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,0,0]), Forecaster),
		intermittent_demand_forecasting::check_forecaster(Forecaster),
		intermittent_demand_forecasting::forecast(Forecaster, 3, Forecasts).

	test(single_zero, deterministic(Forecasts == [0])) :-
		intermittent_demand_forecasting::learn(intermittent_series([0]), Forecaster),
		intermittent_demand_forecasting::forecast(Forecaster, 1, Forecasts).

	test(consecutive_positive, deterministic(Value =~= 8.0)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6,10]), Forecaster, [model(croston), alpha(0.5)]),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [Value]).

	test(fractional_demands, deterministic(Value =~= 0.5)) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,0.5,0,1.5]), Forecaster, [model(croston), alpha(0.5)]),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [Value]).

	test(coefficient_one, deterministic(Value =~= 2.5)) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,0,6,0,10]), Forecaster, [alpha(1), beta(1)]),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [Value]).

	test(missing_skip, deterministic(Value =~= 4.0)) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,First,6,Last,0,10]), Forecaster, [model(croston), alpha(0.5), beta(0.5)]),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [Value]),
		ground(Forecaster), var(First), var(Last).

	test(missing_elapsed, deterministic(Value =~= 2.6666666666666665)) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,_,6,_,0,10]), Forecaster, [model(croston), alpha(0.5), beta(0.5), missing(elapsed)]),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [Value]).

	test(sba_missing_skip, deterministic(Value =~= 3.0)) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,_,6,_,0,10]), Forecaster, [alpha(0.5), beta(0.5)]),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [Value]).

	test(sba_missing_elapsed, deterministic(Value =~= 2.0)) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,_,6,_,0,10]), Forecaster, [alpha(0.5), beta(0.5), missing(elapsed)]),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [Value]).

	test(zero_horizon, deterministic(Forecasts == [])) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Forecaster),
		intermittent_demand_forecasting::forecast(Forecaster, 0, Forecasts).

	test(pending_update, deterministic(Value =~= 1.5)) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,0]), Original, [beta(0.5)]),
		intermittent_demand_forecasting::update(Original, 6, Updated),
		intermittent_demand_forecasting::forecast(Updated, 1, [Value]),
		intermittent_demand_forecasting::forecast(Original, 1, [0]).

	test(pending_missing_skip, deterministic) :-
		intermittent_demand_forecasting::learn(intermittent_series([0]), Original),
		intermittent_demand_forecasting::update(Original, Missing, Updated),
		Updated = intermittent_demand_forecaster(sba, pending_state(1), _),
		var(Missing), intermittent_demand_forecasting::check_forecaster(Updated).

	test(pending_missing_elapsed, deterministic) :-
		intermittent_demand_forecasting::learn(intermittent_series([0]), Original, [missing(elapsed)]),
		intermittent_demand_forecasting::update(Original, Missing, Updated),
		Updated = intermittent_demand_forecaster(sba, pending_state(2), _),
		var(Missing), intermittent_demand_forecasting::check_forecaster(Updated).

	test(positive_update, deterministic(Value =~= 2.4)) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,0,6,0]), Original, [alpha(0.5), beta(0.5)]),
		intermittent_demand_forecasting::update(Original, 10, Updated),
		intermittent_demand_forecasting::check_forecaster(Updated),
		intermittent_demand_forecasting::forecast(Updated, 1, [Value]).

	test(zero_update_preserves_rate, deterministic) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Original),
		intermittent_demand_forecasting::update(Original, 0, Updated),
		intermittent_demand_forecasting::forecast(Original, 1, Forecasts),
		intermittent_demand_forecasting::forecast(Updated, 1, Forecasts),
		intermittent_demand_forecasting::check_forecaster(Updated).

	test(diagnostics_and_immutable_update, deterministic) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,_,6]), Original, [missing(elapsed)]),
		intermittent_demand_forecasting::update(Original, Missing, Updated),
		var(Missing), ground(Updated),
		intermittent_demand_forecasting::diagnostics(Updated, Diagnostics),
		memberchk(mean_absolute_error(MAE), Diagnostics),
		memberchk(root_mean_squared_error(RMSE), Diagnostics),
		Diagnostics == [model(intermittent_demand_forecasting), training_series_length(4),
			options([model(sba), alpha(0.1), beta(0.1), missing(elapsed)]), method(sba),
			observed_count(2), missing_count(2), positive_count(1), effective_period_count(4), update_count(1),
			scored_count(2), sum_absolute_error(6), sum_squared_error(36), mean_absolute_error(MAE), root_mean_squared_error(RMSE)],
		MAE =~= 3.0, RMSE =~= 4.242640687119285,
		intermittent_demand_forecasting::diagnostics(Original, OriginalDiagnostics),
		memberchk(training_series_length(3), OriginalDiagnostics),
		memberchk(update_count(0), OriginalDiagnostics).

	test(diagnostic_enumeration, deterministic(Diagnostics == Enumerated)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Forecaster),
		intermittent_demand_forecasting::diagnostics(Forecaster, Diagnostics),
		findall(Diagnostic, intermittent_demand_forecasting::diagnostic(Forecaster, Diagnostic), Enumerated).

	test(public_option_hooks, deterministic) :-
		intermittent_demand_forecasting::valid_option(missing(elapsed)),
		intermittent_demand_forecasting::default_option(model(sba)).

	test(update_equivalence_croston_skip, deterministic) :-
		check_update_equivalence(croston, skip).

	test(update_equivalence_croston_elapsed, deterministic) :-
		check_update_equivalence(croston, elapsed).

	test(update_equivalence_sba_skip, deterministic) :-
		check_update_equivalence(sba, skip).

	test(update_equivalence_sba_elapsed, deterministic) :-
		check_update_equivalence(sba, elapsed).

	test(update_equivalence_tsb_skip, deterministic) :-
		check_update_equivalence(tsb, skip).

	test(update_equivalence_tsb_elapsed, deterministic) :-
		check_update_equivalence(tsb, elapsed).

	test(long_zero_series, deterministic) :-
		length(Zeros, 10000), zeros(Zeros),
		intermittent_demand_forecasting::learn(intermittent_series(Zeros), Forecaster, [model(croston)]),
		Forecaster = intermittent_demand_forecaster(croston, pending_state(10000), _),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [0]).

	test(long_tsb_zero_decay, deterministic(Value =~= 0.0)) :-
		length(Zeros, 10000), zeros(Zeros),
		intermittent_demand_forecasting::learn(intermittent_series([6| Zeros]), Forecaster, [model(tsb), beta(0.5)]),
		intermittent_demand_forecasting::check_forecaster(Forecaster),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [Value]).

	test(tiny_coefficients, deterministic(Value =~= 6.0)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6,6]), Forecaster, [model(croston), alpha(0.000000001), beta(0.000000001)]),
		intermittent_demand_forecasting::check_forecaster(Forecaster),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [Value]).

	test(tiny_demands, deterministic(Value > 0)) :-
		intermittent_demand_forecasting::learn(intermittent_series([0.000000001,0,0.000000002]), Forecaster, [model(tsb)]),
		intermittent_demand_forecasting::check_forecaster(Forecaster),
		intermittent_demand_forecasting::forecast(Forecaster, 1, [Value]).

	test(missing_input_isolated, deterministic) :-
		Values = [0,Missing,6], copy_term(Values, Snapshot),
		intermittent_demand_forecasting::learn(intermittent_series(Values), Forecaster),
		intermittent_demand_forecasting::update(Forecaster, Missing, Updated),
		lgtunit::variant(Values, Snapshot), ground(Forecaster), ground(Updated),
		Missing = bad,
		intermittent_demand_forecasting::check_forecaster(Forecaster),
		intermittent_demand_forecasting::check_forecaster(Updated).

	test(no_update_options_api, fail) :-
		intermittent_demand_forecasting::current_predicate(update/4).

	test(croston_error_metrics, deterministic) :-
		check_error_metrics(croston, 16.0, 94.0, 4.0, 4.847679857416329).

	test(sba_error_metrics, deterministic) :-
		ExpectedRMSE is sqrt(101.125 / 4),
		check_error_metrics(sba, 16.0, 101.125, 4.0, ExpectedRMSE).

	test(tsb_error_metrics, deterministic) :-
		ExpectedRMSE is sqrt(117.25 / 4),
		check_error_metrics(tsb, 17.5, 117.25, 4.375, ExpectedRMSE).

	test(error_metrics_cold_start, deterministic((MAE =~= 6.0, RMSE =~= 6.0))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Forecaster),
		once(intermittent_demand_forecasting::diagnostic(Forecaster, scored_count(1))),
		intermittent_demand_forecasting::forecaster_options(Forecaster, [model(sba),alpha(0.1),beta(0.1),missing(skip)]),
		intermittent_demand_forecasting::diagnostics(Forecaster, Diagnostics),
		memberchk(mean_absolute_error(MAE), Diagnostics),
		memberchk(root_mean_squared_error(RMSE), Diagnostics).

	test(error_metrics_all_zero, deterministic((MAE =~= 0.0, RMSE =~= 0.0))) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,0,0]), Forecaster),
		intermittent_demand_forecasting::diagnostics(Forecaster, Diagnostics),
		memberchk(scored_count(3), Diagnostics),
		memberchk(mean_absolute_error(MAE), Diagnostics),
		memberchk(root_mean_squared_error(RMSE), Diagnostics).

	test(error_metrics_missing_skip, deterministic((Absolute =~= 16.0, Squared =~= 94.0))) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,_,6,_,0,10]), Forecaster, [model(croston),alpha(0.5),beta(0.5)]),
		intermittent_demand_forecasting::diagnostics(Forecaster, Diagnostics),
		memberchk(scored_count(4), Diagnostics), memberchk(observed_count(4), Diagnostics),
		memberchk(sum_absolute_error(Absolute), Diagnostics), memberchk(sum_squared_error(Squared), Diagnostics).

	test(error_metrics_missing_elapsed, deterministic((Absolute =~= 16.0, Squared =~= 104.0))) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,_,6,_,0,10]), Forecaster, [model(croston),alpha(0.5),beta(0.5),missing(elapsed)]),
		intermittent_demand_forecasting::diagnostics(Forecaster, Diagnostics),
		memberchk(scored_count(4), Diagnostics), memberchk(observed_count(4), Diagnostics),
		memberchk(sum_absolute_error(Absolute), Diagnostics), memberchk(sum_squared_error(Squared), Diagnostics).

	test(all_missing, error(domain_error(insufficient_observations, intermittent_series([_,_])))) :-
		intermittent_demand_forecasting::learn(intermittent_series([_,_]), _).

	test(empty_series, error(domain_error(non_empty_series, intermittent_series([])))) :-
		intermittent_demand_forecasting::learn(intermittent_series([]), _).

	test(negative_demand, error(domain_error(non_negative_number, -1))) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,-1,2]), _).

	test(non_numeric_demand, error(type_error(number, bad))) :-
		intermittent_demand_forecasting::learn(non_numeric_value, _).

	test(gap_indices, error(domain_error(series_index_sequence, gap_index))) :-
		intermittent_demand_forecasting::learn(gap_index, _).

	test(inconsistent_length, error(consistency_error(series_length, 4, 3))) :-
		intermittent_demand_forecasting::learn(inconsistent_series_length, _).

	test(zero_length, error(domain_error(positive_integer, 0))) :-
		intermittent_demand_forecasting::learn(zero_series_length, _).

	test(non_integer_length, error(type_error(integer, one))) :-
		intermittent_demand_forecasting::learn(non_integer_series_length, _).

	test(invalid_alpha_zero, error(domain_error(option, alpha(0)))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [alpha(0)]).

	test(invalid_alpha_negative, error(domain_error(option, alpha(-0.1)))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [alpha(-0.1)]).

	test(invalid_alpha_large, error(domain_error(option, alpha(2)))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [alpha(2)]).

	test(auto_alpha_boundary, deterministic) :-
		Dataset = intermittent_series([6,10,10]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [model(croston),alpha(auto),beta(0.37)]),
		intermittent_demand_forecasting::learn(Dataset, Expected, [model(croston),alpha(1.0),beta(0.37)]),
		Forecaster == Expected,
		intermittent_demand_forecasting::forecaster_options(Forecaster, [model(croston),alpha(1.0),beta(0.37),missing(skip)]),
		intermittent_demand_forecasting::diagnostics(Forecaster, Diagnostics),
		memberchk(sum_squared_error(Squared), Diagnostics), Squared =~= 52.0.

	test(invalid_alpha_variable, error(domain_error(option, alpha(_)))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [alpha(_)]).

	test(invalid_beta_zero, error(domain_error(option, beta(0)))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [beta(0)]).

	test(invalid_beta_negative, error(domain_error(option, beta(-0.1)))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [beta(-0.1)]).

	test(auto_beta_boundary, deterministic) :-
		Dataset = intermittent_series([6,0,0]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [model(tsb),alpha(0.37),beta(auto)]),
		intermittent_demand_forecasting::learn(Dataset, Expected, [model(tsb),alpha(0.37),beta(1.0)]),
		Forecaster == Expected,
		intermittent_demand_forecasting::forecaster_options(Forecaster, [model(tsb),alpha(0.37),beta(1.0),missing(skip)]),
		intermittent_demand_forecasting::diagnostics(Forecaster, Diagnostics),
		memberchk(sum_squared_error(Squared), Diagnostics), Squared =~= 72.0.

	test(invalid_beta_large, error(domain_error(option, beta(1.1)))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [beta(1.1)]).

	test(invalid_missing_policy, error(domain_error(option, missing(zero)))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [missing(zero)]).

	test(invalid_model, error(domain_error(option, model(other)))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [model(other)]).

	test(variable_options, error(instantiation_error)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, _).

	test(partial_options, error(instantiation_error)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [model(tsb)| _]).

	test(variable_option, error(instantiation_error)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [_]).

	test(non_list_options, error(type_error(list, bad))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, bad).

	test(non_compound_option, error(type_error(compound, bad))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [bad]).

	test(irrelevant_frequency_option, error(domain_error(option, frequency(4)))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), _, [frequency(4)]).

	test(invalid_forecaster, error(domain_error(forecaster, bad))) :-
		intermittent_demand_forecasting::forecast(bad, 0, _).

	test(variable_forecaster, error(instantiation_error)) :-
		intermittent_demand_forecasting::update(_, 6, _).

	test(partial_forecaster_not_bound, deterministic) :-
		Forecaster = intermittent_demand_forecaster(sba, State, []),
		copy_term(Forecaster, Snapshot),
		\+ intermittent_demand_forecasting::valid_forecaster(Forecaster),
		var(State), lgtunit::variant(Forecaster, Snapshot).

	test(invalid_term, fail) :-
		intermittent_demand_forecasting::valid_forecaster(bad).

	test(invalid_state_method_tsb, fail) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), intermittent_demand_forecaster(tsb, _, Diagnostics), [model(tsb)]),
		intermittent_demand_forecasting::valid_forecaster(intermittent_demand_forecaster(tsb, croston_state(6,1,0), Diagnostics)).

	test(invalid_state_method_sba, fail) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), intermittent_demand_forecaster(sba, _, Diagnostics)),
		intermittent_demand_forecasting::valid_forecaster(intermittent_demand_forecaster(sba, tsb_state(6,1), Diagnostics)).

	test(invalid_state_shape, fail) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), intermittent_demand_forecaster(sba, _, Diagnostics)),
		intermittent_demand_forecasting::valid_forecaster(intermittent_demand_forecaster(sba, other(6), Diagnostics)).

	test(invalid_positive_pending_state, fail) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), intermittent_demand_forecaster(sba, _, Diagnostics)),
		intermittent_demand_forecasting::valid_forecaster(intermittent_demand_forecaster(sba, pending_state(1), Diagnostics)).

	test(invalid_pending_clock, fail) :-
		intermittent_demand_forecasting::learn(intermittent_series([0]), intermittent_demand_forecaster(sba, _, Diagnostics)),
		intermittent_demand_forecasting::valid_forecaster(intermittent_demand_forecaster(sba, pending_state(2), Diagnostics)).

	test(invalid_size, fail) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), intermittent_demand_forecaster(sba, _, Diagnostics)),
		intermittent_demand_forecasting::valid_forecaster(intermittent_demand_forecaster(sba, croston_state(0,1,0), Diagnostics)).

	test(invalid_interval_zero, fail) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), intermittent_demand_forecaster(sba, _, Diagnostics)),
		intermittent_demand_forecasting::valid_forecaster(intermittent_demand_forecaster(sba, croston_state(6,0,0), Diagnostics)).

	test(invalid_interval_exceeds_time, fail) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), intermittent_demand_forecaster(sba, _, Diagnostics)),
		intermittent_demand_forecasting::valid_forecaster(intermittent_demand_forecaster(sba, croston_state(6,2,0), Diagnostics)).

	test(invalid_age, fail) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), intermittent_demand_forecaster(sba, _, Diagnostics)),
		intermittent_demand_forecasting::valid_forecaster(intermittent_demand_forecaster(sba, croston_state(6,1,1), Diagnostics)).

	test(invalid_tsb_probability, fail) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), intermittent_demand_forecaster(tsb, _, Diagnostics), [model(tsb)]),
		intermittent_demand_forecasting::valid_forecaster(intermittent_demand_forecaster(tsb, tsb_state(6,2), Diagnostics)).

	test(invalid_tsb_negative_probability, fail) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), intermittent_demand_forecaster(tsb, _, Diagnostics), [model(tsb)]),
		intermittent_demand_forecasting::valid_forecaster(intermittent_demand_forecaster(tsb, tsb_state(6,-1), Diagnostics)).

	test(invalid_observed_count, fail) :-
		invalid_counts(1, 0, 1, 0, 0, 0).

	test(invalid_missing_count, fail) :-
		invalid_counts(1, 1, -1, 1, 1, 0).

	test(invalid_length_count_consistency, fail) :-
		invalid_counts(2, 1, 0, 1, 1, 0).

	test(invalid_positive_count, fail) :-
		invalid_counts(1, 1, 0, 2, 1, 0).

	test(invalid_effective_count, fail) :-
		invalid_counts(1, 1, 0, 1, 2, 0).

	test(invalid_update_count, fail) :-
		invalid_counts(1, 1, 0, 1, 1, 1).

	test(invalid_options_state_consistency, fail) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), intermittent_demand_forecaster(sba, State, Diagnostics)),
		intermittent_demand_forecasting::valid_forecaster(intermittent_demand_forecaster(croston, State, Diagnostics)).

	test(invalid_update_negative, error(domain_error(non_negative_number, -1))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Forecaster),
		intermittent_demand_forecasting::update(Forecaster, -1, _).

	test(error_metrics_absent_croston, fail) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,6,0,10]), Complete, [model(croston)]),
		forecaster_without_errors(Complete, Incomplete),
		intermittent_demand_forecasting::valid_forecaster(Incomplete).

	test(error_metrics_absent_sba, fail) :-
		forecaster_with_errors([], Forecaster),
		intermittent_demand_forecasting::valid_forecaster(Forecaster).

	test(error_metrics_absent_tsb, fail) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,6,0,10]), Complete, [model(tsb)]),
		forecaster_without_errors(Complete, Incomplete),
		intermittent_demand_forecasting::valid_forecaster(Incomplete).

	test(error_metrics_absent_pending, fail) :-
		intermittent_demand_forecasting::learn(intermittent_series([0]), Complete),
		forecaster_without_errors(Complete, Incomplete),
		intermittent_demand_forecasting::valid_forecaster(Incomplete).

	test(error_metrics_missing_count, fail) :-
		forecaster_with_errors([sum_absolute_error(6),sum_squared_error(36),mean_absolute_error(6.0),root_mean_squared_error(6.0)], Forecaster),
		intermittent_demand_forecasting::valid_forecaster(Forecaster).

	test(error_metrics_missing_absolute_sum, fail) :-
		forecaster_with_errors([scored_count(1),sum_squared_error(36),mean_absolute_error(6.0),root_mean_squared_error(6.0)], Forecaster),
		intermittent_demand_forecasting::valid_forecaster(Forecaster).

	test(error_metrics_missing_squared_sum, fail) :-
		forecaster_with_errors([scored_count(1),sum_absolute_error(6),mean_absolute_error(6.0),root_mean_squared_error(6.0)], Forecaster),
		intermittent_demand_forecasting::valid_forecaster(Forecaster).

	test(error_metrics_missing_mae, fail) :-
		forecaster_with_errors([scored_count(1),sum_absolute_error(6),sum_squared_error(36),root_mean_squared_error(6.0)], Forecaster),
		intermittent_demand_forecasting::valid_forecaster(Forecaster).

	test(error_metrics_missing_rmse, fail) :-
		forecaster_with_errors([scored_count(1),sum_absolute_error(6),sum_squared_error(36),mean_absolute_error(6.0)], Forecaster),
		intermittent_demand_forecasting::valid_forecaster(Forecaster).

	test(error_metrics_check_rejects_absent, error(domain_error(forecaster, _))) :-
		forecaster_with_errors([], Forecaster),
		intermittent_demand_forecasting::check_forecaster(Forecaster).

	test(error_metrics_update_rejects_absent, error(domain_error(forecaster, _))) :-
		forecaster_with_errors([], Forecaster),
		intermittent_demand_forecasting::update(Forecaster, 0, _).

	test(error_metrics_missing_update_rejects_absent, error(domain_error(forecaster, _))) :-
		forecaster_with_errors([], Forecaster),
		intermittent_demand_forecasting::update(Forecaster, _, _).

	test(error_metrics_missing_update_unchanged, deterministic) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,6,0,10]), Original, [model(tsb),missing(elapsed)]),
		forecaster_error_terms(Original, Errors),
		intermittent_demand_forecasting::update(Original, Missing, Updated),
		forecaster_error_terms(Updated, Errors),
		var(Missing), ground(Updated),
		intermittent_demand_forecasting::check_forecaster(Updated).

	test(error_metrics_zero_update_scored, deterministic) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Original, [model(croston)]),
		intermittent_demand_forecasting::update(Original, 0, Updated),
		forecaster_error_terms(Updated, [scored_count(2),sum_absolute_error(Absolute),sum_squared_error(Squared),mean_absolute_error(MAE),root_mean_squared_error(RMSE)]),
		Absolute =~= 12.0, Squared =~= 72.0, MAE =~= 6.0, RMSE =~= 6.0,
		forecaster_error_terms(Original, [scored_count(1),sum_absolute_error(6),sum_squared_error(36),mean_absolute_error(OriginalMAE),root_mean_squared_error(OriginalRMSE)]),
		OriginalMAE =~= 6.0, OriginalRMSE =~= 6.0.

	test(error_metrics_partial_group, fail) :-
		forecaster_with_errors([scored_count(1)], Forecaster),
		intermittent_demand_forecasting::valid_forecaster(Forecaster).

	test(error_metrics_duplicate_group, fail) :-
		forecaster_with_errors([scored_count(1),sum_absolute_error(6),sum_squared_error(36),mean_absolute_error(6.0),root_mean_squared_error(6.0),scored_count(1)], Forecaster),
		intermittent_demand_forecasting::valid_forecaster(Forecaster).

	test(error_metrics_duplicate_replaces_missing, fail) :-
		forecaster_with_errors([scored_count(1),sum_absolute_error(6),sum_squared_error(36),root_mean_squared_error(6.0),root_mean_squared_error(6.0)], Forecaster),
		intermittent_demand_forecasting::valid_forecaster(Forecaster).

	test(error_metrics_wrong_arity, fail) :-
		forecaster_with_errors([scored_count(1,bad)], Forecaster),
		intermittent_demand_forecasting::valid_forecaster(Forecaster).

	test(error_metrics_atom, fail) :-
		forecaster_with_errors([scored_count], Forecaster),
		intermittent_demand_forecasting::valid_forecaster(Forecaster).

	test(error_metrics_inconsistent_count, fail) :-
		invalid_error_value(scored_count, 2).

	test(error_metrics_noninteger_count, fail) :-
		invalid_error_value(scored_count, 1.0).

	test(error_metrics_nonnumeric_count, fail) :-
		invalid_error_value(scored_count, bad).

	test(error_metrics_negative_absolute_sum, fail) :-
		invalid_error_value(sum_absolute_error, -6).

	test(error_metrics_negative_squared_sum, fail) :-
		invalid_error_value(sum_squared_error, -36).

	test(error_metrics_nonnumeric_absolute_sum, fail) :-
		invalid_error_value(sum_absolute_error, bad).

	test(error_metrics_nonnumeric_squared_sum, fail) :-
		invalid_error_value(sum_squared_error, bad).

	test(error_metrics_nonnumeric_mae, fail) :-
		invalid_error_value(mean_absolute_error, bad).

	test(error_metrics_nonnumeric_rmse, fail) :-
		invalid_error_value(root_mean_squared_error, bad).

	test(error_metrics_negative_mae, fail) :-
		invalid_error_value(mean_absolute_error, -6).

	test(error_metrics_negative_rmse, fail) :-
		invalid_error_value(root_mean_squared_error, -6).

	test(error_metrics_inconsistent_mae, fail) :-
		invalid_error_value(mean_absolute_error, 0).

	test(error_metrics_inconsistent_rmse, fail) :-
		invalid_error_value(root_mean_squared_error, 0).

	test(error_metrics_partial_not_bound, deterministic) :-
		forecaster_with_errors([scored_count(1),sum_absolute_error(Absolute),sum_squared_error(36),mean_absolute_error(6.0),root_mean_squared_error(6.0)], Forecaster),
		copy_term(Forecaster, Snapshot),
		\+ intermittent_demand_forecasting::valid_forecaster(Forecaster),
		var(Absolute), lgtunit::variant(Forecaster, Snapshot).

	test(error_metrics_check_rejects_partial, error(domain_error(forecaster, _))) :-
		forecaster_with_errors([scored_count(1)], Forecaster),
		intermittent_demand_forecasting::check_forecaster(Forecaster).

	test(error_metrics_update_rejects_partial, error(domain_error(forecaster, _))) :-
		forecaster_with_errors([scored_count(1)], Forecaster),
		intermittent_demand_forecasting::update(Forecaster, 0, _).

	test(invalid_update_nonnumeric, error(type_error(number, bad))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Forecaster),
		intermittent_demand_forecasting::update(Forecaster, bad, _).

	test(negative_horizon, error(domain_error(non_negative_integer, -1))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Forecaster),
		intermittent_demand_forecasting::forecast(Forecaster, -1, _).

	test(variable_horizon, error(instantiation_error)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Forecaster),
		intermittent_demand_forecasting::forecast(Forecaster, _, _).

	test(non_integer_horizon, error(type_error(integer, one))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Forecaster),
		intermittent_demand_forecasting::forecast(Forecaster, one, _).

	test(export_invalid_functor, error(type_error(atom, 1))) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Forecaster),
		intermittent_demand_forecasting::export_to_clauses(intermittent_series([6]), Forecaster, 1, _).

	test(export_variable_functor, error(instantiation_error)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Forecaster),
		intermittent_demand_forecasting::export_to_clauses(intermittent_series([6]), Forecaster, _, _).

	test(export_invalid_forecaster, error(domain_error(forecaster, bad))) :-
		intermittent_demand_forecasting::export_to_clauses(intermittent_series([6]), bad, saved_demand, _).

	test(print_invalid_forecaster, error(domain_error(forecaster, bad))) :-
		intermittent_demand_forecasting::print_forecaster(bad).

	test(print_forecaster, deterministic) :-
		^^suppress_text_output,
		intermittent_demand_forecasting::learn(intermittent_series([6]), Forecaster),
		intermittent_demand_forecasting::print_forecaster(Forecaster).

	test(export_clauses, deterministic(Clauses == [saved_demand(Forecaster)])) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Forecaster),
		intermittent_demand_forecasting::export_to_clauses(intermittent_series([6]), Forecaster, saved_demand, Clauses).

	test(export_file_roundtrip, deterministic) :-
		Dataset = intermittent_series([0,_,6,0,10]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster),
		check_export_roundtrip(Dataset, Forecaster).

	test(export_updated_tsb_roundtrip, deterministic) :-
		Dataset = intermittent_series([0,6,0]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [model(tsb)]),
		intermittent_demand_forecasting::update(Forecaster, 10, Updated),
		check_export_roundtrip(Dataset, Updated).

	test(export_pending_roundtrip, deterministic) :-
		Dataset = intermittent_series([0,_,0]),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [missing(elapsed)]),
		check_export_roundtrip(Dataset, Forecaster).

	% auxiliary predicates

	check_coefficient_grid(Policy) :-
		check_coefficient_grid(Policy, [0.1,0.2,0.5,0.8,1.0], []).

	check_coefficient_grid(Policy, Grid, GridOptions) :-
		Dataset = intermittent_series([0,FirstMissing,6,LastMissing,0,10]),
		copy_term(Dataset, Snapshot),
		findall(RMSE-Candidate, (
			member(Method, [sba,croston,tsb]), member(Alpha, Grid), member(Beta, Grid),
			intermittent_demand_forecasting::learn(Dataset, Candidate, [model(Method),alpha(Alpha),beta(Beta),missing(Policy)]),
			intermittent_demand_forecasting::diagnostics(Candidate, CandidateDiagnostics),
			memberchk(root_mean_squared_error(RMSE), CandidateDiagnostics)
		), Candidates),
		length(Grid, GridLength), CandidateCount is 3 * GridLength * GridLength,
		length(Candidates, CandidateCount), keysort(Candidates, [_-Expected| _]),
		Requests = [model(auto),alpha(auto),beta(auto),missing(Policy)| GridOptions],
		intermittent_demand_forecasting::learn(Dataset, Forecaster, Requests),
		Forecaster == Expected, ground(Forecaster),
		var(FirstMissing), var(LastMissing), lgtunit::variant(Dataset, Snapshot),
		intermittent_demand_forecasting::forecaster_options(Forecaster, Options),
		intermittent_demand_forecasting::fitted_values(Dataset, Values, Requests),
		intermittent_demand_forecasting::fitted_values(Dataset, ExpectedValues, Options),
		lgtunit::variant(Values, ExpectedValues),
		intermittent_demand_forecasting::check_forecaster(Forecaster).

	check_invalid_grid(Grid) :-
		copy_term(Grid, Snapshot),
		\+ intermittent_demand_forecasting::valid_option(coefficient_grid(Grid)),
		catch(intermittent_demand_forecasting::learn(intermittent_series([6]), _, [coefficient_grid(Grid)]), LearnError, true),
		nonvar(LearnError), LearnError = error(domain_error(option, coefficient_grid(Grid)), _),
		lgtunit::variant(Grid, Snapshot),
		catch(intermittent_demand_forecasting::fitted_values(intermittent_series([6]), _, [coefficient_grid(Grid)]), FittedError, true),
		nonvar(FittedError), FittedError = error(domain_error(option, coefficient_grid(Grid)), _),
		lgtunit::variant(Grid, Snapshot).

	stored_coefficients(Values, Alpha, Beta, intermittent_demand_forecaster(sba,State,Changed)) :-
		intermittent_demand_forecasting::learn(intermittent_series(Values), intermittent_demand_forecaster(sba,State,Diagnostics)),
		Diagnostics = [Model,Length,options([model(sba),alpha(_),beta(_),missing(Policy)])| Rest],
		Changed = [Model,Length,options([model(sba),alpha(Alpha),beta(Beta),missing(Policy)])| Rest].

	check_fitted_missing(Method, Policy, ExpectedThird, ExpectedFourth) :-
		Dataset = intermittent_series([0,FirstMissing,6,LastMissing,0,10]),
		copy_term(Dataset, Snapshot),
		intermittent_demand_forecasting::fitted_values(Dataset, Values, [model(Method),alpha(0.5),beta(0.5),missing(Policy)]),
		length(Values, 6),
		Values = [First,FirstPlaceholder,Second,LastPlaceholder,Third,Fourth],
		First =~= 0.0, Second =~= 0.0, Third =~= ExpectedThird, Fourth =~= ExpectedFourth,
		var(FirstMissing), var(LastMissing), var(FirstPlaceholder), var(LastPlaceholder),
		FirstPlaceholder \== FirstMissing, FirstPlaceholder \== LastMissing,
		LastPlaceholder \== FirstMissing, LastPlaceholder \== LastMissing,
		FirstPlaceholder \== LastPlaceholder,
		lgtunit::variant(Dataset, Snapshot),
		check_fitted_errors([0,FirstMissing,6,LastMissing,0,10], Method, Policy).

	check_fitted_auto(Series, Method) :-
		Dataset = intermittent_series(Series),
		intermittent_demand_forecasting::fitted_values(Dataset, Values, [model(auto),alpha(0.5),beta(0.5)]),
		intermittent_demand_forecasting::fitted_values(Dataset, Expected, [model(Method),alpha(0.5),beta(0.5)]),
		Values == Expected.

	check_fitted_errors(Series, Method, Policy) :-
		Dataset = intermittent_series(Series),
		Options = [model(Method),alpha(0.5),beta(0.5),missing(Policy)],
		intermittent_demand_forecasting::learn(Dataset, Forecaster, Options),
		intermittent_demand_forecasting::fitted_values(Dataset, Values, Options),
		fitted_error_totals(Series, Values, forecast_error_totals(0,0,0), forecast_error_totals(Count,Absolute,Squared)),
		MAE is float(Absolute / Count), RMSE is float(sqrt(Squared / Count)),
		intermittent_demand_forecasting::diagnostics(Forecaster, Diagnostics),
		memberchk(scored_count(Count), Diagnostics),
		memberchk(sum_absolute_error(StoredAbsolute), Diagnostics),
		memberchk(sum_squared_error(StoredSquared), Diagnostics),
		memberchk(mean_absolute_error(StoredMAE), Diagnostics),
		memberchk(root_mean_squared_error(StoredRMSE), Diagnostics),
		Absolute =~= StoredAbsolute, Squared =~= StoredSquared,
		MAE =~= StoredMAE, RMSE =~= StoredRMSE.

	fitted_error_totals([], [], Totals, Totals).
	fitted_error_totals([Actual| Actuals], [Prediction| Predictions], Totals0, Totals) :-
		( var(Actual) ->
			var(Prediction), Totals1 = Totals0
		; Totals0 = forecast_error_totals(Count0,Absolute0,Squared0),
			Error is Actual - Prediction,
			Count is Count0 + 1, Absolute is Absolute0 + abs(Error), Squared is Squared0 + Error * Error,
			Totals1 = forecast_error_totals(Count,Absolute,Squared)
		),
		fitted_error_totals(Actuals, Predictions, Totals1, Totals).

	check_auto_selection(Values, Method, Squared) :-
		Dataset = intermittent_series(Values),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [model(auto),alpha(0.5),beta(0.5)]),
		intermittent_demand_forecasting::learn(Dataset, Expected, [model(Method),alpha(0.5),beta(0.5)]),
		Forecaster == Expected,
		intermittent_demand_forecasting::check_forecaster(Forecaster),
		intermittent_demand_forecasting::forecaster_options(Forecaster, [model(Method),alpha(0.5),beta(0.5),missing(skip)]),
		intermittent_demand_forecasting::diagnostics(Forecaster, Diagnostics),
		memberchk(sum_squared_error(ActualSquared), Diagnostics),
		ActualSquared =~= Squared.

	check_auto_missing(Policy, Squared) :-
		Dataset = intermittent_series([0,First,6,Last,0,10]),
		copy_term(Dataset, Snapshot),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [model(auto),alpha(0.5),beta(0.5),missing(Policy)]),
		intermittent_demand_forecasting::learn(Dataset, Expected, [model(croston),alpha(0.5),beta(0.5),missing(Policy)]),
		Forecaster == Expected,
		var(First), var(Last), ground(Forecaster),
		lgtunit::variant(Dataset, Snapshot),
		intermittent_demand_forecasting::diagnostics(Forecaster, Diagnostics),
		memberchk(scored_count(4), Diagnostics),
		memberchk(sum_squared_error(ActualSquared), Diagnostics),
		ActualSquared =~= Squared,
		intermittent_demand_forecasting::update(Forecaster, Missing, Updated),
		var(Missing), intermittent_demand_forecasting::check_forecaster(Updated).

	stored_auto_forecaster(Values, intermittent_demand_forecaster(auto,State,Changed)) :-
		intermittent_demand_forecasting::learn(intermittent_series(Values), intermittent_demand_forecaster(sba,State,Diagnostics)),
		Diagnostics = [Model,Length,options([model(sba)| Options]),method(sba)| Counts],
		Changed = [Model,Length,options([model(auto)| Options]),method(auto)| Counts].

	check_auto_export(Values) :-
		Dataset = intermittent_series(Values),
		intermittent_demand_forecasting::learn(Dataset, Forecaster, [model(auto),alpha(0.5),beta(0.5)]),
		check_export_roundtrip(Dataset, Forecaster).

	forecaster_error_terms(Forecaster, Errors) :-
		intermittent_demand_forecasting::diagnostics(Forecaster, Diagnostics),
		Errors = [scored_count(_),sum_absolute_error(_),sum_squared_error(_),mean_absolute_error(_),root_mean_squared_error(_)],
		once(append(_, Errors, Diagnostics)).

	forecaster_without_errors(Forecaster, intermittent_demand_forecaster(Method,State,OtherDiagnostics)) :-
		Forecaster = intermittent_demand_forecaster(Method,State,Diagnostics),
		forecaster_error_terms(Forecaster, Errors),
		once(append(OtherDiagnostics, Errors, Diagnostics)).

	forecaster_with_errors(Errors, intermittent_demand_forecaster(Method,State,Diagnostics)) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), Tracked),
		forecaster_without_errors(Tracked, intermittent_demand_forecaster(Method,State,OtherDiagnostics)),
		append(OtherDiagnostics, Errors, Diagnostics).

	invalid_error_value(Name, Value) :-
		intermittent_demand_forecasting::learn(intermittent_series([6]), intermittent_demand_forecaster(Method,State,Diagnostics)),
		append(Prefix, [Diagnostic| Suffix], Diagnostics),
		Diagnostic =.. [Name,_],
		Replacement =.. [Name,Value],
		append(Prefix, [Replacement| Suffix], Changed),
		intermittent_demand_forecasting::valid_forecaster(intermittent_demand_forecaster(Method,State,Changed)).

	check_error_metrics(Method, Absolute, Squared, MAE, RMSE) :-
		intermittent_demand_forecasting::learn(intermittent_series([0,6,0,10]), Forecaster, [model(Method),alpha(0.5),beta(0.5)]),
		intermittent_demand_forecasting::check_forecaster(Forecaster),
		intermittent_demand_forecasting::diagnostics(Forecaster, Diagnostics),
		memberchk(scored_count(4), Diagnostics),
		memberchk(sum_absolute_error(ActualAbsolute), Diagnostics),
		memberchk(sum_squared_error(ActualSquared), Diagnostics),
		memberchk(mean_absolute_error(ActualMAE), Diagnostics),
		memberchk(root_mean_squared_error(ActualRMSE), Diagnostics),
		ActualAbsolute =~= Absolute, ActualSquared =~= Squared,
		ActualMAE =~= MAE, ActualRMSE =~= RMSE.

	check_export_roundtrip(Dataset, Forecaster) :-
		^^file_path('test_output.pl', File),
		intermittent_demand_forecasting::export_to_file(Dataset, Forecaster, saved_demand, File),
		open(File, read, Stream),
		catch(read(Stream, Clause), Error, (close(Stream), throw(Error))),
		close(Stream),
		Clause = saved_demand(Loaded),
		Loaded == Forecaster,
		intermittent_demand_forecasting::check_forecaster(Loaded),
		intermittent_demand_forecasting::forecast(Loaded, 3, Forecasts),
		intermittent_demand_forecasting::forecast(Forecaster, 3, Forecasts),
		intermittent_demand_forecasting::update(Loaded, 8, Updated),
		intermittent_demand_forecasting::update(Forecaster, 8, Updated).

	check_update_equivalence(Method, Policy) :-
		Prefix = [0,Missing,0],
		Suffix = [6,Missing,0,10,0],
		Options = [model(Method), alpha(0.5), beta(0.5), missing(Policy)],
		intermittent_demand_forecasting::learn(intermittent_series(Prefix), Original, Options),
		apply_updates(Suffix, Original, Updated),
		append(Prefix, Suffix, Values),
		intermittent_demand_forecasting::learn(intermittent_series(Values), Relearned, Options),
		Updated = intermittent_demand_forecaster(Method, State, Diagnostics),
		Relearned = intermittent_demand_forecaster(Method, State, NewDiagnostics),
		without_update_count(Diagnostics, Comparable),
		without_update_count(NewDiagnostics, Comparable),
		intermittent_demand_forecasting::forecast(Updated, 3, Forecasts),
		intermittent_demand_forecasting::forecast(Relearned, 3, Forecasts),
		memberchk(update_count(5), Diagnostics),
		intermittent_demand_forecasting::check_forecaster(Updated),
		intermittent_demand_forecasting::check_forecaster(Original),
		var(Missing).

	apply_updates([], Forecaster, Forecaster).
	apply_updates([Observation| Observations], Forecaster, Updated) :-
		intermittent_demand_forecasting::update(Forecaster, Observation, Next),
		apply_updates(Observations, Next, Updated).

	without_update_count([], []).
	without_update_count([update_count(_)| Diagnostics], Rest) :-
		!,
		without_update_count(Diagnostics, Rest).
	without_update_count([Diagnostic| Diagnostics], [Diagnostic| Rest]) :-
		without_update_count(Diagnostics, Rest).

	zeros([]).
	zeros([0| Zeros]) :-
		zeros(Zeros).

	invalid_counts(Length, Count, Missing, Positive, Effective, Updates) :-
		intermittent_demand_forecasting::valid_forecaster(intermittent_demand_forecaster(sba, croston_state(6,1,0), [
			model(intermittent_demand_forecasting), training_series_length(Length),
			options([model(sba), alpha(0.1), beta(0.1), missing(skip)]), method(sba),
			observed_count(Count), missing_count(Missing), positive_count(Positive),
			effective_period_count(Effective), update_count(Updates)
		])).

:- end_object.
