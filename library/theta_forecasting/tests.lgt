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
		comment is 'Tests for the "theta_forecasting" library.'
	]).

	:- uses(list, [
		memberchk/2, select/3, nth1/3
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, variant/2
	]).

	cover(theta_forecasting).

	cleanup :-
		^^clean_file('test_output.pl').

	test(theta_forecasting_retained_residuals_complete_oracle, deterministic) :-
		theta_forecasting::learn(theta_series([10,12,14,16], none), Model,
			[alpha(0.5),initialization(first),retain_residuals(true)]),
		theta_forecasting::diagnostics(Model, Diagnostics),
		memberchk(residuals([Second,Third,Fourth]), Diagnostics),
		memberchk(residual_indices([2,3,4]), Diagnostics),
		Second =~= 2.0, Third =~= 3.0, Fourth =~= 3.5,
		ground(Model), theta_forecasting::check_forecaster(Model).

	test(theta_forecasting_retained_residuals_gap_oracle, deterministic) :-
		Series = [10,Missing,14,16], copy_term(Series, Before),
		theta_forecasting::learn(theta_series(Series, none), Model,
			[alpha(0.5),initialization(first),retain_residuals(true)]),
		theta_forecasting::diagnostics(Model, Diagnostics),
		memberchk(residuals([Third,Fourth]), Diagnostics), memberchk(residual_indices([3,4]), Diagnostics),
		Third =~= 4.0, Fourth =~= 4.0,
		variant(Series, Before), var(Missing), ground(Model), theta_forecasting::check_forecaster(Model).

	test(theta_forecasting_retained_residuals_default_off, deterministic) :-
		learn_line(0.5, Model), theta_forecasting::diagnostics(Model, Diagnostics),
		memberchk(residuals(none), Diagnostics), memberchk(residual_indices(none), Diagnostics),
		theta_forecasting::default_option(retain_residuals(false)).

	test(theta_forecasting_retained_residuals_explicit_off, deterministic(Default == Explicit)) :-
		Dataset = theta_series([10,12,14,16], none), Options = [alpha(0.5),initialization(first)],
		theta_forecasting::learn(Dataset, Default, Options),
		theta_forecasting::learn(Dataset, Explicit, [retain_residuals(false)| Options]).

	test(theta_forecasting_retained_residuals_leading, deterministic) :-
		theta_forecasting::learn(theta_series([_,10,12], none), Model,
			[alpha(0.5),initialization(first),retain_residuals(true)]),
		theta_forecasting::diagnostics(Model, Diagnostics),
		memberchk(residual_indices([3]), Diagnostics), memberchk(residuals([Error]), Diagnostics), Error =~= 2.0.

	test(theta_forecasting_retained_residuals_trailing, deterministic) :-
		theta_forecasting::learn(theta_series([10,12,_], none), Model,
			[alpha(0.5),initialization(first),retain_residuals(true)]),
		theta_forecasting::diagnostics(Model, Diagnostics),
		memberchk(residual_indices([2]), Diagnostics), memberchk(residuals([Error]), Diagnostics), Error =~= 2.0.

	test(theta_forecasting_retained_residuals_alpha_zero, deterministic) :-
		theta_forecasting::learn(theta_series([10,12,14,16], none), Model,
			[alpha(0),initialization(first),retain_residuals(true)]),
		theta_forecasting::diagnostics(Model, Diagnostics), memberchk(residuals([Second,Third,Fourth]), Diagnostics),
		Second =~= 2.0, Third =~= 4.0, Fourth =~= 6.0.

	test(theta_forecasting_retained_residuals_alpha_one, deterministic) :-
		theta_forecasting::learn(theta_series([10,12,14,16], none), Model,
			[alpha(1),initialization(first),retain_residuals(true)]),
		theta_forecasting::diagnostics(Model, Diagnostics), memberchk(residuals([Second,Third,Fourth]), Diagnostics),
		Second =~= 2.0, Third =~= 2.0, Fourth =~= 2.0.

	test(theta_forecasting_retained_residuals_odd_phase, deterministic) :-
		Series = [8,10,12,8,10,12,8], Dataset = theta_series(Series, 3),
		Options = [alpha(0.5),initialization(first),seasonal(additive),retain_residuals(true)],
		theta_forecasting::learn(Dataset, Model, Options),
		theta_forecasting::diagnostics(Model, Diagnostics), memberchk(residual_indices([2,3,4,5,6,7]), Diagnostics),
		theta_forecasting::fitted_values(Dataset, Fits, Options), check_retained_diagnostics(Series, Fits, Diagnostics).

	test(theta_forecasting_retained_residuals_original_multiplicative_scale, deterministic) :-
		Series = [5,15,6,18,7,21,_,24,8,24,9,27], Dataset = theta_series(Series, 2),
		Options = [alpha(0),initialization(first),seasonal(multiplicative),retain_residuals(true)],
		theta_forecasting::learn(Dataset, Model, Options),
		Model = theta_forecaster(theta_state(_,_,_,_,seasonal(_,_,_,[Low,High])), Diagnostics),
		memberchk(residual_indices([2,3,4,5,6,8,9,10,11,12]), Diagnostics),
		memberchk(residuals([Second,Third,Fourth| _]), Diagnostics),
		Prediction is 5 * High / Low, ExpectedSecond is 15 - Prediction, ExpectedFourth is 18 - Prediction,
		Second =~= ExpectedSecond, Third =~= 1.0, Fourth =~= ExpectedFourth,
		memberchk(sum_squared_error(SSE), Diagnostics), memberchk(optimization_sum_squared_error(Objective), Diagnostics),
		abs(SSE - Objective) > 1,
		theta_forecasting::fitted_values(Dataset, Fits, Options), check_retained_diagnostics(Series, Fits, Diagnostics).

	test(theta_forecasting_retained_residuals_auto_seasonal, deterministic) :-
		Series = [5,15,5,15,_,15,5,15,5,15,5,15,5,15,5,15,5,15,5,15,5,15,5,15], Dataset = theta_series(Series, 2),
		theta_forecasting::learn(Dataset, Model, [retain_residuals(true)]),
		theta_forecasting::diagnostics(Model, Diagnostics), memberchk(seasonal_mode(multiplicative), Diagnostics),
		theta_forecasting::fitted_values(Dataset, Fits, [retain_residuals(true)]),
		check_retained_diagnostics(Series, Fits, Diagnostics).

	test(theta_forecasting_retained_residuals_strict_policy, deterministic) :-
		theta_forecasting::learn(theta_series([10,12,14,16], none), Model,
			[alpha(0.5),initialization(first),retain_residuals(true),missing_policy(error)]),
		theta_forecasting::check_forecaster(Model), theta_forecasting::diagnostics(Model, Diagnostics),
		memberchk(residual_indices([2,3,4]), Diagnostics).

	test(theta_forecasting_retained_residuals_export_roundtrip, deterministic) :-
		Series = [_,12,8,12,8,12,_,12,8,12,8,12,8,_], Dataset = theta_series(Series, 2),
		theta_forecasting::learn(Dataset, Model, [alpha(0.5),initialization(first),seasonal(additive),retain_residuals(true)]),
		theta_forecasting::export_to_clauses(Dataset, Model, saved, [saved(FactCopy)]), FactCopy == Model,
		theta_forecasting::export_to_file(Dataset, Model, saved, 'test_output.pl'),
		open('test_output.pl', read, Stream),
		catch(read(Stream, saved(Copy)), Error, (close(Stream), throw(Error))), close(Stream),
		Copy == Model, ground(Copy), theta_forecasting::check_forecaster(Copy),
		theta_forecasting::forecast(Model, 8, Forecasts), theta_forecasting::forecast(Copy, 8, Forecasts),
		theta_forecasting::diagnostics(Copy, Diagnostics),
		memberchk(residual_indices([3,4,5,6,8,9,10,11,12,13]), Diagnostics),
		theta_forecasting::fitted_values(Dataset, Fits, [alpha(0.5),initialization(first),seasonal(additive)]),
		check_retained_diagnostics(Series, Fits, Diagnostics).

	test(theta_forecasting_retained_residuals_invalid_option, error(domain_error(option, retain_residuals(yes)))) :-
		theta_forecasting::learn(theta_series([10,12], none), _, [retain_residuals(yes)]).

	test(theta_forecasting_retained_residuals_variable_option, deterministic(var(Boolean))) :-
		\+ theta_forecasting::valid_option(retain_residuals(Boolean)).

	test(theta_forecasting_retained_residuals_bad_payloads, deterministic) :-
		theta_forecasting::learn(theta_series([10,12,14,16], none), Model,
			[alpha(0.5),initialization(first),retain_residuals(true)]),
		check_invalid_gap_fields([residuals(none),residual_indices(none),residuals([]),residual_indices([]),
			residuals([2,3]),residual_indices([2,3]),residuals([2,3,4,5]),residual_indices([2,3,4,5]),
			residuals([2,3,bad]),residuals([2-2,3-3,4-4]),residuals([2,3,_]),residuals([2,3|_]),
			residuals([2,3|bad]),residual_indices([1,3,4]),residual_indices([2,3,5]),
			residual_indices([2,2,4]),residual_indices([3,2,4]),residual_indices([2,3,4.0]),
			residual_indices([2,3,bad]),residual_indices([2,3,_]),residual_indices([2,3|_]),
			residual_indices([2,3|bad])], Model).

	test(theta_forecasting_retained_residuals_disabled_payloads, deterministic) :-
		learn_line(0.5, Model),
		check_invalid_gap_fields([residuals([]),residual_indices([]),residuals([2,3,3.5]),residual_indices([2,3,4])], Model).

	test(theta_forecasting_retained_residuals_missing_metadata, fail) :-
		learn_line(0.5, theta_forecaster(State, Diagnostics)), select(residuals(_), Diagnostics, Partial),
		theta_forecasting::valid_forecaster(theta_forecaster(State, Partial)).

	test(theta_forecasting_retained_residuals_missing_indices, fail) :-
		learn_line(0.5, theta_forecaster(State, Diagnostics)), select(residual_indices(_), Diagnostics, Partial),
		theta_forecasting::valid_forecaster(theta_forecaster(State, Partial)).

	test(theta_forecasting_retained_residuals_duplicate_metadata, fail) :-
		learn_line(0.5, theta_forecaster(State, Diagnostics)),
		theta_forecasting::valid_forecaster(theta_forecaster(State, [residuals(none)| Diagnostics])).

	test(theta_forecasting_retained_residuals_wrong_arity, fail) :-
		learn_line(0.5, Model), replace_field(Model, residual_indices(_), residual_indices(none,none), Invalid),
		theta_forecasting::valid_forecaster(Invalid).

	test(theta_forecasting_retained_residuals_old_options_rejected, fail) :-
		learn_line(0.5, Model),
		replace_field(Model, options(_), options([alpha(0.5),initialization(first),seasonal(none),frequency(1),missing_policy(skip_update)]), Invalid),
		theta_forecasting::valid_forecaster(Invalid).

	test(theta_forecasting_retained_residuals_no_total_recomputation, deterministic) :-
		theta_forecasting::learn(theta_series([10,12,14,16], none), Model,
			[alpha(0.5),initialization(first),retain_residuals(true)]),
		replace_field(Model, residuals(_), residuals([100,200,300]), Modified), !,
		theta_forecasting::check_forecaster(Modified).

	test(theta_forecasting_fitted_values_complete_oracle, deterministic) :-
		theta_forecasting::fitted_values(theta_series([10,12,14,16], none), [Anchor,Second,Third,Fourth],
			[alpha(0.5),initialization(first),seasonal(none)]),
		var(Anchor), Second =~= 10.0, Third =~= 11.0, Fourth =~= 12.5.

	test(theta_forecasting_fitted_values_gap_oracle, deterministic) :-
		Series = [10,Missing,14,16], copy_term(Series, Before),
		theta_forecasting::fitted_values(theta_series(Series, none), [Anchor,Gap,Third,Fourth],
			[alpha(0.5),initialization(first),seasonal(none)]),
		variant(Series, Before), var(Anchor), var(Gap), var(Missing),
		Anchor \== Gap, Anchor \== Missing, Gap \== Missing,
		Third =~= 10.0, Fourth =~= 12.0.

	test(theta_forecasting_fitted_values_leading_oracle, deterministic) :-
		theta_forecasting::fitted_values(theta_series([_,10,12], none), [Leading,Anchor,Third],
			[alpha(0.5),initialization(first)]),
		var(Leading), var(Anchor), Leading \== Anchor, Third =~= 10.0.

	test(theta_forecasting_fitted_values_trailing_oracle, deterministic) :-
		theta_forecasting::fitted_values(theta_series([10,12,_], none), [Anchor,Second,Trailing],
			[alpha(0.5),initialization(first)]),
		var(Anchor), var(Trailing), Anchor \== Trailing, Second =~= 10.0.

	test(theta_forecasting_fitted_values_default_options, deterministic) :-
		Dataset = theta_series([10,12,14,16], none),
		theta_forecasting::fitted_values(Dataset, Default),
		theta_forecasting::fitted_values(Dataset, Explicit, []),
		variant(Default, Explicit).

	test(theta_forecasting_fitted_values_alpha_zero, deterministic) :-
		theta_forecasting::fitted_values(theta_series([10,12,14,16], none), [Anchor,Second,Third,Fourth],
			[alpha(0),initialization(first)]),
		var(Anchor), Second =~= 10.0, Third =~= 10.0, Fourth =~= 10.0.

	test(theta_forecasting_fitted_values_alpha_one, deterministic) :-
		theta_forecasting::fitted_values(theta_series([10,12,14,16], none), [Anchor,Second,Third,Fourth],
			[alpha(1),initialization(first)]),
		var(Anchor), Second =~= 10.0, Third =~= 12.0, Fourth =~= 14.0.

	test(theta_forecasting_fitted_values_optimized_anchor, deterministic) :-
		Dataset = theta_series([_,10,_,14,16,_], none), Options = [alpha(0.5)],
		theta_forecasting::learn(Dataset, Model, Options),
		theta_forecasting::diagnostics(Model, Diagnostics), memberchk(initial_level(Initial), Diagnostics),
		theta_forecasting::fitted_values(Dataset, Values, Options),
		Values = [Leading,Anchor,Gap,First,_,Trailing],
		var(Leading), var(Anchor), var(Gap), var(Trailing),
		First =~= Initial,
		check_fitted_diagnostics([_,10,_,14,16,_], Values, Diagnostics).

	test(theta_forecasting_fitted_values_complete_metrics, deterministic) :-
		Series = [10,12,14,16], learn_line(0.5, Model),
		theta_forecasting::diagnostics(Model, Diagnostics),
		theta_forecasting::fitted_values(theta_series(Series, none), Values,
			[alpha(0.5),initialization(first),seasonal(none)]),
		check_fitted_diagnostics(Series, Values, Diagnostics).

	test(theta_forecasting_fitted_values_gap_metrics, deterministic) :-
		Series = [10,_,14,16], Dataset = theta_series(Series, none),
		Options = [alpha(0.5),initialization(first)],
		theta_forecasting::learn(Dataset, Model, Options),
		theta_forecasting::diagnostics(Model, Diagnostics),
		theta_forecasting::fitted_values(Dataset, Values, Options),
		check_fitted_diagnostics(Series, Values, Diagnostics).

	test(theta_forecasting_fitted_values_additive_oracle, deterministic) :-
		theta_forecasting::fitted_values(theta_series([8,12,8,12,8,12], 2), [Anchor,Second,Third,Fourth,Fifth,Sixth],
			[alpha(0.5),initialization(first),seasonal(additive)]),
		var(Anchor), Second =~= 12.0, Third =~= 8.0, Fourth =~= 12.0, Fifth =~= 8.0, Sixth =~= 12.0.

	test(theta_forecasting_fitted_values_multiplicative_oracle, deterministic) :-
		theta_forecasting::fitted_values(theta_series([5,15,5,15,5,15], 2), [Anchor,Second,Third,Fourth,Fifth,Sixth],
			[alpha(0.5),initialization(first),seasonal(multiplicative)]),
		var(Anchor), Second =~= 15.0, Third =~= 5.0, Fourth =~= 15.0, Fifth =~= 5.0, Sixth =~= 15.0.

	test(theta_forecasting_fitted_values_odd_training_phase, deterministic) :-
		theta_forecasting::fitted_values(theta_series([8,10,12,8,10,12,8], 3), [Anchor,Second,Third,Fourth,Fifth,Sixth,Seventh],
			[alpha(0.5),initialization(first),seasonal(additive)]),
		var(Anchor), Second =~= 10.0, Third =~= 12.0, Fourth =~= 8.0,
		Fifth =~= 10.0, Sixth =~= 12.0, Seventh =~= 8.0.

	test(theta_forecasting_fitted_values_seasonal_independent_placeholders, deterministic) :-
		Series = [Missing,12,8,12,8,12,Missing,12,8,12,8,12,8,Missing], copy_term(Series, Before),
		theta_forecasting::fitted_values(theta_series(Series, 2), Values,
			[alpha(0.5),initialization(first),seasonal(additive)]),
		Values = [Leading,Anchor,Third,Fourth,_,_,Gap,_,_,_,_,_,_,Trailing],
		variant(Series, Before), var(Missing), var(Leading), var(Anchor), var(Gap), var(Trailing),
		Leading \== Anchor, Leading \== Gap, Leading \== Trailing,
		Anchor \== Gap, Anchor \== Trailing, Gap \== Trailing,
		Leading \== Missing, Anchor \== Missing, Gap \== Missing, Trailing \== Missing,
		Third =~= 8.0, Fourth =~= 12.0.

	test(theta_forecasting_fitted_values_multiplicative_nonzero_oracle, deterministic) :-
		Series = [5,15,6,18,7,21,_,24,8,24,9,27], Dataset = theta_series(Series, 2),
		Options = [alpha(0),initialization(first),seasonal(multiplicative)],
		theta_forecasting::learn(Dataset, Model, Options),
		Model = theta_forecaster(theta_state(_,_,_,_,seasonal(_,_,_,[Low,High])), Diagnostics),
		ExpectedHigh is 5 * High / Low,
		theta_forecasting::fitted_values(Dataset, Values, Options),
		Values = [Anchor,Second,Third,Fourth,Fifth,Sixth,Gap,Eighth,Ninth,Tenth,Eleventh,Twelfth],
		var(Anchor), var(Gap), Anchor \== Gap,
		Second =~= ExpectedHigh, Third =~= 5.0, Fourth =~= ExpectedHigh,
		Fifth =~= 5.0, Sixth =~= ExpectedHigh, Eighth =~= ExpectedHigh,
		Ninth =~= 5.0, Tenth =~= ExpectedHigh, Eleventh =~= 5.0, Twelfth =~= ExpectedHigh,
		check_fitted_diagnostics(Series, Values, Diagnostics).

	test(theta_forecasting_fitted_values_auto_seasonal_multiplicative, deterministic) :-
		Series = [5,15,5,15,_,15,5,15,5,15,5,15,5,15,5,15,5,15,5,15,5,15,5,15],
		Dataset = theta_series(Series, 2),
		theta_forecasting::learn(Dataset, Model),
		theta_forecasting::diagnostics(Model, Diagnostics), memberchk(seasonal_mode(multiplicative), Diagnostics),
		theta_forecasting::fitted_values(Dataset, Values),
		check_fitted_diagnostics(Series, Values, Diagnostics).

	test(theta_forecasting_fitted_values_auto_seasonal_additive, deterministic) :-
		Series = [-2,2,-2,2,_,2,-2,2,-2,2,-2,2,-2,2,-2,2,-2,2,-2,2,-2,2,-2,2],
		Dataset = theta_series(Series, 2),
		theta_forecasting::learn(Dataset, Model),
		theta_forecasting::diagnostics(Model, Diagnostics), memberchk(seasonal_mode(additive), Diagnostics),
		theta_forecasting::fitted_values(Dataset, Values),
		check_fitted_diagnostics(Series, Values, Diagnostics).

	test(theta_forecasting_fitted_values_strict_complete, deterministic) :-
		Dataset = theta_series([10,12,14,16], none),
		theta_forecasting::fitted_values(Dataset, Skip, [alpha(0.5),initialization(first)]),
		theta_forecasting::fitted_values(Dataset, Strict, [alpha(0.5),initialization(first),missing_policy(error)]),
		variant(Skip, Strict).

	test(theta_forecasting_fitted_values_atom_observation, error(type_error(number, missing))) :-
		theta_forecasting::fitted_values(theta_series([10,missing,14], none), _).

	test(theta_forecasting_fitted_values_empty, error(domain_error(non_empty_series, theta_series([], none)))) :-
		theta_forecasting::fitted_values(theta_series([], none), _).

	test(theta_forecasting_fitted_values_one_known, error(domain_error(insufficient_known_observations, theta_series([_,10,_], none)))) :-
		theta_forecasting::fitted_values(theta_series([_,10,_], none), _).

	test(theta_forecasting_fitted_values_all_missing, error(domain_error(insufficient_known_observations, theta_series([_,_], none)))) :-
		theta_forecasting::fitted_values(theta_series([_,_], none), _).

	test(theta_forecasting_fitted_values_invalid_alpha, error(domain_error(option, alpha(2)))) :-
		theta_forecasting::fitted_values(theta_series([10,12], none), _, [alpha(2)]).

	test(theta_forecasting_fitted_values_variable_options, error(instantiation_error)) :-
		theta_forecasting::fitted_values(theta_series([10,12], none), _, _).

	test(theta_forecasting_fitted_values_open_options, error(instantiation_error)) :-
		theta_forecasting::fitted_values(theta_series([10,12], none), _, [alpha(0.5)| _]).

	test(theta_forecasting_fitted_values_invalid_optimizer, error(domain_error(option, optimizer_options([objective(maximize)])))) :-
		theta_forecasting::fitted_values(theta_series([10,12], none), _, [optimizer_options([objective(maximize)])]).

	test(theta_forecasting_fitted_values_strict_missing, error(instantiation_error)) :-
		theta_forecasting::fitted_values(theta_series([10,_,14], none), _, [missing_policy(error)]).

	test(theta_forecasting_fitted_values_unestimable_phase, error(domain_error(insufficient_seasonal_phase_observations, 2))) :-
		theta_forecasting::fitted_values(theta_series([8,12,_,12,8,12], 2), _, [seasonal(additive)]).

	test(theta_forecasting_fitted_values_irrelevant_frequency, error(domain_error(theta_forecasting_option, frequency(2)))) :-
		theta_forecasting::fitted_values(theta_series([10,12], 2), _, [seasonal(none),frequency(2)]).

	test(theta_forecasting_fitted_values_bad_dataset_frequency, error(type_error(integer, bad))) :-
		theta_forecasting::fitted_values(theta_series([10,12], bad), _).

	test(theta_forecasting_fitted_values_nonbinding_invalid_option, deterministic) :-
		Options = [alpha(Alpha)], copy_term(Options, Before),
		catch((theta_forecasting::fitted_values(theta_series([10,12], none), _, Options), fail),
			error(domain_error(option,alpha(_)),_), true),
		var(Alpha), variant(Options, Before).

	test(theta_forecasting_fixed_oracle, deterministic) :-
		learn_line(0.5, Model),
		theta_forecasting::forecast(Model, 3, [First,Second,Third]),
		First =~= 16.125, Second =~= 17.125, Third =~= 18.125,
		theta_forecasting::check_forecaster(Model).

	test(theta_forecasting_training_errors, deterministic) :-
		learn_line(0.5, Model),
		theta_forecasting::diagnostics(Model, Diagnostics),
		memberchk(scored_count(3), Diagnostics),
		memberchk(sum_absolute_error(Absolute), Diagnostics), Absolute =~= 8.5,
		memberchk(sum_squared_error(Squared), Diagnostics), Squared =~= 25.25,
		memberchk(mean_absolute_error(MAE), Diagnostics), ExpectedMAE is 8.5/3, MAE =~= ExpectedMAE,
		memberchk(root_mean_squared_error(RMSE), Diagnostics), ExpectedRMSE is sqrt(25.25/3), RMSE =~= ExpectedRMSE.

	test(theta_forecasting_alpha_zero, deterministic((First =~= 14.0, Second =~= 15.0, Third =~= 16.0))) :-
		learn_line(0, Model), theta_forecasting::forecast(Model, 3, [First,Second,Third]).

	test(theta_forecasting_alpha_one, deterministic((First =~= 17.0, Second =~= 18.0, Third =~= 19.0))) :-
		learn_line(1, Model), theta_forecasting::forecast(Model, 3, [First,Second,Third]).

	test(theta_forecasting_constant, deterministic((First =~= 7.0, Second =~= 7.0))) :-
		theta_forecasting::learn(theta_series([7,7,7,7], none), Model),
		theta_forecasting::forecast(Model, 2, [First,Second]).

	test(theta_forecasting_zero_horizon, deterministic(Values == [])) :-
		learn_line(0.5, Model), theta_forecasting::forecast(Model, 0, Values).

	test(theta_forecasting_export_fact, deterministic(Copy == Model)) :-
		learn_line(0.5, Model),
		theta_forecasting::export_to_clauses(theta_series([10,12,14,16], none), Model, saved, [saved(Copy)]).

	test(theta_forecasting_invalid_alpha, error(domain_error(option, alpha(2)))) :-
		theta_forecasting::learn(theta_series([1,2], none), _, [alpha(2)]).

	test(theta_forecasting_missing_observation, deterministic) :-
		Series = [1,Missing,3], copy_term(Series, Before),
		theta_forecasting::learn(theta_series(Series, none), Model),
		variant(Series, Before), var(Missing), ground(Model), theta_forecasting::check_forecaster(Model).

	test(theta_forecasting_missing_gap_oracle, deterministic) :-
		theta_forecasting::learn(theta_series([10,Missing,14,16], none), Model, [alpha(0.5),initialization(first)]),
		var(Missing), theta_forecasting::forecast(Model, 3, [First,Second,Third]),
		First =~= 15.875, Second =~= 16.875, Third =~= 17.875,
		theta_forecasting::diagnostics(Model, Diagnostics),
		memberchk(observed_count(3), Diagnostics), memberchk(missing_count(1), Diagnostics),
		memberchk(initialization_index(1), Diagnostics), memberchk(scored_count(2), Diagnostics),
		memberchk(sum_squared_error(SSE), Diagnostics), SSE =~= 32.0,
		memberchk(sum_absolute_error(SAE), Diagnostics), SAE =~= 8.0,
		memberchk(mean_absolute_error(MAE), Diagnostics), MAE =~= 4.0,
		memberchk(root_mean_squared_error(RMSE), Diagnostics), RMSE =~= 4.0.

	test(theta_forecasting_missing_leading_oracle, deterministic) :-
		theta_forecasting::learn(theta_series([_,10,12], none), Model, [alpha(0.5),initialization(first)]),
		theta_forecasting::forecast(Model, 1, [Forecast]), Forecast =~= 12.75,
		theta_forecasting::diagnostics(Model, Diagnostics),
		memberchk(initialization_index(2), Diagnostics), memberchk(scored_count(1), Diagnostics),
		memberchk(sum_squared_error(SSE), Diagnostics), SSE =~= 4.0.

	test(theta_forecasting_missing_trailing_oracle, deterministic) :-
		theta_forecasting::learn(theta_series([10,12,_], none), Model, [alpha(0.5),initialization(first)]),
		theta_forecasting::forecast(Model, 1, [Forecast]), Forecast =~= 12.75,
		theta_forecasting::learn(theta_series([10,12], none), Prefix, [alpha(0.5),initialization(first)]),
		theta_forecasting::forecast(Prefix, 1, [PrefixForecast]), PrefixForecast =~= 12.5.

	test(theta_forecasting_missing_optimized_anchor, deterministic) :-
		theta_forecasting::learn(theta_series([_,10,12], none), Model, [alpha(0.5)]),
		theta_forecasting::check_forecaster(Model), theta_forecasting::diagnostics(Model, Diagnostics),
		memberchk(initialization_index(2), Diagnostics), memberchk(scored_count(1), Diagnostics).

	test(theta_forecasting_missing_auto_alpha_first, deterministic) :-
		theta_forecasting::learn(theta_series([_,10,_,14,16,_], none), Model, [initialization(first)]),
		theta_forecasting::check_forecaster(Model), theta_forecasting::diagnostics(Model, Diagnostics),
		memberchk(scored_count(2), Diagnostics), memberchk(initialization_index(2), Diagnostics).

	test(theta_forecasting_missing_atom_rejected, error(type_error(number, missing))) :-
		theta_forecasting::learn(theta_series([10,missing,14,16], none), _).
	test(theta_forecasting_missing_compound_rejected, error(type_error(number, absent(value)))) :-
		theta_forecasting::learn(theta_series([10,absent(value),14,16], none), _).

	test(theta_forecasting_missing_strict_variable, error(instantiation_error)) :-
		theta_forecasting::learn(theta_series([1,_,3], none), _, [missing_policy(error)]).

	test(theta_forecasting_missing_strict_marker, error(type_error(number, missing))) :-
		theta_forecasting::learn(theta_series([1,missing,3], none), _, [missing_policy(error)]).

	test(theta_forecasting_missing_all, error(domain_error(insufficient_known_observations, theta_series([_,_], none)))) :-
		theta_forecasting::learn(theta_series([_,_], none), _).

	test(theta_forecasting_missing_one_known, error(domain_error(insufficient_known_observations, theta_series([_,1,_], none)))) :-
		theta_forecasting::learn(theta_series([_,1,_], none), _).

	test(theta_forecasting_missing_marker_option_rejected, error(domain_error(option, missing_value(missing)))) :-
		theta_forecasting::learn(theta_series([1,2], none), _, [missing_value(missing)]).

	test(theta_forecasting_missing_variable_policy, deterministic(var(Policy))) :-
		\+ theta_forecasting::valid_option(missing_policy(Policy)).

	test(theta_forecasting_nonnumeric, error(type_error(number, bad))) :-
		theta_forecasting::learn(theta_series([1,bad], none), _).

	test(theta_forecasting_short, error(domain_error(series_length, theta_series([1], none)))) :-
		theta_forecasting::learn(theta_series([1], none), _).

	test(theta_forecasting_empty, error(domain_error(non_empty_series, theta_series([], none)))) :-
		theta_forecasting::learn(theta_series([], none), _).

	test(theta_forecasting_invalid_model, error(domain_error(forecaster, wrong))) :-
		theta_forecasting::forecast(wrong, 0, _).

	test(theta_forecasting_variable_model, error(instantiation_error)) :-
		theta_forecasting::check_forecaster(_).

	test(theta_forecasting_negative_horizon, error(domain_error(non_negative_integer, -1))) :-
		learn_line(0.5, Model), theta_forecasting::forecast(Model, -1, _).

	test(theta_forecasting_missing_error_metadata, fail) :-
		learn_line(0.5, theta_forecaster(State, Diagnostics)),
		select(scored_count(_), Diagnostics, Partial),
		theta_forecasting::valid_forecaster(theta_forecaster(State, Partial)).

	test(theta_forecasting_nonbinding_validation, deterministic) :-
		Partial = theta_forecaster(theta_state(_,_,_,_,none), _), copy_term(Partial, Before),
		\+ theta_forecasting::valid_forecaster(Partial), variant(Partial, Before).

	test(theta_forecasting_auto_first_replay, deterministic) :-
		Dataset = theta_series([10,12,14,16], none),
		theta_forecasting::learn(Dataset, Automatic, [initialization(first)]),
		theta_forecasting::forecaster_options(Automatic, [alpha(Alpha),initialization(first),seasonal(none),frequency(1),missing_policy(skip_update),retain_residuals(false)]),
		theta_forecasting::learn(Dataset, Fixed, [alpha(Alpha),initialization(first)]),
		theta_forecasting::forecast(Automatic, 3, Forecasts),
		theta_forecasting::forecast(Fixed, 3, Forecasts),
		theta_forecasting::check_forecaster(Automatic).

	test(theta_forecasting_default_auto, deterministic) :-
		Dataset = theta_series([10,12,14,16], none),
		theta_forecasting::learn(Dataset, First), theta_forecasting::learn(Dataset, Second),
		First == Second, theta_forecasting::check_forecaster(First),
		theta_forecasting::diagnostics(First, Diagnostics),
		memberchk(fitting(nelder_mead), Diagnostics), memberchk(scored_count(3), Diagnostics).

	test(theta_forecasting_fixed_alpha_optimized_level, deterministic) :-
		theta_forecasting::learn(theta_series([10,12,14,16], none), Model, [alpha(0.5)]),
		theta_forecasting::check_forecaster(Model),
		theta_forecasting::diagnostics(Model, Diagnostics), memberchk(fitting(nelder_mead), Diagnostics).

	test(theta_forecasting_iteration_limit, deterministic) :-
		theta_forecasting::learn(theta_series([10,12,14,16], none), Model, [optimizer_options([max_iterations(1)])]),
		theta_forecasting::check_forecaster(Model),
		theta_forecasting::diagnostics(Model, Diagnostics), memberchk(convergence(maximum_iterations), Diagnostics).

	test(theta_forecasting_tiny_alpha, deterministic) :-
		learn_line(0.000000000001, Model), theta_forecasting::forecast(Model, 1, [First]), First =~= 14.0.

	test(theta_forecasting_invalid_optimizer_option, error(domain_error(option, optimizer_options([objective(maximize)])))) :-
		theta_forecasting::learn(theta_series([1,2], none), _, [optimizer_options([objective(maximize)])]).

	test(theta_forecasting_duplicate_optimizer_options, fail) :-
		theta_forecasting::valid_option(optimizer_options([tol_x(0.1),tol_x(0.2)])).

	test(theta_forecasting_seasonal_additive, deterministic) :-
		learn_season([8,12,8,12,8,12], 2, additive, Model),
		theta_forecasting::forecast(Model, 4, [First,Second,Third,Fourth]),
		First =~= 8.0, Second =~= 12.0, Third =~= 8.0, Fourth =~= 12.0,
		theta_forecasting::check_forecaster(Model).

	test(theta_forecasting_seasonal_multiplicative, deterministic) :-
		learn_season([5,15,5,15,5,15], 2, multiplicative, Model),
		theta_forecasting::forecast(Model, 3, [First,Second,Third]),
		First =~= 5.0, Second =~= 15.0, Third =~= 5.0,
		theta_forecasting::check_forecaster(Model).

	test(theta_forecasting_seasonal_partial_cycle, deterministic((First =~= 10.0, Second =~= 12.0, Third =~= 8.0, Fourth =~= 10.0))) :-
		learn_season([8,10,12,8,10,12,8], 3, additive, Model),
		theta_forecasting::forecast(Model, 4, [First,Second,Third,Fourth]).

	test(theta_forecasting_auto_multiplicative, deterministic) :-
		learn_season([5,15,5,15,5,15,5,15,5,15,5,15], 2, auto, Model),
		theta_forecasting::forecaster_options(Model, [alpha(_),initialization(first),seasonal(multiplicative),frequency(2),missing_policy(skip_update),retain_residuals(false)]),
		theta_forecasting::check_forecaster(Model).

	test(theta_forecasting_auto_additive_signed, deterministic) :-
		learn_season([-2,2,-2,2,-2,2,-2,2,-2,2,-2,2], 2, auto, Model),
		theta_forecasting::forecaster_options(Model, [alpha(_),initialization(first),seasonal(additive),frequency(2),missing_policy(skip_update),retain_residuals(false)]),
		theta_forecasting::forecast(Model, 2, [First,Second]), First =~= -2.0, Second =~= 2.0.

	test(theta_forecasting_auto_two_cycles_nonseasonal, deterministic) :-
		learn_season([5,15,5,15], 2, auto, Model),
		theta_forecasting::forecaster_options(Model, [alpha(_),initialization(first),seasonal(none),frequency(1),missing_policy(skip_update),retain_residuals(false)]).

	test(theta_forecasting_irrelevant_frequency, error(domain_error(theta_forecasting_option, frequency(2)))) :-
		theta_forecasting::learn(theta_series([1,2,3], 2), _, [seasonal(none),frequency(2)]).

	test(theta_forecasting_forced_missing_frequency, error(domain_error(seasonal_frequency, theta_series([1,2,3,4], none)))) :-
		theta_forecasting::learn(theta_series([1,2,3,4], none), _, [seasonal(additive)]).

	test(theta_forecasting_dataset_bad_frequency, error(type_error(integer, bad))) :-
		theta_forecasting::learn(theta_series([1,2,3,4], bad), _).

	test(theta_forecasting_frequency_override, deterministic) :-
		theta_forecasting::learn(theta_series([8,12,8,12,8,12], bad), Model,
			[alpha(0.5),initialization(first),seasonal(additive),frequency(2)]),
		theta_forecasting::check_forecaster(Model).

	test(theta_forecasting_raw_scale_multiplicative_metrics, deterministic) :-
		learn_season([5,15,5,15,5,15], 2, multiplicative, Model),
		theta_forecasting::diagnostics(Model, Diagnostics), memberchk(scored_count(5), Diagnostics),
		memberchk(sum_squared_error(Squared), Diagnostics), Squared =~= 0.0.

	test(theta_forecasting_multiplicative_nonzero_error_oracle, deterministic) :-
		theta_forecasting::learn(theta_series([5,15,6,18,7,21], 2), Model,
			[alpha(0),initialization(first),seasonal(multiplicative)]),
		Ratio is ((60/41 + 72/49)/2) / ((8/15 + 28/53)/2),
		FirstError is 15 - 5*Ratio, ThirdError is 18 - 5*Ratio, FifthError is 21 - 5*Ratio,
		ExpectedSSE is FirstError*FirstError + 1 + ThirdError*ThirdError + 4 + FifthError*FifthError,
		ExpectedSAE is abs(FirstError) + 1 + abs(ThirdError) + 2 + abs(FifthError),
		theta_forecasting::diagnostics(Model, Diagnostics),
		memberchk(sum_squared_error(SSE), Diagnostics), SSE =~= ExpectedSSE,
		memberchk(sum_absolute_error(SAE), Diagnostics), SAE =~= ExpectedSAE,
		memberchk(optimization_sum_squared_error(Objective), Diagnostics),
		abs(SSE - Objective) > 1,
		theta_forecasting::check_forecaster(Model).

	test(theta_forecasting_seasonal_auto_fitting, deterministic) :-
		theta_forecasting::learn(theta_series([5,15,5,15,5,15,5,15,5,15,5,15], 2), Model),
		theta_forecasting::check_forecaster(Model), theta_forecasting::forecast(Model, 0, []).

	test(theta_forecasting_nonseasonal_ignores_dataset_frequency, deterministic) :-
		theta_forecasting::learn(theta_series([1,2,3], bad), Model, [seasonal(none)]),
		theta_forecasting::check_forecaster(Model).

	test(theta_forecasting_forced_short, error(domain_error(series_length, [1,2,3]))) :-
		theta_forecasting::learn(theta_series([1,2,3], 2), _, [seasonal(additive)]).

	test(theta_forecasting_forced_frequency_one, error(domain_error(seasonal_frequency, 1))) :-
		theta_forecasting::learn(theta_series([1,2,3], 1), _, [seasonal(additive)]).

	test(theta_forecasting_forced_multiplicative_zero, error(domain_error(positive_number, 0))) :-
		learn_season([0,2,0,2], 2, multiplicative, _).

	test(theta_forecasting_auto_frequency_one, deterministic) :-
		learn_season([1,2,3], 1, auto, Model), theta_forecasting::check_forecaster(Model).

	test(theta_forecasting_variable_option_values, deterministic) :-
		\+ theta_forecasting::valid_option(initialization(Initialization)), var(Initialization),
		\+ theta_forecasting::valid_option(seasonal(Mode)), var(Mode),
		\+ theta_forecasting::valid_option(alpha(Alpha)), var(Alpha),
		\+ theta_forecasting::valid_option(frequency(Frequency)), var(Frequency),
		\+ theta_forecasting::valid_option(optimizer_options([adaptive(Adaptive)])), var(Adaptive).

	test(theta_forecasting_open_optimizer_options, deterministic) :-
		Options = [tol_x(0.1)| Tail], copy_term(Options, Before),
		\+ theta_forecasting::valid_option(optimizer_options(Options)), var(Tail), variant(Options, Before).

	test(theta_forecasting_invalid_frequency_option, error(domain_error(option, frequency(0)))) :-
		theta_forecasting::learn(theta_series([1,2], none), _, [frequency(0)]).

	test(theta_forecasting_invalid_initialization_option, error(domain_error(option, initialization(bad)))) :-
		theta_forecasting::learn(theta_series([1,2], none), _, [initialization(bad)]).

	test(theta_forecasting_invalid_seasonal_option, error(domain_error(option, seasonal(bad)))) :-
		theta_forecasting::learn(theta_series([1,2], none), _, [seasonal(bad)]).

	test(theta_forecasting_variable_options, error(instantiation_error)) :-
		theta_forecasting::learn(theta_series([1,2], none), _, _).

	test(theta_forecasting_atom_options, error(type_error(list, bad))) :-
		theta_forecasting::learn(theta_series([1,2], none), _, bad).

	test(theta_forecasting_atom_option, error(type_error(compound, bad))) :-
		theta_forecasting::learn(theta_series([1,2], none), _, [bad]).

	test(theta_forecasting_variable_horizon, error(instantiation_error)) :-
		learn_line(0.5, Model), theta_forecasting::forecast(Model, _, _).

	test(theta_forecasting_noninteger_horizon, error(type_error(integer, 1.5))) :-
		learn_line(0.5, Model), theta_forecasting::forecast(Model, 1.5, _).

	test(theta_forecasting_invalid_functor, error(type_error(atom, 1))) :-
		learn_line(0.5, Model), theta_forecasting::export_to_clauses(any, Model, 1, _).

	test(theta_forecasting_export_roundtrip, deterministic) :-
		Dataset = theta_series([8,10,12,8,10,12,8], 3),
		learn_season([8,10,12,8,10,12,8], 3, additive, Model),
		theta_forecasting::export_to_file(Dataset, Model, saved, 'test_output.pl'),
		open('test_output.pl', read, Stream),
		catch(read(Stream, saved(Copy)), Error, (close(Stream), throw(Error))), close(Stream),
		Copy == Model, theta_forecasting::check_forecaster(Copy),
		theta_forecasting::forecast(Copy, 8, Forecasts), theta_forecasting::forecast(Model, 8, Forecasts).

	test(theta_forecasting_invalid_zero_horizon, error(domain_error(forecaster, wrong))) :-
		theta_forecasting::forecast(wrong, 0, _).

	test(theta_forecasting_print_model, deterministic) :-
		^^suppress_text_output, learn_line(0.5, Model), theta_forecasting::print_forecaster(Model).

	test(theta_forecasting_model_immutability, deterministic) :-
		learn_line(0.5, Model), copy_term(Model, Before),
		theta_forecasting::forecast(Model, 10, _), theta_forecasting::forecast(Model, 2, _), Model == Before.

	test(theta_forecasting_each_missing_diagnostic, deterministic) :-
		learn_line(0.5, theta_forecaster(State, Diagnostics)),
		check_missing_fields(Diagnostics, State, Diagnostics).

	test(theta_forecasting_duplicate_diagnostic, fail) :-
		learn_line(0.5, theta_forecaster(State, Diagnostics)),
		theta_forecasting::valid_forecaster(theta_forecaster(State, [scored_count(3)| Diagnostics])).

	test(theta_forecasting_wrong_arity_diagnostic, fail) :-
		learn_line(0.5, theta_forecaster(State, Diagnostics)),
		select(scored_count(_), Diagnostics, Rest),
		theta_forecasting::valid_forecaster(theta_forecaster(State, [scored_count(3,4)| Rest])).

	test(theta_forecasting_inconsistent_correction, fail) :-
		learn_line(0.5, theta_forecaster(theta_state(Level,Slope,Alpha,_,Seasonal), Diagnostics)),
		theta_forecasting::valid_forecaster(theta_forecaster(theta_state(Level,Slope,Alpha,2,Seasonal), Diagnostics)).

	test(theta_forecasting_inconsistent_metrics, fail) :-
		learn_line(0.5, Model), replace_field(Model, mean_absolute_error(_), mean_absolute_error(0), Invalid),
		theta_forecasting::valid_forecaster(Invalid).

	test(theta_forecasting_inconsistent_count, fail) :-
		learn_line(0.5, Model), replace_field(Model, scored_count(_), scored_count(bad), Invalid),
		theta_forecasting::valid_forecaster(Invalid).

	test(theta_forecasting_inconsistent_options, fail) :-
		learn_line(0.5, Model), replace_field(Model, options(_), options([alpha(auto)]), Invalid),
		theta_forecasting::valid_forecaster(Invalid).

	test(theta_forecasting_invalid_convergence, fail) :-
		learn_line(0.5, Model), replace_field(Model, convergence(_), convergence(converged), Invalid),
		theta_forecasting::valid_forecaster(Invalid).

	test(theta_forecasting_invalid_seasonal_phase, fail) :-
		learn_season([8,12,8,12], 2, additive, theta_forecaster(theta_state(Level,Slope,Alpha,Correction,seasonal(Mode,Period,_,Factors)), Diagnostics)),
		theta_forecasting::valid_forecaster(theta_forecaster(theta_state(Level,Slope,Alpha,Correction,seasonal(Mode,Period,2,Factors)), Diagnostics)).

	test(theta_forecasting_invalid_seasonal_factors, fail) :-
		learn_season([5,15,5,15], 2, multiplicative, theta_forecaster(theta_state(Level,Slope,Alpha,Correction,seasonal(Mode,Period,Phase,_)), Diagnostics)),
		theta_forecasting::valid_forecaster(theta_forecaster(theta_state(Level,Slope,Alpha,Correction,seasonal(Mode,Period,Phase,[0,1])), Diagnostics)).

	% auxiliary predicates
	test(theta_forecasting_missing_seasonal_additive_oracle, deterministic) :-
		learn_season([8,12,8,12,_,12,8,12,8,12,8,12], 2, additive, Model),
		theta_forecasting::forecast(Model, 3, [First,Second,Third]),
		First =~= 8.0, Second =~= 12.0, Third =~= 8.0,
		theta_forecasting::diagnostics(Model, Diagnostics),
		memberchk(observed_count(11), Diagnostics), memberchk(scored_count(10), Diagnostics),
		memberchk(sum_squared_error(SSE), Diagnostics), SSE =~= 0.0.

	test(theta_forecasting_missing_seasonal_anchor_and_phase, deterministic) :-
		learn_season([_,12,8,12,8,12,_,12,8,12,8,12,8,_], 2, additive, Model),
		theta_forecasting::forecast(Model, 2, [First,Second]), First =~= 8.0, Second =~= 12.0,
		theta_forecasting::diagnostics(Model, Diagnostics),
		memberchk(initialization_index(2), Diagnostics), memberchk(scored_count(10), Diagnostics),
		memberchk(sum_squared_error(SSE), Diagnostics), SSE =~= 0.0.

	test(theta_forecasting_missing_seasonal_fitting_combinations, deterministic) :-
		check_gap_fitting([first-0.5,first-auto,optimized-0.5,optimized-auto],
			[_,15,5,15,5,15,_,15,5,15,5,15,5,_], multiplicative).
	test(theta_forecasting_missing_additive_fitting_combinations, deterministic) :-
		check_gap_fitting([first-0.5,first-auto,optimized-0.5,optimized-auto],
			[_,12,8,12,8,12,_,12,8,12,8,12,8,_], additive).
	test(theta_forecasting_missing_nonseasonal_fitting_combinations, deterministic) :-
		check_gap_fitting([first-0.5,first-auto,optimized-0.5,optimized-auto],
			[_,10,14,16,18,20,_,22,24,26,28,30,32,_], none).

	test(theta_forecasting_missing_auto_multiplicative, deterministic) :-
		learn_season([5,15,5,15,_,15,5,15,5,15,5,15,5,15,5,15,5,15,5,15,5,15,5,15], 2, auto, Model),
		theta_forecasting::diagnostics(Model, Diagnostics), memberchk(seasonal_mode(multiplicative), Diagnostics),
		theta_forecasting::forecast(Model, 2, [First,Second]), First =~= 5.0, Second =~= 15.0.
	test(theta_forecasting_missing_auto_additive, deterministic) :-
		learn_season([-2,2,-2,2,_,2,-2,2,-2,2,-2,2,-2,2,-2,2,-2,2,-2,2,-2,2,-2,2], 2, auto, Model),
		theta_forecasting::diagnostics(Model, Diagnostics), memberchk(seasonal_mode(additive), Diagnostics).
	test(theta_forecasting_missing_auto_insufficient_evidence, deterministic) :-
		learn_season([8,12,_,_,8,12,_], 2, auto, Model),
		theta_forecasting::diagnostics(Model, Diagnostics), memberchk(seasonal_mode(none), Diagnostics).
	test(theta_forecasting_missing_forced_phase_error, error(domain_error(insufficient_seasonal_phase_observations, 2))) :-
		learn_season([8,12,_,12,8,12], 2, additive, _).
	test(theta_forecasting_missing_auto_phase_error, error(domain_error(insufficient_seasonal_phase_observations, 1))) :-
		learn_season([8,12,_,12,8,_,8,12,_,12,8,_,8,12,_,12,8,_,
			8,12,_,12,8,_,8,12,_,12,8,_,8,12,_,12,8,_], 2, auto, _).

	test(theta_forecasting_missing_multiplicative_error_alignment, deterministic) :-
		theta_forecasting::learn(theta_series([5,15,6,18,7,21,_,24,8,24,9,27], 2),
			theta_forecaster(State, Diagnostics), [alpha(0),initialization(first),seasonal(multiplicative)]),
		State = theta_state(_,_,_,_,seasonal(_,_,_,[Low,High])), Prediction is 5 * High / Low,
		Expected is 30 + (15-Prediction)**2 + (18-Prediction)**2 + (21-Prediction)**2 + 2*(24-Prediction)**2 + (27-Prediction)**2,
		memberchk(sum_squared_error(SSE), Diagnostics), SSE =~= Expected,
		memberchk(scored_count(10), Diagnostics), theta_forecasting::check_forecaster(theta_forecaster(State, Diagnostics)).

	test(theta_forecasting_missing_seasonal_export_roundtrip, deterministic) :-
		Dataset = theta_series([_,12,8,12,8,12,_,12,8,12,8,12,8,_], 2),
		theta_forecasting::learn(Dataset, Model, [alpha(0.5),initialization(first),seasonal(additive)]),
		theta_forecasting::export_to_file(Dataset, Model, saved, 'test_output.pl'),
		open('test_output.pl', read, Stream),
		catch(read(Stream, saved(Copy)), Error, (close(Stream), throw(Error))), close(Stream),
		Copy == Model, ground(Copy), theta_forecasting::check_forecaster(Copy),
		theta_forecasting::forecast(Copy, 8, Forecasts), theta_forecasting::forecast(Model, 8, Forecasts).

	test(theta_forecasting_missing_metadata_validation, deterministic) :-
		theta_forecasting::learn(theta_series([_,10,_,14,16,_], none), Model, [alpha(0.5),initialization(first)]),
		check_invalid_gap_fields([observed_count(bad),observed_count(4),missing_count(-1),missing_count(bad),
			initialization_index(0),initialization_index(5),initialization_index(bad),scored_count(3)], Model).
	test(theta_forecasting_missing_strict_model_inconsistent, fail) :-
		theta_forecasting::learn(theta_series([10,_,14,16], none), Model, [alpha(0.5),initialization(first)]),
		replace_field(Model, options(_), options([alpha(0.5),initialization(first),seasonal(none),frequency(1),missing_policy(error),retain_residuals(false)]), Invalid),
		theta_forecasting::valid_forecaster(Invalid).
	test(theta_forecasting_missing_complete_policy_replay, deterministic(State == StrictState)) :-
		theta_forecasting::learn(theta_series([10,12,14,16], none), theta_forecaster(State,_), [alpha(0.5),initialization(first)]),
		theta_forecasting::learn(theta_series([10,12,14,16], none), Strict, [alpha(0.5),initialization(first),missing_policy(error)]),
		Strict = theta_forecaster(StrictState,_), theta_forecasting::check_forecaster(Strict).

	check_gap_fitting([], _, _) :- !.
	check_gap_fitting([Initialization-Alpha| Options], Series, Mode) :-
		Dataset = theta_series(Series, 2), FitOptions = [initialization(Initialization),alpha(Alpha),seasonal(Mode)],
		copy_term(Series, Before),
		theta_forecasting::learn(Dataset, Model, FitOptions),
		theta_forecasting::check_forecaster(Model), theta_forecasting::forecast(Model, 3, _),
		theta_forecasting::diagnostics(Model, Diagnostics),
		memberchk(initialization_index(2), Diagnostics), memberchk(scored_count(10), Diagnostics),
		theta_forecasting::fitted_values(Dataset, Values, FitOptions),
		variant(Series, Before), ground(Model),
		check_fitted_diagnostics(Series, Values, Diagnostics),
		theta_forecasting::learn(Dataset, Retained, [retain_residuals(true)| FitOptions]),
		Retained = theta_forecaster(RetainedState, RetainedDiagnostics), Model = theta_forecaster(State,_),
		RetainedState == State, theta_forecasting::check_forecaster(Retained),
		theta_forecasting::forecast(Retained, 3, Forecasts), theta_forecasting::forecast(Model, 3, Forecasts),
		theta_forecasting::fitted_values(Dataset, RetainedFits, [retain_residuals(true)| FitOptions]),
		variant(Values, RetainedFits), variant(Series, Before), ground(Retained),
		check_retained_diagnostics(Series, RetainedFits, RetainedDiagnostics),
		select(options(_), Diagnostics, WithoutOptions), select(residuals(_), WithoutOptions, WithoutResiduals),
		select(residual_indices(_), WithoutResiduals, OriginalMetadata), !,
		select(options(_), RetainedDiagnostics, RetainedWithoutOptions), select(residuals(_), RetainedWithoutOptions, RetainedWithoutResiduals),
		select(residual_indices(_), RetainedWithoutResiduals, RetainedMetadata), !,
		OriginalMetadata == RetainedMetadata,
		check_gap_fitting(Options, Series, Mode).

	:- private(check_retained_diagnostics/3).
	:- mode(check_retained_diagnostics(+list, +list, +list(compound)), one_or_error).
	:- info(check_retained_diagnostics/3, [
		comment is 'Independently checks retained numeric errors, original targets and all five aggregate metrics.',
		argnames is ['Series', 'Fits', 'Diagnostics'],
		exceptions is ['Residual-error arithmetic fails' - evaluation_error('Error')]
	]).

	check_retained_diagnostics(Series, Fits, Diagnostics) :-
		memberchk(residuals(Errors), Diagnostics), memberchk(residual_indices(Indices), Diagnostics),
		check_retained_targets(Errors, Indices, Series, Fits),
		retained_error_totals(Errors, forecast_error_totals(0,0,0), forecast_error_totals(Count,Absolute,Squared)),
		memberchk(scored_count(Count), Diagnostics),
		memberchk(sum_absolute_error(StoredAbsolute), Diagnostics), StoredAbsolute =~= Absolute,
		memberchk(sum_squared_error(StoredSquared), Diagnostics), StoredSquared =~= Squared,
		memberchk(mean_absolute_error(MAE), Diagnostics), ExpectedMAE is Absolute / Count, MAE =~= ExpectedMAE,
		memberchk(root_mean_squared_error(RMSE), Diagnostics), ExpectedRMSE is sqrt(Squared / Count), RMSE =~= ExpectedRMSE.

	:- private(check_retained_targets/4).
	:- mode(check_retained_targets(+list(number), +list(integer), +list, +list), one_or_error).
	:- info(check_retained_targets/4, [
		comment is 'Checks residual signs and alignment against the original series and independent fitted values.',
		argnames is ['Errors', 'Indices', 'Series', 'Fits'],
		exceptions is ['Residual comparison arithmetic fails' - evaluation_error('Error')]
	]).

	check_retained_targets([], [], _, _) :- !.
	check_retained_targets([Error| Errors], [Index| Indices], Series, Fits) :-
		nth1(Index, Series, Actual), nth1(Index, Fits, Fit), number(Actual), number(Fit),
		Expected is Actual - Fit, Error =~= Expected,
		check_retained_targets(Errors, Indices, Series, Fits).

	:- private(retained_error_totals/3).
	:- mode(retained_error_totals(+list(number), +compound, -compound), one_or_error).
	:- info(retained_error_totals/3, [
		comment is 'Independently accumulates retained numeric residuals in chronological order.',
		argnames is ['Errors', 'Totals0', 'Totals'],
		exceptions is ['Residual accumulation arithmetic fails' - evaluation_error('Error')]
	]).

	retained_error_totals([], Totals, Totals) :- !.
	retained_error_totals([Error| Errors], forecast_error_totals(Count0,Absolute0,Squared0), Totals) :-
		Count is Count0 + 1, Absolute is Absolute0 + abs(Error), Squared is Squared0 + Error * Error,
		retained_error_totals(Errors, forecast_error_totals(Count,Absolute,Squared), Totals).

	:- private(check_fitted_diagnostics/3).
	:- mode(check_fitted_diagnostics(+list, +list, +list(compound)), one_or_error).
	:- info(check_fitted_diagnostics/3, [
		comment is 'Checks all five stored training-error terms against aligned numeric fitted targets.',
		argnames is ['Series', 'Values', 'Diagnostics'],
		exceptions is ['Error reconstruction arithmetic fails' - evaluation_error('Error')]
	]).

	check_fitted_diagnostics(Series, Values, Diagnostics) :-
		fitted_error_totals(Series, Values, Count, Absolute, Squared),
		memberchk(scored_count(Count), Diagnostics),
		memberchk(sum_absolute_error(StoredAbsolute), Diagnostics), StoredAbsolute =~= Absolute,
		memberchk(sum_squared_error(StoredSquared), Diagnostics), StoredSquared =~= Squared,
		memberchk(mean_absolute_error(MAE), Diagnostics), ExpectedMAE is Absolute / Count, MAE =~= ExpectedMAE,
		memberchk(root_mean_squared_error(RMSE), Diagnostics), ExpectedRMSE is sqrt(Squared / Count), RMSE =~= ExpectedRMSE.

	:- private(fitted_error_totals/5).
	:- mode(fitted_error_totals(+list, +list, -non_negative_integer, -number, -number), one_or_error).
	:- info(fitted_error_totals/5, [
		comment is 'Independently reconstructs score count and error sums, requiring equal elapsed lengths and skipping placeholders.',
		argnames is ['Series', 'Values', 'Count', 'Absolute', 'Squared'],
		exceptions is ['Error reconstruction arithmetic fails' - evaluation_error('Error')]
	]).

	fitted_error_totals([], [], 0, 0, 0) :- !.
	fitted_error_totals([Actual| Actuals], [Fit| Fits], Count, Absolute, Squared) :-
		fitted_error_totals(Actuals, Fits, Count0, Absolute0, Squared0),
		(	var(Fit) ->
			Count = Count0, Absolute = Absolute0, Squared = Squared0
		;	number(Actual), Error is Actual - Fit,
			Count is Count0 + 1, Absolute is Absolute0 + abs(Error), Squared is Squared0 + Error * Error
		).

	check_invalid_gap_fields([], _) :- !.
	check_invalid_gap_fields([Field| Fields], Model) :-
		functor(Field, Name, 1), functor(Old, Name, 1), replace_field(Model, Old, Field, Invalid), !,
		copy_term(Invalid, Before), \+ theta_forecasting::valid_forecaster(Invalid), variant(Invalid, Before),
		catch((theta_forecasting::check_forecaster(Invalid), fail), error(domain_error(forecaster,Invalid),_), true),
		check_invalid_gap_fields(Fields, Model).

	check_missing_fields([], _, _).
	check_missing_fields([Field| Fields], State, Diagnostics) :-
		select(Field, Diagnostics, Partial), !,
		\+ theta_forecasting::valid_forecaster(theta_forecaster(State, Partial)),
		check_missing_fields(Fields, State, Diagnostics).

	replace_field(theta_forecaster(State, Diagnostics), Old, New, theta_forecaster(State, [New| Rest])) :-
		select(Old, Diagnostics, Rest).

	learn_season(Series, Frequency, Mode, Model) :-
		theta_forecasting::learn(theta_series(Series, Frequency), Model,
			[alpha(0.5),initialization(first),seasonal(Mode)]).

	learn_line(Alpha, Model) :-
		theta_forecasting::learn(theta_series([10,12,14,16], none), Model, [alpha(Alpha),initialization(first),seasonal(none)]).

:- end_object.
