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


:- object(sample_forecaster,
	imports([options, forecaster_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-27,
		comment is 'Minimal naive-forecast forecaster used to exercise the forecaster_protocol and forecaster_common shared code end-to-end.'
	]).

	:- uses(list, [
		length/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, sample_forecaster(Series, Diagnostics), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_series(Dataset, Series0),
		^^check_series(Dataset, Series0),
		^^check_series_length(Dataset, Series0, 1),
		Series = Series0,
		length(Series, TrainingSeriesLength),
		build_diagnostics(TrainingSeriesLength, Options, Diagnostics).

	forecast(sample_forecaster(Series, _Diagnostics), Horizon, Forecasts) :-
		^^naive_forecast(Series, Horizon, Forecasts).

	build_diagnostics(TrainingSeriesLength, Options, Diagnostics) :-
		^^base_forecaster_diagnostics(sample_forecaster, TrainingSeriesLength, Options, [], Diagnostics).

	check_forecaster(Forecaster) :-
		(	var(Forecaster) ->
			instantiation_error
		;	Forecaster = sample_forecaster(Series, Diagnostics),
			valid(non_empty_list(number), Series),
			^^valid_forecaster_metadata(sample_forecaster, _Options, Diagnostics) ->
			true
		;	domain_error(forecaster, Forecaster)
		).

	forecaster_export_template(_Dataset, _Forecaster, Functor, Template) :-
		Template =.. [Functor, 'Forecaster'].

	forecaster_term_template(sample_forecaster(_Series, _Diagnostics), sample_forecaster('Series', 'Diagnostics')).

	export_to_clauses(_Dataset, Forecaster, Functor, [Clause]) :-
		Clause =.. [Functor, Forecaster].

	print_forecaster(Forecaster) :-
		^^print_forecaster_template(Forecaster),
		writeq(Forecaster), nl.

	default_option(sample_option(enabled)).

	valid_option(sample_option(Value)) :-
		once((Value == enabled; Value == disabled)).

:- end_object.
