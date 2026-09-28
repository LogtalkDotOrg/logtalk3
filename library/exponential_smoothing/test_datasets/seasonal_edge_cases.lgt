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


:- object(seasonal_additive_trend_partial,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-27,
		comment is 'Additive seasonal series with a nonzero trend ending partway through a cycle.'
	]).

	observation(1, 9).
	observation(2, 19).
	observation(3, 17).
	observation(4, 15).
	observation(5, 17).
	observation(6, 27).
	observation(7, 25).
	observation(8, 23).
	observation(9, 25).
	observation(10, 35).
	observation(11, 33).

	series_length(11).

	frequency(4).

:- end_object.


:- object(unit_frequency_seasonal,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-27,
		comment is 'Degenerate seasonal series with frequency one.'
	]).

	observation(1, 10).
	observation(2, 11).
	observation(3, 12).

	series_length(3).

	frequency(1).

:- end_object.


:- object(seasonal_additive_many_cycles,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-27,
		comment is 'Six repetitions of a two-position additive cycle used to exercise multiple seasonal queue rotations.'
	]).

	observation(1, 10).
	observation(2, 20).
	observation(3, 10).
	observation(4, 20).
	observation(5, 10).
	observation(6, 20).
	observation(7, 10).
	observation(8, 20).
	observation(9, 10).
	observation(10, 20).
	observation(11, 10).
	observation(12, 20).

	series_length(12).

	frequency(2).

:- end_object.


:- object(minimum_length_seasonal,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-27,
		comment is 'Seasonal series with exactly two cycles plus one observation.'
	]).

	observation(1, 10).
	observation(2, 20).
	observation(3, 10).
	observation(4, 20).
	observation(5, 10).

	series_length(5).

	frequency(2).

:- end_object.


:- object(adverse_multiplicative_trajectory,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-27,
		comment is 'Positive multiplicative seasonal series with a sharp downward shift.'
	]).

	observation(1, 1000).
	observation(2, 1000).
	observation(3, 10).
	observation(4, 10).
	observation(5, 10).
	observation(6, 10).

	series_length(6).

	frequency(2).

:- end_object.


:- object(seasonal_multiplicative_trend_partial,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-27,
		comment is 'Positive multiplicative seasonal series with a nonzero trend ending partway through a cycle.'
	]).

	observation(1, 88).
	observation(2, 144).
	observation(3, 130).
	observation(4, 154).
	observation(5, 120).
	observation(6, 192).
	observation(7, 170).
	observation(8, 198).
	observation(9, 152).
	observation(10, 240).
	observation(11, 210).

	series_length(11).

	frequency(4).

:- end_object.


:- object(long_linear_trend,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-27,
		comment is 'Long exact linear trend used for automatic model selection.'
	]).

	observation(1, 2).
	observation(2, 4).
	observation(3, 6).
	observation(4, 8).
	observation(5, 10).
	observation(6, 12).
	observation(7, 14).
	observation(8, 16).
	observation(9, 18).
	observation(10, 20).

	series_length(10).

:- end_object.
