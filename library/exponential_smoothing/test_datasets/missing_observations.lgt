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


:- object(missing_nonseasonal,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-27,
		comment is 'Regular nonseasonal series with leading, consecutive, intermediate, and trailing missing observations.'
	]).

	observation(1, _).
	observation(2, 2).
	observation(3, _).
	observation(4, _).
	observation(5, 8).
	observation(6, 10).
	observation(7, _).

	series_length(7).

:- end_object.


:- object(missing_seasonal,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-27,
		comment is 'Two-position seasonal series with missing observations during initialization and fitting.'
	]).

	observation(1, _).
	observation(2, 20).
	observation(3, 10).
	observation(4, 20).
	observation(5, _).
	observation(6, 20).
	observation(7, 10).
	observation(8, _).
	observation(9, 10).

	series_length(9).

	frequency(2).

:- end_object.


:- object(missing_seasonal_phase,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-27,
		comment is 'Seasonal series whose initialization window has no known value for its first phase.'
	]).

	observation(1, _).
	observation(2, 20).
	observation(3, _).
	observation(4, 20).
	observation(5, 10).

	series_length(5).

	frequency(2).

:- end_object.


:- object(missing_transformed,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-27,
		comment is 'Positive series with an anonymous missing observation for transformed fitting.'
	]).

	observation(1, 2).
	observation(2, _).
	observation(3, 6).
	observation(4, 8).

	series_length(4).

:- end_object.


:- object(smoothing_series(_Values_),
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Parametric series for missing-observation validation and input-preservation tests.',
		parameters is ['Values' - 'Time-ordered numeric or unbound observations, or invalid values for rejection tests.']
	]).

	:- uses(list, [length/2, nth1/3]).

	observation(Index, Value) :-
		nth1(Index, _Values_, Value).

	series_length(Length) :-
		length(_Values_, Length).

:- end_object.
