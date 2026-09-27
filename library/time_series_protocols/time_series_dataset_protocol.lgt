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


:- protocol(time_series_dataset_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-27,
		comment is 'Protocol for datasets used with time series forecasting algorithms.'
	]).

	:- public(observation/2).
	:- mode(observation(-integer, -number), zero_or_more).
	:- info(observation/2, [
		comment is 'Enumerates by backtracking the time-ordered observations. ``Index`` is the 1-based position of the observation in the series and ``Value`` is its numeric value.',
		argnames is ['Index', 'Value']
	]).

	:- public(series_length/1).
	:- mode(series_length(-positive_integer), one).
	:- info(series_length/1, [
		comment is 'Returns the number of observations in the series. The declared length must match the number of observations enumerated by ``observation/2``.',
		argnames is ['Length']
	]).

	:- public(frequency/1).
	:- mode(frequency(-positive_integer), zero_or_one).
	:- info(frequency/1, [
		comment is 'Returns the positive number of observations per seasonal cycle (e.g. 12 for monthly data with yearly seasonality). Fails when the series has no declared seasonal period.',
		argnames is ['Frequency']
	]).

:- end_protocol.
