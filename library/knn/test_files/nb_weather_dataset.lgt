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


:- object(nb_weather_dataset,
	implements(dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-15,
		comment is 'Weather dataset for Naive Bayes testing (categorical features).'
	]).

	attribute_values(outlook, [sunny, overcast, rainy]).
	attribute_values(temperature, [hot, mild, cool]).
	attribute_values(humidity, [high, normal]).
	attribute_values(wind, [weak, strong]).

	class(play).

	class_values([yes, no]).

	example( 1, no,  [outlook-sunny,    temperature-hot,  humidity-high,   wind-weak]).
	example( 2, no,  [outlook-sunny,    temperature-hot,  humidity-high,   wind-strong]).
	example( 3, yes, [outlook-overcast, temperature-hot,  humidity-high,   wind-weak]).
	example( 4, yes, [outlook-rainy,    temperature-mild, humidity-high,   wind-weak]).
	example( 5, yes, [outlook-rainy,    temperature-cool, humidity-normal, wind-weak]).
	example( 6, no,  [outlook-rainy,    temperature-cool, humidity-normal, wind-strong]).
	example( 7, yes, [outlook-overcast, temperature-cool, humidity-normal, wind-strong]).
	example( 8, no,  [outlook-sunny,    temperature-mild, humidity-high,   wind-weak]).
	example( 9, yes, [outlook-sunny,    temperature-cool, humidity-normal, wind-weak]).
	example(10, yes, [outlook-rainy,    temperature-mild, humidity-normal, wind-weak]).
	example(11, yes, [outlook-sunny,    temperature-mild, humidity-normal, wind-strong]).
	example(12, yes, [outlook-overcast, temperature-mild, humidity-high,   wind-strong]).
	example(13, yes, [outlook-overcast, temperature-hot,  humidity-normal, wind-weak]).
	example(14, no,  [outlook-rainy,    temperature-mild, humidity-high,   wind-strong]).

:- end_object.
