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



:- object(mock_open_meteo_rest_client,
	extends(open_meteo_rest_client)).

	http_get('http://geocoding-api.open-meteo.com/v1/search?name=New%20York&count=1&language=en&format=json', Response) :-
		http::response(
			http(1, 1),
			status(200, 'OK'),
			[],
			content('application/json', json({
				results-[{
					id-5128581,
					name-'New York',
					latitude-40.71427,
					longitude- -74.00597,
					country-'United States',
					timezone-'America/New_York'
				}]
			})),
			[],
			Response
		).
	http_get('http://api.open-meteo.com/v1/forecast?latitude=40.71427&longitude=-74.00597&current=temperature_2m,weather_code&daily=weather_code,temperature_2m_max,temperature_2m_min&timezone=auto&forecast_days=3', Response) :-
		http::response(
			http(1, 1),
			status(200, 'OK'),
			[],
			content('application/json', json({
				latitude-40.71427,
				longitude- -74.00597,
				timezone-'America/New_York',
				current-{
					time-'2026-05-27T12:00',
					temperature_2m-18.4,
					weather_code-3
				},
				daily-{
					time-['2026-05-27', '2026-05-28', '2026-05-29'],
					weather_code-[3, 61, 2],
					temperature_2m_max-[23.1, 21.4, 24.0],
					temperature_2m_min-[15.0, 14.2, 16.1]
				}
			})),
			[],
			Response
		).

:- end_object.


:- object(empty_results_open_meteo_rest_client,
	extends(open_meteo_rest_client)).

	http_get('http://geocoding-api.open-meteo.com/v1/search?name=atlantis&count=1&language=en&format=json', Response) :-
		http::response(
			http(1, 1),
			status(200, 'OK'),
			[],
			content('application/json', json({results-[]})),
			[],
			Response
		).

:- end_object.


:- object(api_error_open_meteo_rest_client,
	extends(open_meteo_rest_client)).

	http_get('http://geocoding-api.open-meteo.com/v1/search?name=porto&count=1&language=en&format=json', Response) :-
		http::response(
			http(1, 1),
			status(200, 'OK'),
			[],
			content('application/json', json({error- @true, reason-'Too many requests.'})),
			[],
			Response
		).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-27,
		comment is 'Unit tests for the "http_rest_open_meteo" example.'
	]).

	cover(open_meteo_rest_client).

	test(http_rest_open_meteo_01, deterministic(Forecast == forecast(location('New York', 'United States', 40.71427, -74.00597, 'America/New_York'), {latitude-40.71427, longitude- -74.00597, timezone-'America/New_York', current-{time-'2026-05-27T12:00', temperature_2m-18.4, weather_code-3}, daily-{time-['2026-05-27', '2026-05-28', '2026-05-29'], weather_code-[3, 61, 2], temperature_2m_max-[23.1, 21.4, 24.0], temperature_2m_min-[15.0, 14.2, 16.1]}}))) :-
		mock_open_meteo_rest_client::forecast('New York', Forecast).

	test(http_rest_open_meteo_02, error(error(existence_error(location, atlantis), empty_results_open_meteo_rest_client::forecast/2))) :-
		empty_results_open_meteo_rest_client::forecast(atlantis, _Forecast).

	test(http_rest_open_meteo_03, error(error(open_meteo_api_error('Too many requests.'), api_error_open_meteo_rest_client::forecast/2))) :-
		api_error_open_meteo_rest_client::forecast(porto, _Forecast).

:- end_object.
