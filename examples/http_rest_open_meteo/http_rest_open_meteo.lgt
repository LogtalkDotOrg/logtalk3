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


:- object(open_meteo_rest_client).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'REST client example for the Open-Meteo geocoding and forecast APIs.'
	]).

	:- public(forecast/2).
	:- mode(forecast(++atom, -compound), one_or_error).
	:- info(forecast/2, [
		comment is 'Resolves a location name using the Open-Meteo geocoding API and then fetches a small weather forecast for the best match.',
		argnames is ['Location', 'Forecast']
	]).

	:- protected(http_get/2).
	:- mode(http_get(+atom, --compound), one_or_error).
	:- info(http_get/2, [
		comment is 'Hook predicate used to perform HTTP GET requests. Tests can override it to return canned responses.',
		argnames is ['URL', 'Response']
	]).

	:- uses(json_pointer, [
		evaluate/3
	]).

	forecast(Location, forecast(location(Name, Country, Latitude, Longitude, Timezone), ForecastJSON)) :-
		geocoding_url(Location, GeocodingURL),
		::http_get(GeocodingURL, GeocodingResponse),
		response_json(GeocodingResponse, GeocodingJSON),
		geocoding_result(GeocodingJSON, Location, Name, Country, Latitude, Longitude, Timezone),
		forecast_url(Latitude, Longitude, ForecastURL),
		::http_get(ForecastURL, ForecastResponse),
		response_json(ForecastResponse, ForecastJSON).

	http_get(URL, Response) :-
		http_client::get(URL, Response, []).

	geocoding_url(Location, URL) :-
		atom_concat('name=', Location, Prefix),
		atom_concat(Prefix, '&count=1&language=en&format=json', Query),
		url(atom)::generate([
			scheme(http),
			authority('geocoding-api.open-meteo.com'),
			path('/v1/search'),
			query(Query)
		], URL).

	forecast_url(Latitude, Longitude, URL) :-
		number_atom(Latitude, LatitudeAtom),
		number_atom(Longitude, LongitudeAtom),
		atom_concat('latitude=', LatitudeAtom, Prefix0),
		atom_concat(Prefix0, '&longitude=', Prefix1),
		atom_concat(Prefix1, LongitudeAtom, Prefix2),
		atom_concat(Prefix2, '&current=temperature_2m,weather_code', Prefix3),
		atom_concat(Prefix3, '&daily=weather_code,temperature_2m_max,temperature_2m_min', Prefix4),
		atom_concat(Prefix4, '&timezone=auto&forecast_days=3', Query),
		url(atom)::generate([
			scheme(http),
			authority('api.open-meteo.com'),
			path('/v1/forecast'),
			query(Query)
		], URL).

	response_json(Response, JSON) :-
		( 	http_core::status(Response, status(200, 'OK')),
			http_core::body(Response, content('application/json', json(JSON))) ->
			check_api_error(JSON)
		; 	throw_forecast_error(unexpected_open_meteo_response(Response))
		).

	check_api_error(JSON) :-
		( 	evaluate([error], JSON, @true) ->
			evaluate([reason], JSON, Reason),
			throw_forecast_error(open_meteo_api_error(Reason))
		; 	true
		).

	geocoding_result(JSON, Location, Name, Country, Latitude, Longitude, Timezone) :-
		( 	evaluate([results], JSON, [Result| _Results]) ->
			geocoding_value(name, Result, Name),
			geocoding_value(country, Result, Country),
			geocoding_value(latitude, Result, Latitude),
			geocoding_value(longitude, Result, Longitude),
			geocoding_value(timezone, Result, Timezone)
		; 	evaluate([results], JSON, []) ->
			throw_forecast_error(existence_error(location, Location))
		; 	throw_forecast_error(unexpected_open_meteo_geocoding_response(JSON))
		).

	geocoding_value(Key, Result, Value) :-
		( 	evaluate([Key], Result, Value) ->
			true
		; 	throw_forecast_error(unexpected_open_meteo_geocoding_result(Result))
		).

	number_atom(Number, Atom) :-
		number_codes(Number, Codes),
		atom_codes(Atom, Codes).

	throw_forecast_error(Error) :-
		self(Self),
		throw(error(Error, Self::forecast/2)).

:- end_object.
