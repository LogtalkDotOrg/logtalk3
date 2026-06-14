%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(dates_tz_protocol).

	:- info([
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2026-06-14,
		comment is 'Protocol for zone-aware date-time conversion using cached TZif data.',
		see_also is [dates_tz]
	]).

	:- public(utc_to_local_tz/3).
	:- mode(utc_to_local_tz(+compound, +atom, -compound), one_or_error).
	:- info(utc_to_local_tz/3, [
		comment is 'Converts a UTC date-time to the civil local date-time in the named zone. Requires the zone to be present in the cached TZif data.',
		argnames is ['UTCDateTime', 'Zone', 'LocalDateTime'],
		exceptions is [
			'``Zone`` is not present in the cached TZif data' - existence_error(time_zone, 'Zone')
		]
	]).

	:- public(local_to_utc_tz/3).
	:- mode(local_to_utc_tz(+compound, +atom, -compound), zero_or_one_or_error).
	:- info(local_to_utc_tz/3, [
		comment is 'Converts a civil local date-time in the named zone to UTC. This strict variant fails silently if the local time falls in a DST gap (non-existent time) or a DST fold (ambiguous time). Requires the zone to be present in the cached TZif data.',
		argnames is ['LocalDateTime', 'Zone', 'UTCDateTime'],
		exceptions is [
			'``LocalDateTime`` is a variable' - instantiation_error,
			'``LocalDateTime`` is neither a variable nor a valid ``date_time/6`` term' - type_error(date_time, 'LocalDateTime'),
			'``Zone`` is not present in the cached TZif data' - existence_error(time_zone, 'Zone')
		]
	]).

	:- public(local_to_utc_tz_with_resolution/4).
	:- mode(local_to_utc_tz_with_resolution(+compound, +atom, +atom, -compound), zero_or_more).
	:- info(local_to_utc_tz_with_resolution/4, [
		comment is 'Converts a civil local date-time in the named zone to UTC using an explicit resolution mode for ambiguous or non-existent times. The resolution mode can be ``strict`` (fail unless exactly one interpretation), ``first`` (prefer the earliest valid interpretation), ``second`` (prefer the latest valid interpretation), or ``all`` (enumerate all valid interpretations). Requires the zone to be present in the cached TZif data.',
		argnames is ['LocalDateTime', 'Zone', 'ResolutionMode', 'UTCDateTime']
	]).

	:- public(convert_zones/4).
	:- mode(convert_zones(+compound, +atom, +atom, -compound), zero_or_one_or_error).
	:- info(convert_zones/4, [
		comment is 'Converts a civil local date-time in one zone to the civil local date-time in another zone. Uses strict interpretation: fails if the source local time is in a DST gap or fold. Requires both zones to be present in the cached TZif data.',
		argnames is ['LocalDateTime', 'FromZone', 'ToZone', 'ResultDateTime'],
		exceptions is [
			'``LocalDateTime`` is a variable' - instantiation_error,
			'``LocalDateTime`` is neither a variable nor a valid ``date_time/6`` term' - type_error(date_time, 'LocalDateTime'),
			'``FromZone`` is not present in the cached TZif data' - existence_error(time_zone, 'FromZone'),
			'``ToZone`` is not present in the cached TZif data' - existence_error(time_zone, 'ToZone')
		]
	]).

	:- public(convert_zones_with_resolution/5).
	:- mode(convert_zones_with_resolution(+compound, +atom, +atom, +atom, -compound), zero_or_more).
	:- info(convert_zones_with_resolution/5, [
		comment is 'Converts a civil local date-time in one zone to the civil local date-time in another zone using an explicit resolution mode for the source zone. The resolution mode is applied when the source local time is ambiguous or non-existent. Requires both zones to be present in the cached TZif data.',
		argnames is ['LocalDateTime', 'FromZone', 'ResolutionMode', 'ToZone', 'ResultDateTime']
	]).

:- end_protocol.
