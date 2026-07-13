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


:- protocol(tle_orbits_protocol).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-13,
		comment is 'Protocol for parsing Two-Line Element sets and performing approximate portable orbit propagation with near-earth and deep-space variants plus ground-track sampling.',
		see_also is [tle_orbits, crs_projections_protocol]
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --list(compound)), one_or_error).
	:- info(parse/2, [
		comment is 'Parses one or more TLE records from a source specification into canonical ``tle(...)`` terms. Supported source specifications are ``atom(Atom)``, ``chars(List)``, ``codes(List)``, ``stream(Stream)``, and ``file(Path)``.',
		argnames is ['Source', 'TLEs'],
		exceptions is [
			'``Source`` is a variable or contains a variable source argument' - instantiation_error,
			'``Source`` is neither a variable nor a valid source specification' - domain_error(tle_source, 'Source'),
			'The parsed non-blank lines cannot be grouped into valid TLE records' - domain_error(tle_records, 'Lines')
		]
	]).

	:- public(parse_lines/4).
	:- mode(parse_lines(++nonvar, ++atom, ++atom, --compound), one_or_error).
	:- info(parse_lines/4, [
		comment is 'Parses a single TLE record given an optional name atom or the atom ``none`` plus the two raw TLE lines.',
		argnames is ['Name', 'Line1', 'Line2', 'TLE'],
		exceptions is [
			'``Name`` is a variable' - instantiation_error,
			'``Name`` is neither the atom ``none`` nor an atom name' - domain_error(tle_name, 'Name'),
			'``Line1`` is a variable' - instantiation_error,
			'``Line1`` is neither a variable nor an atom' - domain_error(tle_line, 'Line1'),
			'``Line2`` is a variable' - instantiation_error,
			'``Line2`` is neither a variable nor an atom' - domain_error(tle_line, 'Line2'),
			'The given name and lines do not form a valid TLE record because of invalid syntax, checksum, field encoding, or orbital element ranges' - domain_error(tle_lines, ['Name', 'Line1', 'Line2'])
		]
	]).

	:- public(propagate/3).
	:- mode(propagate(++compound, ++nonvar, --compound), one_or_error).
	:- info(propagate/3, [
		comment is 'Propagates a parsed TLE using the default ``approximate`` model and returns a ``geographic(Latitude,Longitude,Height)`` coordinate. Supported time specifications are ``date_time(Year,Month,Day,Hours,Minutes,Seconds)``, ``julian_date(JulianDate)``, and ``offset_seconds(SecondsSinceEpoch)``.',
		argnames is ['TLE', 'Time', 'Coordinate'],
		exceptions is [
			'``Time`` is not one of the supported time specifications with the required numeric components' - domain_error(tle_time, 'Time')
		]
	]).

	:- public(propagate/4).
	:- mode(propagate(++compound, ++nonvar, ++atom, --compound), one_or_error).
	:- info(propagate/4, [
		comment is 'Propagates a parsed TLE using the default ``approximate`` model and returns coordinates in the requested frame. Supported frames are ``eci`` returning ``eci(X,Y,Z)``, ``ecef`` returning ``ecef(X,Y,Z)``, and ``wgs84_3d`` returning ``geographic(Latitude,Longitude,Height)``.',
		argnames is ['TLE', 'Time', 'Frame', 'Coordinate'],
		exceptions is [
			'``Frame`` is not one of the supported reference frame identifiers' - domain_error(tle_reference_frame, 'Frame'),
			'``Time`` is not one of the supported time specifications with the required numeric components' - domain_error(tle_time, 'Time')
		]
	]).

	:- public(propagate/5).
	:- mode(propagate(++compound, ++nonvar, ++atom, ++atom, --compound), one_or_error).
	:- info(propagate/5, [
		comment is 'Propagates a parsed TLE using the requested propagation model and returns coordinates in the requested frame. Supported models are ``approximate`` for automatic near-earth versus deep-space dispatch, ``approximate_near_earth`` for the low-period approximate branch with J2 secular and short-period corrections plus low-order B* drag handling, ``approximate_deep_space`` for the dedicated deep-space approximate branch with resonance-aware long-period corrections, and ``two_body`` for the legacy Keplerian approximation.',
		argnames is ['TLE', 'Time', 'Frame', 'Model', 'Coordinate'],
		exceptions is [
			'``Frame`` is not one of the supported reference frame identifiers' - domain_error(tle_reference_frame, 'Frame'),
			'``Model`` is not one of the supported propagation model identifiers' - domain_error(tle_propagation_model, 'Model'),
			'``Time`` is not one of the supported time specifications with the required numeric components' - domain_error(tle_time, 'Time')
		]
	]).

	:- public(propagate_state/4).
	:- mode(propagate_state(++compound, ++nonvar, ++atom, --compound), one_or_error).
	:- info(propagate_state/4, [
		comment is 'Propagates a parsed TLE using the default ``approximate`` model and returns ``state(Position,Velocity)`` in the requested frame. Velocity is derived directly from the propagated orbital elements in ECI and analytically transformed to the requested frame. Supported frames are ``eci`` returning ``state(eci(X,Y,Z), eci(VX,VY,VZ))``, ``ecef`` returning ``state(ecef(X,Y,Z), ecef(VX,VY,VZ))``, and ``wgs84_3d`` returning ``state(geographic(Latitude,Longitude,Height), enu(East,North,Up))``.',
		argnames is ['TLE', 'Time', 'Frame', 'State'],
		exceptions is [
			'``Frame`` is not one of the supported reference frame identifiers' - domain_error(tle_reference_frame, 'Frame'),
			'``Time`` is not one of the supported time specifications with the required numeric components' - domain_error(tle_time, 'Time')
		]
	]).

	:- public(propagate_state/5).
	:- mode(propagate_state(++compound, ++nonvar, ++atom, ++atom, --compound), one_or_error).
	:- info(propagate_state/5, [
		comment is 'Propagates a parsed TLE using the requested propagation model and returns ``state(Position,Velocity)`` in the requested frame. Velocity is derived directly from the propagated orbital elements in ECI and analytically transformed to the requested frame. Supported models are ``approximate``, ``approximate_near_earth``, ``approximate_deep_space``, and ``two_body``.',
		argnames is ['TLE', 'Time', 'Frame', 'Model', 'State'],
		exceptions is [
			'``Frame`` is not one of the supported reference frame identifiers' - domain_error(tle_reference_frame, 'Frame'),
			'``Model`` is not one of the supported propagation model identifiers' - domain_error(tle_propagation_model, 'Model'),
			'``Time`` is not one of the supported time specifications with the required numeric components' - domain_error(tle_time, 'Time')
		]
	]).

	:- public(ground_track/5).
	:- mode(ground_track(++compound, ++compound, ++compound, ++number, --list(compound)), one_or_error).
	:- info(ground_track/5, [
		comment is 'Samples the propagated sub-satellite ground track between two UTC ``date_time/6`` instants separated by a positive step size in seconds using the default ``approximate`` model, returning ``sample(DateTime, geographic(Latitude,Longitude,Height))`` terms.',
		argnames is ['TLE', 'StartDateTime', 'EndDateTime', 'StepSeconds', 'Samples'],
		exceptions is [
			'``StartDateTime``, ``EndDateTime``, or ``StepSeconds`` do not define a valid ground-track sampling interval for the default model' - domain_error(tle_ground_track, ['StartDateTime', 'EndDateTime', 'StepSeconds', approximate])
		]
	]).

	:- public(ground_track/6).
	:- mode(ground_track(++compound, ++compound, ++compound, ++number, ++atom, --list(compound)), one_or_error).
	:- info(ground_track/6, [
		comment is 'Samples the propagated sub-satellite ground track between two UTC ``date_time/6`` instants separated by a positive step size in seconds using the requested propagation model. Supported models are ``approximate``, ``approximate_near_earth``, ``approximate_deep_space``, and ``two_body``.',
		argnames is ['TLE', 'StartDateTime', 'EndDateTime', 'StepSeconds', 'Model', 'Samples'],
		exceptions is [
			'``Model`` is not one of the supported propagation model identifiers' - domain_error(tle_propagation_model, 'Model'),
			'``StartDateTime``, ``EndDateTime``, or ``StepSeconds`` do not define a valid ground-track sampling interval for the requested model' - domain_error(tle_ground_track, ['StartDateTime', 'EndDateTime', 'StepSeconds', 'Model'])
		]
	]).

:- end_protocol.
