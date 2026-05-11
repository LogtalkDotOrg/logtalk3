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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-11,
		comment is 'Protocol for parsing Two-Line Element sets and performing approximate portable orbit propagation with near-earth and deep-space variants plus ground-track sampling.',
		see_also is [tle_orbits, crs_projections_protocol]
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --list(compound)), one_or_error).
	:- info(parse/2, [
		comment is 'Parses one or more TLE records from a source specification into canonical ``tle(...)`` terms. Supported source specifications are ``atom(Atom)``, ``chars(List)``, ``codes(List)``, ``stream(Stream)``, and ``file(Path)``.',
		argnames is ['Source', 'TLEs']
	]).

	:- public(parse_lines/4).
	:- mode(parse_lines(++nonvar, ++atom, ++atom, --compound), one_or_error).
	:- info(parse_lines/4, [
		comment is 'Parses a single TLE record given an optional name atom or the atom ``none`` plus the two raw TLE lines.',
		argnames is ['Name', 'Line1', 'Line2', 'TLE']
	]).

	:- public(propagate/3).
	:- mode(propagate(++compound, ++nonvar, --compound), one_or_error).
	:- info(propagate/3, [
		comment is 'Propagates a parsed TLE using the default ``approximate`` model and returns a ``geographic(Latitude,Longitude,Height)`` coordinate. Supported time specifications are ``date_time(Year,Month,Day,Hours,Minutes,Seconds)``, ``julian_date(JulianDate)``, and ``offset_seconds(SecondsSinceEpoch)``.',
		argnames is ['TLE', 'Time', 'Coordinate']
	]).

	:- public(propagate/4).
	:- mode(propagate(++compound, ++nonvar, ++atom, --compound), one_or_error).
	:- info(propagate/4, [
		comment is 'Propagates a parsed TLE using the default ``approximate`` model and returns coordinates in the requested frame. Supported frames are ``eci`` returning ``eci(X,Y,Z)``, ``ecef`` returning ``ecef(X,Y,Z)``, and ``wgs84_3d`` returning ``geographic(Latitude,Longitude,Height)``.',
		argnames is ['TLE', 'Time', 'Frame', 'Coordinate']
	]).

	:- public(propagate/5).
	:- mode(propagate(++compound, ++nonvar, ++atom, ++atom, --compound), one_or_error).
	:- info(propagate/5, [
		comment is 'Propagates a parsed TLE using the requested propagation model and returns coordinates in the requested frame. Supported models are ``approximate`` for automatic near-earth versus deep-space dispatch, ``approximate_near_earth`` for the low-period approximate branch with J2 secular and short-period corrections plus low-order B* drag handling, ``approximate_deep_space`` for the dedicated deep-space approximate branch with resonance-aware long-period corrections, and ``two_body`` for the legacy Keplerian approximation.',
		argnames is ['TLE', 'Time', 'Frame', 'Model', 'Coordinate']
	]).

	:- public(propagate_state/4).
	:- mode(propagate_state(++compound, ++nonvar, ++atom, --compound), one_or_error).
	:- info(propagate_state/4, [
		comment is 'Propagates a parsed TLE using the default ``approximate`` model and returns ``state(Position,Velocity)`` in the requested frame. Velocity is derived directly from the propagated orbital elements in ECI and analytically transformed to the requested frame. Supported frames are ``eci`` returning ``state(eci(X,Y,Z), eci(VX,VY,VZ))``, ``ecef`` returning ``state(ecef(X,Y,Z), ecef(VX,VY,VZ))``, and ``wgs84_3d`` returning ``state(geographic(Latitude,Longitude,Height), enu(East,North,Up))``.',
		argnames is ['TLE', 'Time', 'Frame', 'State']
	]).

	:- public(propagate_state/5).
	:- mode(propagate_state(++compound, ++nonvar, ++atom, ++atom, --compound), one_or_error).
	:- info(propagate_state/5, [
		comment is 'Propagates a parsed TLE using the requested propagation model and returns ``state(Position,Velocity)`` in the requested frame. Velocity is derived directly from the propagated orbital elements in ECI and analytically transformed to the requested frame. Supported models are ``approximate``, ``approximate_near_earth``, ``approximate_deep_space``, and ``two_body``.',
		argnames is ['TLE', 'Time', 'Frame', 'Model', 'State']
	]).

	:- public(ground_track/5).
	:- mode(ground_track(++compound, ++compound, ++compound, ++number, --list(compound)), one_or_error).
	:- info(ground_track/5, [
		comment is 'Samples the propagated sub-satellite ground track between two UTC ``date_time/6`` instants separated by a positive step size in seconds using the default ``approximate`` model, returning ``sample(DateTime, geographic(Latitude,Longitude,Height))`` terms.',
		argnames is ['TLE', 'StartDateTime', 'EndDateTime', 'StepSeconds', 'Samples']
	]).

	:- public(ground_track/6).
	:- mode(ground_track(++compound, ++compound, ++compound, ++number, ++atom, --list(compound)), one_or_error).
	:- info(ground_track/6, [
		comment is 'Samples the propagated sub-satellite ground track between two UTC ``date_time/6`` instants separated by a positive step size in seconds using the requested propagation model. Supported models are ``approximate``, ``approximate_near_earth``, ``approximate_deep_space``, and ``two_body``.',
		argnames is ['TLE', 'StartDateTime', 'EndDateTime', 'StepSeconds', 'Model', 'Samples']
	]).

:- end_protocol.
