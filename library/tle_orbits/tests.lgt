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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-11,
		comment is 'Unit tests for the "tle_orbits" library.'
	]).

	:- uses(tle_orbits, [
		parse/2, parse_lines/4, propagate/3, propagate/4, propagate/5, propagate_state/4, propagate_state/5, ground_track/5, ground_track/6
	]).

	cover(tle_orbits).

	test(tle_orbits_parse_lines_01, deterministic((
		TLE = tle('ISS (ZARYA)', satellite(25544, u, designator(1998, 67, 'A')), epoch_julian_date(EpochJulianDate), drag(0.00012051, MeanMotionDdot, BStar), orbit(51.6393, 184.4452, 0.0003580, 32.9443, 327.1663, 15.50957687), ephemeris_type(0), element_set(999), revolution(45212)),
		EpochJulianDate > 2460429.0,
		EpochJulianDate < 2460431.0,
		abs(MeanMotionDdot) < 1.0e-12,
		abs(BStar - 2.1940e-4) < 1.0e-8
	))) :-
		parse_lines('ISS (ZARYA)', '1 25544U 98067A   24120.51782528  .00012051  00000-0  21940-3 0  9995', '2 25544  51.6393 184.4452 0003580  32.9443 327.1663 15.50957687452123', TLE).

	test(tle_orbits_parse_atom_named_and_unnamed_01, deterministic) :-
		parse(atom('ISS (ZARYA)\n1 25544U 98067A   24120.51782528  .00012051  00000-0  21940-3 0  9995\n2 25544  51.6393 184.4452 0003580  32.9443 327.1663 15.50957687452123\n1 25338U 98030A   24120.51285227  .00000087  00000-0  76459-4 0  9992\n2 25338  98.5482 146.2457 0011075 280.3899  79.6112 14.25901712352098\n'), [tle('ISS (ZARYA)', _, _, _, _, _, _, _), tle(none, satellite(25338, _, _), _, _, _, _, _, _)]).

	test(tle_orbits_parse_file_01, deterministic(Length == 2)) :-
		^^file_path('test_files/sample.tle', Path),
		parse(file(Path), TLEs),
		length(TLEs, Length).

	test(tle_orbits_parse_invalid_checksum_01, error(domain_error(tle_lines, _))) :-
		parse_lines('ISS (ZARYA)', '1 25544U 98067A   24120.51782528  .00012051  00000-0  21940-3 0  9999', '2 25544  51.6393 184.4452 0003580  32.9443 327.1663 15.50957687452123', _).

	test(tle_orbits_propagate_geographic_01, deterministic((
		Coordinate = geographic(Latitude, Longitude, Height),
		Latitude >= -90.0,
		Latitude =< 90.0,
		Longitude >= -180.0,
		Longitude =< 180.0,
		Height > 300000.0,
		Height < 500000.0
	))) :-
		iss_tle(TLE),
		propagate(TLE, offset_seconds(0.0), Coordinate).

	test(tle_orbits_propagate_eci_01, deterministic((
		Coordinate = eci(X, Y, Z),
		Radius is sqrt(X*X + Y*Y + Z*Z),
		Radius > 6500000.0,
		Radius < 7100000.0
	))) :-
		iss_tle(TLE),
		propagate(TLE, offset_seconds(0.0), eci, Coordinate).

	test(tle_orbits_propagate_ecef_01, deterministic((
		Coordinate = ecef(X, Y, Z),
		Radius is sqrt(X*X + Y*Y + Z*Z),
		Radius > 6500000.0,
		Radius < 7100000.0
	))) :-
		iss_tle(TLE),
		propagate(TLE, offset_seconds(0.0), ecef, Coordinate).

	test(tle_orbits_propagate_explicit_default_model_01, deterministic(Distance < 1.0e-3)) :-
		iss_tle(TLE),
		propagate(TLE, offset_seconds(3600.0), eci, eci(X0, Y0, Z0)),
		propagate(TLE, offset_seconds(3600.0), eci, approximate, eci(X1, Y1, Z1)),
		DX is X0 - X1,
		DY is Y0 - Y1,
		DZ is Z0 - Z1,
		Distance is sqrt(DX*DX + DY*DY + DZ*DZ).

	test(tle_orbits_propagate_auto_dispatch_matches_near_earth_01, deterministic(Distance < 1.0e-3)) :-
		iss_tle(TLE),
		propagate(TLE, offset_seconds(5400.0), eci, approximate, eci(X0, Y0, Z0)),
		propagate(TLE, offset_seconds(5400.0), eci, approximate_near_earth, eci(X1, Y1, Z1)),
		DX is X0 - X1,
		DY is Y0 - Y1,
		DZ is Z0 - Z1,
		Distance is sqrt(DX*DX + DY*DY + DZ*DZ).

	test(tle_orbits_propagate_models_diverge_off_epoch_01, deterministic((
		Distance > 1000.0,
		Radius > 6500000.0,
		Radius < 7100000.0
	))) :-
		iss_tle(TLE),
		propagate(TLE, offset_seconds(86400.0), eci, two_body, eci(X0, Y0, Z0)),
		propagate(TLE, offset_seconds(86400.0), eci, approximate_near_earth, eci(X1, Y1, Z1)),
		DX is X0 - X1,
		DY is Y0 - Y1,
		DZ is Z0 - Z1,
		Distance is sqrt(DX*DX + DY*DY + DZ*DZ),
		Radius is sqrt(X1*X1 + Y1*Y1 + Z1*Z1).

	test(tle_orbits_propagate_julian_date_matches_epoch_offset_01, deterministic(Distance < 1.0e-3)) :-
		iss_tle(TLE),
		TLE = tle(_, _, epoch_julian_date(EpochJulianDate), _, _, _, _, _),
		propagate(TLE, offset_seconds(0.0), eci, eci(X0, Y0, Z0)),
		propagate(TLE, julian_date(EpochJulianDate), eci, eci(X1, Y1, Z1)),
		DX is X0 - X1,
		DY is Y0 - Y1,
		DZ is Z0 - Z1,
		Distance is sqrt(DX*DX + DY*DY + DZ*DZ).

	test(tle_orbits_ground_track_01, deterministic) :-
		iss_tle(TLE),
		TLE = tle(_, _, epoch_julian_date(EpochJulianDate), _, _, _, _, _),
		propagate(TLE, julian_date(EpochJulianDate), wgs84_3d, _),
		start_and_end_datetimes(EpochJulianDate, StartDateTime, EndDateTime),
		ground_track(TLE, StartDateTime, EndDateTime, 600.0, Samples),
		Samples = [sample(date_time(_, _, _, _, _, _), geographic(_, _, _)), sample(_, geographic(_, _, _)), sample(_, geographic(_, _, _)), sample(_, geographic(_, _, _))].

	test(tle_orbits_ground_track_two_body_model_01, deterministic(Length == 4)) :-
		iss_tle(TLE),
		TLE = tle(_, _, epoch_julian_date(EpochJulianDate), _, _, _, _, _),
		start_and_end_datetimes(EpochJulianDate, StartDateTime, EndDateTime),
		ground_track(TLE, StartDateTime, EndDateTime, 600.0, two_body, Samples),
		length(Samples, Length).

	test(tle_orbits_propagate_auto_dispatch_matches_deep_space_01, deterministic(Distance < 1.0e-3)) :-
		deep_space_tle(TLE),
		propagate(TLE, offset_seconds(43200.0), eci, approximate, eci(X0, Y0, Z0)),
		propagate(TLE, offset_seconds(43200.0), eci, approximate_deep_space, eci(X1, Y1, Z1)),
		DX is X0 - X1,
		DY is Y0 - Y1,
		DZ is Z0 - Z1,
		Distance is sqrt(DX*DX + DY*DY + DZ*DZ).

	test(tle_orbits_propagate_deep_space_branch_diverges_from_near_earth_01, deterministic((
		Distance > 1000.0,
		Radius > 40000000.0,
		Radius < 45000000.0
	))) :-
		deep_space_tle(TLE),
		propagate(TLE, offset_seconds(172800.0), eci, approximate_near_earth, eci(X0, Y0, Z0)),
		propagate(TLE, offset_seconds(172800.0), eci, approximate_deep_space, eci(X1, Y1, Z1)),
		DX is X0 - X1,
		DY is Y0 - Y1,
		DZ is Z0 - Z1,
		Distance is sqrt(DX*DX + DY*DY + DZ*DZ),
		Radius is sqrt(X1*X1 + Y1*Y1 + Z1*Z1).

	test(tle_orbits_propagate_state_explicit_default_model_01, deterministic((
		PositionDelta < 1.0e-3,
		VelocityDelta < 1.0e-6
	))) :-
		iss_tle(TLE),
		propagate_state(TLE, offset_seconds(1800.0), eci, state(eci(X0, Y0, Z0), eci(VX0, VY0, VZ0))),
		propagate_state(TLE, offset_seconds(1800.0), eci, approximate, state(eci(X1, Y1, Z1), eci(VX1, VY1, VZ1))),
		DX is X0 - X1,
		DY is Y0 - Y1,
		DZ is Z0 - Z1,
		PositionDelta is sqrt(DX*DX + DY*DY + DZ*DZ),
		DVX is VX0 - VX1,
		DVY is VY0 - VY1,
		DVZ is VZ0 - VZ1,
		VelocityDelta is sqrt(DVX*DVX + DVY*DVY + DVZ*DVZ).

	test(tle_orbits_propagate_state_eci_01, deterministic((
		Speed > 7000.0,
		Speed < 8000.0
	))) :-
		iss_tle(TLE),
		propagate_state(TLE, offset_seconds(0.0), eci, state(eci(_, _, _), eci(VX, VY, VZ))),
		Speed is sqrt(VX*VX + VY*VY + VZ*VZ).

	test(tle_orbits_propagate_state_ecef_01, deterministic((
		Speed > 6500.0,
		Speed < 8000.0
	))) :-
		iss_tle(TLE),
		propagate_state(TLE, offset_seconds(0.0), ecef, state(ecef(_, _, _), ecef(VX, VY, VZ))),
		Speed is sqrt(VX*VX + VY*VY + VZ*VZ).

	test(tle_orbits_propagate_state_wgs84_3d_01, deterministic((
		Speed > 7000.0,
		Speed < 8000.0
	))) :-
		iss_tle(TLE),
		propagate_state(TLE, offset_seconds(1200.0), wgs84_3d, state(geographic(_, _, _), enu(East, North, Up))),
		Speed is sqrt(East*East + North*North + Up*Up).

	test(tle_orbits_propagate_state_wgs84_3d_matches_ecef_speed_01, deterministic(Delta < 1.0e-6)) :-
		iss_tle(TLE),
		propagate_state(TLE, offset_seconds(1200.0), ecef, state(ecef(_, _, _), ecef(EcefVX, EcefVY, EcefVZ))),
		propagate_state(TLE, offset_seconds(1200.0), wgs84_3d, state(geographic(_, _, _), enu(East, North, Up))),
		EcefSpeed is sqrt(EcefVX*EcefVX + EcefVY*EcefVY + EcefVZ*EcefVZ),
		EnuSpeed is sqrt(East*East + North*North + Up*Up),
		Delta is abs(EcefSpeed - EnuSpeed).

	test(tle_orbits_propagate_state_deep_space_01, deterministic((
		Speed > 2500.0,
		Speed < 3500.0
	))) :-
		deep_space_tle(TLE),
		propagate_state(TLE, offset_seconds(0.0), eci, approximate_deep_space, state(eci(_, _, _), eci(VX, VY, VZ))),
		Speed is sqrt(VX*VX + VY*VY + VZ*VZ).

	% auxiliary predicates

	iss_tle(TLE) :-
		parse_lines('ISS (ZARYA)', '1 25544U 98067A   24120.51782528  .00012051  00000-0  21940-3 0  9995', '2 25544  51.6393 184.4452 0003580  32.9443 327.1663 15.50957687452123', TLE).

	deep_space_tle(
		tle('GOES 16', satellite(41866, u, designator(2016, 71, 'A')), epoch_julian_date(2460430.0), drag(0.0, 0.0, 0.000003), orbit(0.056, 89.582, 0.000115, 268.470, 203.951, 1.002700), ephemeris_type(0), element_set(999), revolution(2718))
	).

	start_and_end_datetimes(EpochJulianDate, StartDateTime, EndDateTime) :-
		StartDateTime = date_time(2024, 4, 29, 12, 25, 40.104192197322845),
		EndDateTime = date_time(2024, 4, 29, 12, 55, 40.104192197322845),
		EpochJulianDate > 2460429.0,
		EpochJulianDate < 2460431.0.

:- end_object.
