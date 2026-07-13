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


:- object(tle_orbits,
	implements(tle_orbits_protocol)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2026-07-13,
		comment is 'Parser for Two-Line Element sets with an approximate portable propagator featuring automatic branching between near-earth and deep-space variants, a legacy two-body fallback model, and ground-track sampling.'
	]).

	:- uses(crs_projections, [
		transform/4
	]).

	:- uses(date, [
		day_of_year_date/3, valid/3 as valid_date/3
	]).

	:- uses(iso8601, [
		date/4
	]).

	:- uses(list, [
		reverse/2, take/3, drop/3
	]).

	:- uses(reader, [
		file_to_codes/2, stream_to_codes/2
	]).

	:- uses(time, [
		valid/3 as valid_time/3
	]).

	parse(Source, TLEs) :-
		source_codes(Source, Codes),
		phrase(lines(RawLines), Codes),
		keep_non_blank_lines(RawLines, Lines),
		lines_to_tles(Lines, TLEs).

	parse_lines(Name, Line1, Line2, TLE) :-
		name_codes(Name, NameCodes),
		line_input_codes(Line1, Line1Codes),
		line_input_codes(Line2, Line2Codes),
		parse_lines_codes(NameCodes, Line1Codes, Line2Codes, TLE),
		!.
	parse_lines(Name, Line1, Line2, _) :-
		domain_error(tle_lines, [Name, Line1, Line2]).

	propagate(TLE, Time, Coordinate) :-
		default_propagation_model(Model),
		propagate(TLE, Time, wgs84_3d, Model, Coordinate).

	propagate(TLE, Time, Frame, Coordinate) :-
		default_propagation_model(Model),
		propagate(TLE, Time, Frame, Model, Coordinate).

	propagate(TLE, Time, Frame, Model, Coordinate) :-
		(	valid_frame(Frame) ->
			true
		;	domain_error(tle_reference_frame, Frame)
		),
		(	valid_model(Model) ->
			true
		;	domain_error(tle_propagation_model, Model)
		),
		time_spec_julian_date(TLE, Time, JulianDate),
		propagate_julian_date(TLE, JulianDate, Model, ECI, ECEF, Geographic),
		coordinate_for_frame(Frame, ECI, ECEF, Geographic, Coordinate).

	propagate_state(TLE, Time, Frame, State) :-
		default_propagation_model(Model),
		propagate_state(TLE, Time, Frame, Model, State).

	propagate_state(TLE, Time, Frame, Model, State) :-
		(	valid_frame(Frame) ->
			true
		;	domain_error(tle_reference_frame, Frame)
		),
		(	valid_model(Model) ->
			true
		;	domain_error(tle_propagation_model, Model)
		),
		time_spec_julian_date(TLE, Time, JulianDate),
		propagate_state_julian_date(Frame, TLE, JulianDate, Model, State).

	ground_track(TLE, StartDateTime, EndDateTime, StepSeconds, Samples) :-
		default_propagation_model(Model),
		ground_track(TLE, StartDateTime, EndDateTime, StepSeconds, Model, Samples).

	ground_track(TLE, StartDateTime, EndDateTime, StepSeconds, Model, Samples) :-
		valid_date_time(StartDateTime),
		valid_date_time(EndDateTime),
		number(StepSeconds),
		StepSeconds > 0.0,
		(	valid_model(Model) ->
			true
		;	domain_error(tle_propagation_model, Model)
		),
		date_time_julian_date(StartDateTime, StartJulianDate),
		date_time_julian_date(EndDateTime, EndJulianDate),
		StartJulianDate =< EndJulianDate,
		StepDays is StepSeconds / 86400.0,
		ground_track_samples(StartJulianDate, EndJulianDate, StepDays, Model, TLE, Samples),
		!.
	ground_track(_, StartDateTime, EndDateTime, StepSeconds, Model, _) :-
		domain_error(tle_ground_track, [StartDateTime, EndDateTime, StepSeconds, Model]).

	source_codes(Source, _) :-
		var(Source),
		!,
		instantiation_error.
	source_codes(file(File), Codes) :-
		(	var(File) ->
			instantiation_error
		;	file_to_codes(File, Codes)
		),
		!.
	source_codes(stream(Stream), Codes) :-
		(	var(Stream) ->
			instantiation_error
		;	stream_to_codes(Stream, Codes)
		),
		!.
	source_codes(atom(Atom), Codes) :-
		(	var(Atom) ->
			instantiation_error
		;	atom_codes(Atom, Codes)
		),
		!.
	source_codes(chars(Chars), Codes) :-
		(	var(Chars) ->
			instantiation_error
		;	chars_to_codes(Chars, Codes)
		),
		!.
	source_codes(codes(Codes), Codes) :-
		(	var(Codes) ->
			instantiation_error
		;	true
		),
		!.
	source_codes(Source, _) :-
		domain_error(tle_source, Source).

	name_codes(Name, _) :-
		var(Name),
		instantiation_error.
	name_codes(none, []) :-
		!.
	name_codes(Name, Codes) :-
		atom(Name),
		!,
		atom_codes(Name, Codes).
	name_codes(Name, _) :-
		domain_error(tle_name, Name).

	line_input_codes(Line, _) :-
		var(Line),
		instantiation_error.
	line_input_codes(Line, Codes) :-
		atom(Line),
		!,
		atom_codes(Line, Codes).
	line_input_codes(Line, _) :-
		domain_error(tle_line, Line).

	lines([Line| Lines]) -->
		line(Line),
		line_end,
		!,
		lines(Lines).
	lines([]) -->
		eos,
		!.
	lines([Line]) -->
		line(Line),
		eos,
		!.

	line([Code| Codes]) -->
		[Code],
		{Code =\= 10, Code =\= 13},
		!,
		line_rest(Codes).
	line([]) -->
		[].

	line_rest([Code| Codes]) -->
		[Code],
		{Code =\= 10, Code =\= 13},
		!,
		line_rest(Codes).
	line_rest([]) -->
		[].

	line_end -->
		[13, 10],
		!.
	line_end -->
		[10],
		!.
	line_end -->
		[13].

	keep_non_blank_lines([], []).
	keep_non_blank_lines([Line| Lines], KeptLines) :-
		(	blank_line(Line) ->
			keep_non_blank_lines(Lines, KeptLines)
		;	KeptLines = [Line| Rest],
			keep_non_blank_lines(Lines, Rest)
		).

	blank_line([]).
	blank_line([Code| Codes]) :-
		white_space_code(Code),
		blank_line(Codes).

	white_space_code(32).
	white_space_code(0'\t).

	lines_to_tles([], []) :-
		!.
	lines_to_tles([Line1, Line2| Lines], [TLE| TLEs]) :-
		line_number(Line1, 1),
		line_number(Line2, 2),
		!,
		parse_lines_codes([], Line1, Line2, TLE),
		lines_to_tles(Lines, TLEs).
	lines_to_tles([Name, Line1, Line2| Lines], [TLE| TLEs]) :-
		\+ line_number(Name, 1),
		\+ line_number(Name, 2),
		line_number(Line1, 1),
		line_number(Line2, 2),
		!,
		parse_lines_codes(Name, Line1, Line2, TLE),
		lines_to_tles(Lines, TLEs).
	lines_to_tles(Lines, _) :-
		domain_error(tle_records, Lines).

	line_number([Code| _], Number) :-
		Code =:= 0'0 + Number.

	parse_lines_codes(NameCodes0, Line1Codes0, Line2Codes0,
		tle(Name, satellite(CatalogNumber, Classification, designator(LaunchYear, LaunchNumber, Piece)), epoch_julian_date(EpochJulianDate), drag(MeanMotionDot, MeanMotionDdot, BStar), orbit(Inclination, RightAscension, Eccentricity, ArgumentOfPerigee, MeanAnomaly, MeanMotion), ephemeris_type(EphemerisType), element_set(ElementSetNumber), revolution(RevolutionNumber))
	) :-
		normalize_name(NameCodes0, Name),
		normalize_tle_line(Line1Codes0, Line1Codes),
		normalize_tle_line(Line2Codes0, Line2Codes),
		validate_tle_checksum(Line1Codes),
		validate_tle_checksum(Line2Codes),
		line_number(Line1Codes, 1),
		line_number(Line2Codes, 2),
		parse_line_1(Line1Codes, CatalogNumber, Classification, LaunchYear, LaunchNumber, Piece, EpochJulianDate, MeanMotionDot, MeanMotionDdot, BStar, EphemerisType, ElementSetNumber),
		parse_line_2(Line2Codes, CatalogNumber, Inclination, RightAscension, Eccentricity, ArgumentOfPerigee, MeanAnomaly, MeanMotion, RevolutionNumber).

	normalize_name([], none) :-
		!.
	normalize_name(NameCodes0, Name) :-
		trim_codes(NameCodes0, NameCodes),
		(	NameCodes == [] ->
			Name = none
		;	atom_codes(Name, NameCodes)
		).

	normalize_tle_line(LineCodes0, LineCodes) :-
		take(69, LineCodes0, LineCodes).

	validate_tle_checksum(LineCodes) :-
		field_codes(LineCodes, 69, 69, [ChecksumCode]),
		digit_code_value(ChecksumCode, ProvidedChecksum),
		field_codes(LineCodes, 1, 68, PayloadCodes),
		checksum_value(PayloadCodes, ComputedChecksum),
		ProvidedChecksum =:= ComputedChecksum.

	checksum_value(Codes, Value) :-
		checksum_value(Codes, 0, Sum),
		Value is Sum mod 10.

	checksum_value([], Sum, Sum).
	checksum_value([Code| Codes], Sum0, Sum) :-
		checksum_digit_value(Code, Increment),
		Sum1 is Sum0 + Increment,
		checksum_value(Codes, Sum1, Sum).

	checksum_digit_value(Code, Value) :-
		(	digit_code_value(Code, Value0) ->
			Value = Value0
		;	Code =:= 0'- ->
			Value = 1
		;	Value = 0
		).

	parse_line_1(LineCodes, CatalogNumber, Classification, LaunchYear, LaunchNumber, Piece, EpochJulianDate, MeanMotionDot, MeanMotionDdot, BStar, EphemerisType, ElementSetNumber) :-
		field_codes(LineCodes, 3, 7, CatalogCodes),
		parse_integer_field(CatalogCodes, CatalogNumber),
		field_codes(LineCodes, 8, 8, [ClassificationCode]),
		classification_atom(ClassificationCode, Classification),
		field_codes(LineCodes, 10, 11, LaunchYearCodes),
		parse_two_digit_year_field(LaunchYearCodes, LaunchYear),
		field_codes(LineCodes, 12, 14, LaunchNumberCodes),
		parse_integer_field(LaunchNumberCodes, LaunchNumber),
		field_codes(LineCodes, 15, 17, PieceCodes),
		piece_atom(PieceCodes, Piece),
		field_codes(LineCodes, 19, 20, EpochYearCodes),
		parse_two_digit_year_field(EpochYearCodes, EpochYear),
		field_codes(LineCodes, 21, 32, EpochDayCodes),
		parse_float_field(EpochDayCodes, EpochDay),
		timestamp_epoch(EpochYear, EpochDay, EpochJulianDate),
		field_codes(LineCodes, 34, 43, MeanMotionDotCodes),
		parse_float_field(MeanMotionDotCodes, MeanMotionDot),
		field_codes(LineCodes, 45, 52, MeanMotionDdotCodes),
		parse_assumed_decimal_exponent_field(MeanMotionDdotCodes, MeanMotionDdot),
		field_codes(LineCodes, 54, 61, BStarCodes),
		parse_assumed_decimal_exponent_field(BStarCodes, BStar),
		field_codes(LineCodes, 63, 63, EphemerisTypeCodes),
		parse_integer_field(EphemerisTypeCodes, EphemerisType),
		field_codes(LineCodes, 65, 68, ElementSetCodes),
		parse_integer_field(ElementSetCodes, ElementSetNumber).

	parse_line_2(LineCodes, CatalogNumber, Inclination, RightAscension, Eccentricity, ArgumentOfPerigee, MeanAnomaly, MeanMotion, RevolutionNumber) :-
		field_codes(LineCodes, 3, 7, CatalogCodes),
		parse_integer_field(CatalogCodes, CatalogNumber),
		field_codes(LineCodes, 9, 16, InclinationCodes),
		parse_float_field(InclinationCodes, Inclination),
		field_codes(LineCodes, 18, 25, RightAscensionCodes),
		parse_float_field(RightAscensionCodes, RightAscension),
		field_codes(LineCodes, 27, 33, EccentricityCodes),
		parse_assumed_decimal_digits_field(EccentricityCodes, Eccentricity),
		field_codes(LineCodes, 35, 42, ArgumentOfPerigeeCodes),
		parse_float_field(ArgumentOfPerigeeCodes, ArgumentOfPerigee),
		field_codes(LineCodes, 44, 51, MeanAnomalyCodes),
		parse_float_field(MeanAnomalyCodes, MeanAnomaly),
		field_codes(LineCodes, 53, 63, MeanMotionCodes),
		parse_float_field(MeanMotionCodes, MeanMotion),
		field_codes(LineCodes, 64, 68, RevolutionCodes),
		parse_integer_field(RevolutionCodes, RevolutionNumber),
		Eccentricity >= 0.0,
		Eccentricity < 1.0,
		MeanMotion > 0.0.

	classification_atom(Code, Classification) :-
		code_atom(Code, Atom),
		Atom \== '',
		Classification = Atom.

	piece_atom(Codes0, Piece) :-
		trim_codes(Codes0, Codes),
		(	Codes == [] ->
			Piece = none
		;	atom_codes(Piece, Codes)
		).

	parse_integer_field(Codes0, Integer) :-
		trim_codes(Codes0, Codes),
		Codes \== [],
		number_codes(Integer, Codes),
		integer(Integer).

	parse_float_field(Codes0, Float) :-
		trim_codes(Codes0, Codes),
		Codes \== [],
		canonical_float_codes(Codes, CanonicalCodes),
		number_codes(Float, CanonicalCodes),
		number(Float).

	parse_two_digit_year_field(Codes, Year) :-
		parse_integer_field(Codes, Year2),
		expand_tle_year(Year2, Year).

	expand_tle_year(Year2, Year) :-
		Year2 >= 57,
		!,
		Year is 1900 + Year2.
	expand_tle_year(Year2, Year) :-
		Year is 2000 + Year2.

	parse_assumed_decimal_digits_field(Codes0, Value) :-
		trim_codes(Codes0, Codes),
		Codes \== [],
		all_digit_codes(Codes),
		number_codes(Integer, Codes),
		Value is Integer / 10000000.0.

	parse_assumed_decimal_exponent_field(Codes0, Value) :-
		trim_right_codes(Codes0, Codes),
		Codes = [SignCode, D1, D2, D3, D4, D5, ExponentSignCode, ExponentDigitCode],
		signed_code_multiplier(SignCode, Sign),
		all_digit_codes([D1, D2, D3, D4, D5]),
		digit_code_value(ExponentDigitCode, ExponentDigit),
		signed_code_multiplier(ExponentSignCode, ExponentSign),
		number_codes(Mantissa, [D1, D2, D3, D4, D5]),
		Exponent is ExponentSign * ExponentDigit,
		Value is Sign * (Mantissa / 100000.0) * 10.0 ** Exponent.

	signed_code_multiplier(0'-, -1) :-
		!.
	signed_code_multiplier(0'+, 1) :-
		!.
	signed_code_multiplier(0' , 1).

	timestamp_epoch(Year, DayOfYear, JulianDate) :-
		DayOfYear >= 1.0,
		DayNumber is floor(DayOfYear),
		DayFraction is DayOfYear - DayNumber,
		day_of_year_date(Year, DayNumber, date(Year, Month, Day)),
		date_time_julian_date(date_time(Year, Month, Day, 0, 0, 0.0), StartOfDayJulianDate),
		JulianDate is StartOfDayJulianDate + DayFraction.

	valid_frame(eci).
	valid_frame(ecef).
	valid_frame(wgs84_3d).

	valid_model(two_body).
	valid_model(approximate).
	valid_model(approximate_near_earth).
	valid_model(approximate_deep_space).

	default_propagation_model(approximate).

	time_spec_julian_date(tle(_, _, epoch_julian_date(EpochJulianDate), _, _, _, _, _), offset_seconds(Seconds), JulianDate) :-
		number(Seconds),
		!,
		JulianDate is EpochJulianDate + Seconds / 86400.0.
	time_spec_julian_date(_, julian_date(JulianDate), JulianDate) :-
		number(JulianDate),
		!.
	time_spec_julian_date(_, date_time(Year, Month, Day, Hours, Minutes, Seconds), JulianDate) :-
		date_time_julian_date(date_time(Year, Month, Day, Hours, Minutes, Seconds), JulianDate),
		!.
	time_spec_julian_date(_, Time, _) :-
		domain_error(tle_time, Time).

	coordinate_for_frame(eci, Coordinate, _, _, Coordinate).
	coordinate_for_frame(ecef, _, Coordinate, _, Coordinate).
	coordinate_for_frame(wgs84_3d, _, _, Coordinate, Coordinate).

	propagate_state_julian_date(Frame, TLE, JulianDate, Model, state(Position, Velocity)) :-
		orbital_state_eci(Model, TLE, JulianDate, ECI, ECIVelocity),
		eci_state_to_ecef(JulianDate, ECI, ECIVelocity, ECEF, ECEFVelocity),
		transform(ecef, wgs84_3d, ECEF, Geographic),
		coordinate_for_frame(Frame, ECI, ECEF, Geographic, Position),
		velocity_for_frame(Frame, Geographic, ECIVelocity, ECEFVelocity, Velocity).

	velocity_for_frame(eci, _Geographic, eci(VX, VY, VZ), _ECEFVelocity, eci(VX, VY, VZ)).
	velocity_for_frame(ecef, _Geographic, _ECIVelocity, ecef(VX, VY, VZ), ecef(VX, VY, VZ)).
	velocity_for_frame(wgs84_3d, Geographic, _ECIVelocity, ecef(VX, VY, VZ), enu(East, North, Up)) :-
		ecef_velocity_to_enu(Geographic, VX, VY, VZ, East, North, Up).

	propagate_julian_date(TLE, JulianDate, Model, ECI, ECEF, Geographic) :-
		orbital_state_eci(Model, TLE, JulianDate, ECI),
		eci_to_ecef(JulianDate, ECI, ECEF),
		transform(ecef, wgs84_3d, ECEF, Geographic).

	orbital_state_eci(approximate, TLE, JulianDate, ECI) :-
		(	deep_space_tle(TLE) ->
			orbital_state_eci(approximate_deep_space, TLE, JulianDate, ECI)
		;	orbital_state_eci(approximate_near_earth, TLE, JulianDate, ECI)
		).
	orbital_state_eci(two_body, TLE, JulianDate, eci(X, Y, Z)) :-
		two_body_position(TLE, JulianDate, X, Y, Z).
	orbital_state_eci(approximate_near_earth, TLE, JulianDate, eci(X, Y, Z)) :-
		approximate_near_earth_position(TLE, JulianDate, X, Y, Z).
	orbital_state_eci(approximate_deep_space, TLE, JulianDate, eci(X, Y, Z)) :-
		approximate_deep_space_position(TLE, JulianDate, X, Y, Z).

	orbital_state_eci(approximate, TLE, JulianDate, ECI, Velocity) :-
		(	deep_space_tle(TLE) ->
			orbital_state_eci(approximate_deep_space, TLE, JulianDate, ECI, Velocity)
		;	orbital_state_eci(approximate_near_earth, TLE, JulianDate, ECI, Velocity)
		).
	orbital_state_eci(two_body, TLE, JulianDate, eci(X, Y, Z), eci(VX, VY, VZ)) :-
		two_body_state(TLE, JulianDate, X, Y, Z, VX, VY, VZ).
	orbital_state_eci(approximate_near_earth, TLE, JulianDate, eci(X, Y, Z), eci(VX, VY, VZ)) :-
		approximate_near_earth_state(TLE, JulianDate, X, Y, Z, VX, VY, VZ).
	orbital_state_eci(approximate_deep_space, TLE, JulianDate, eci(X, Y, Z), eci(VX, VY, VZ)) :-
		approximate_deep_space_state(TLE, JulianDate, X, Y, Z, VX, VY, VZ).

	two_body_position(TLE, JulianDate, X, Y, Z) :-
		two_body_mean_elements(TLE, JulianDate, MeanElements),
		mean_elements_position(MeanElements, X, Y, Z).

	two_body_state(TLE, JulianDate, X, Y, Z, VX, VY, VZ) :-
		two_body_mean_elements(TLE, JulianDate, MeanElements),
		mean_elements_state(MeanElements, X, Y, Z, VX, VY, VZ).

	two_body_mean_elements(tle(_, _, epoch_julian_date(EpochJulianDate), _, orbit(InclinationDegrees, RightAscensionDegrees, Eccentricity, ArgumentOfPerigeeDegrees, MeanAnomalyDegrees, MeanMotionRevolutionsPerDay), _, _, _), JulianDate, mean_elements(SemiMajorAxis, Eccentricity, Inclination, RightAscension, ArgumentOfPerigee, NormalizedMeanAnomaly, MeanMotionRadiansPerSecond)) :-
		SecondsSinceEpoch is (JulianDate - EpochJulianDate) * 86400.0,
		deg_to_rad(InclinationDegrees, Inclination),
		deg_to_rad(RightAscensionDegrees, RightAscension),
		deg_to_rad(ArgumentOfPerigeeDegrees, ArgumentOfPerigee),
		deg_to_rad(MeanAnomalyDegrees, MeanAnomaly0),
		mean_motion_radians_per_second(MeanMotionRevolutionsPerDay, MeanMotionRadiansPerSecond),
		mu_earth(Mu),
		semi_major_axis(Mu, MeanMotionRadiansPerSecond, SemiMajorAxis),
		MeanAnomaly is MeanAnomaly0 + MeanMotionRadiansPerSecond * SecondsSinceEpoch,
		normalize_angle_radians(MeanAnomaly, NormalizedMeanAnomaly).

	approximate_near_earth_position(TLE, JulianDate, X, Y, Z) :-
		approximate_near_earth_elements(TLE, JulianDate, ShortPeriodElements),
		short_period_elements_position(ShortPeriodElements, X, Y, Z).

	approximate_near_earth_state(TLE, JulianDate, X, Y, Z, VX, VY, VZ) :-
		approximate_near_earth_elements(TLE, JulianDate, ShortPeriodElements),
		short_period_elements_state(ShortPeriodElements, X, Y, Z, VX, VY, VZ).

	approximate_near_earth_elements(tle(_, _, epoch_julian_date(EpochJulianDate), drag(MeanMotionDotField, MeanMotionDdotField, BStar), orbit(InclinationDegrees, RightAscensionDegrees, Eccentricity0, ArgumentOfPerigeeDegrees, MeanAnomalyDegrees, MeanMotionRevolutionsPerDay), _, _, _), JulianDate, short_period_elements(SemiMajorAxis, DraggedEccentricity, EccentricAnomaly, Inclination, NormalizedRightAscension, RightAscensionRate, NormalizedArgumentOfPerigee, ArgumentOfPerigeeRate, TrueAnomaly, CurrentMeanMotion)) :-
		mu_earth(Mu),
		SecondsSinceEpoch is (JulianDate - EpochJulianDate) * 86400.0,
		deg_to_rad(InclinationDegrees, Inclination),
		deg_to_rad(RightAscensionDegrees, RightAscension0),
		deg_to_rad(ArgumentOfPerigeeDegrees, ArgumentOfPerigee0),
		deg_to_rad(MeanAnomalyDegrees, MeanAnomaly0),
		mean_motion_radians_per_second(MeanMotionRevolutionsPerDay, MeanMotionRadiansPerSecond0),
		two_body_mean_motion_derivatives(MeanMotionDotField, MeanMotionDdotField, MeanMotionDot, MeanMotionDdot),
		semi_major_axis(Mu, MeanMotionRadiansPerSecond0, SemiMajorAxis00),
		drag_adjusted_elements(SemiMajorAxis00, Eccentricity0, BStar, SecondsSinceEpoch, SemiMajorAxis0, Eccentricity),
		current_mean_motion(Mu, SemiMajorAxis0, MeanMotionRadiansPerSecond0, MeanMotionDot, MeanMotionDdot, SecondsSinceEpoch, CurrentMeanMotion),
		j2_secular_rates(SemiMajorAxis0, Eccentricity, Inclination, CurrentMeanMotion, RightAscensionRate, ArgumentOfPerigeeRate, MeanAnomalyRate),
		CurrentMeanMotion0 is MeanMotionRadiansPerSecond0 + MeanMotionDot * SecondsSinceEpoch + 0.5 * MeanMotionDdot * SecondsSinceEpoch * SecondsSinceEpoch,
		positive_mean_motion(CurrentMeanMotion0, CurrentMeanMotion1),
		semi_major_axis(Mu, CurrentMeanMotion1, SemiMajorAxis1),
		drag_adjusted_elements(SemiMajorAxis1, Eccentricity, BStar, SecondsSinceEpoch, SemiMajorAxis, DraggedEccentricity),
		RightAscension is RightAscension0 + RightAscensionRate * SecondsSinceEpoch,
		ArgumentOfPerigee is ArgumentOfPerigee0 + ArgumentOfPerigeeRate * SecondsSinceEpoch,
		MeanAnomaly is MeanAnomaly0 + (MeanMotionRadiansPerSecond0 + MeanAnomalyRate) * SecondsSinceEpoch + 0.5 * MeanMotionDot * SecondsSinceEpoch * SecondsSinceEpoch + (MeanMotionDdot * SecondsSinceEpoch * SecondsSinceEpoch * SecondsSinceEpoch) / 6.0,
		normalize_angle_radians(RightAscension, NormalizedRightAscension),
		normalize_angle_radians(ArgumentOfPerigee, NormalizedArgumentOfPerigee),
		normalize_angle_radians(MeanAnomaly, NormalizedMeanAnomaly),
		solve_kepler_equation(NormalizedMeanAnomaly, DraggedEccentricity, EccentricAnomaly),
		true_anomaly(EccentricAnomaly, DraggedEccentricity, TrueAnomaly).

	approximate_deep_space_position(TLE, JulianDate, X, Y, Z) :-
		approximate_deep_space_mean_elements(TLE, JulianDate, MeanElements),
		mean_elements_position(MeanElements, X, Y, Z).

	approximate_deep_space_state(TLE, JulianDate, X, Y, Z, VX, VY, VZ) :-
		approximate_deep_space_mean_elements(TLE, JulianDate, MeanElements),
		mean_elements_state(MeanElements, X, Y, Z, VX, VY, VZ).

	approximate_deep_space_mean_elements(tle(_, _, epoch_julian_date(EpochJulianDate), drag(MeanMotionDotField, MeanMotionDdotField, BStar), orbit(InclinationDegrees, RightAscensionDegrees, Eccentricity0, ArgumentOfPerigeeDegrees, MeanAnomalyDegrees, MeanMotionRevolutionsPerDay), _, _, _), JulianDate, mean_elements(SemiMajorAxis, CorrectedEccentricity, Inclination, NormalizedRightAscension, NormalizedArgumentOfPerigee, NormalizedMeanAnomaly, CurrentMeanMotion)) :-
		mu_earth(Mu),
		DaysSinceEpoch is JulianDate - EpochJulianDate,
		SecondsSinceEpoch is DaysSinceEpoch * 86400.0,
		deg_to_rad(InclinationDegrees, Inclination0),
		deg_to_rad(RightAscensionDegrees, RightAscension0),
		deg_to_rad(ArgumentOfPerigeeDegrees, ArgumentOfPerigee0),
		deg_to_rad(MeanAnomalyDegrees, MeanAnomaly0),
		mean_motion_radians_per_second(MeanMotionRevolutionsPerDay, MeanMotionRadiansPerSecond0),
		two_body_mean_motion_derivatives(MeanMotionDotField, MeanMotionDdotField, MeanMotionDot, MeanMotionDdot),
		semi_major_axis(Mu, MeanMotionRadiansPerSecond0, SemiMajorAxis00),
		deep_space_drag_adjusted_elements(SemiMajorAxis00, Eccentricity0, BStar, DaysSinceEpoch, SemiMajorAxis0, Eccentricity),
		current_mean_motion(Mu, SemiMajorAxis0, MeanMotionRadiansPerSecond0, MeanMotionDot, MeanMotionDdot, SecondsSinceEpoch, CurrentMeanMotion),
		deep_space_secular_rates(SemiMajorAxis0, Eccentricity, Inclination0, CurrentMeanMotion, RightAscensionRate, ArgumentOfPerigeeRate, MeanAnomalyRate),
		deep_space_resonance_class(MeanMotionRevolutionsPerDay, ResonanceClass),
		approximate_deep_space_perturbations(ResonanceClass, DaysSinceEpoch, BStar, Inclination0, Eccentricity, MeanMotionRevolutionsPerDay, DeltaSemiMajorAxisFactor, DeltaEccentricity, DeltaInclination, DeltaRightAscension, DeltaArgumentOfPerigee, DeltaMeanAnomaly),
		SemiMajorAxis is SemiMajorAxis0 * (1.0 + DeltaSemiMajorAxisFactor),
		clamp_eccentricity(Eccentricity + DeltaEccentricity, CorrectedEccentricity),
		RightAscension is RightAscension0 + RightAscensionRate * SecondsSinceEpoch + DeltaRightAscension,
		ArgumentOfPerigee is ArgumentOfPerigee0 + ArgumentOfPerigeeRate * SecondsSinceEpoch + DeltaArgumentOfPerigee,
		MeanAnomaly is MeanAnomaly0 + (MeanMotionRadiansPerSecond0 + MeanAnomalyRate) * SecondsSinceEpoch + 0.5 * MeanMotionDot * SecondsSinceEpoch * SecondsSinceEpoch + (MeanMotionDdot * SecondsSinceEpoch * SecondsSinceEpoch * SecondsSinceEpoch) / 6.0 + DeltaMeanAnomaly,
		Inclination is Inclination0 + DeltaInclination,
		normalize_angle_radians(RightAscension, NormalizedRightAscension),
		normalize_angle_radians(ArgumentOfPerigee, NormalizedArgumentOfPerigee),
		normalize_angle_radians(MeanAnomaly, NormalizedMeanAnomaly).

	mu_earth(398600441800000.0).

	earth_rotation_rate(7.2921150e-5).

	earth_equatorial_radius(6378137.0).

	earth_j2(1.08262668e-3).

	deep_space_period_threshold_minutes(225.0).

	mean_motion_radians_per_second(MeanMotionRevolutionsPerDay, MeanMotionRadiansPerSecond) :-
		MeanMotionRadiansPerSecond is MeanMotionRevolutionsPerDay * 2.0 * pi / 86400.0.

	two_body_mean_motion_derivatives(MeanMotionDotField, MeanMotionDdotField, MeanMotionDot, MeanMotionDdot) :-
		MeanMotionDot is 4.0 * pi * MeanMotionDotField / (86400.0 * 86400.0),
		MeanMotionDdot is 12.0 * pi * MeanMotionDdotField / (86400.0 * 86400.0 * 86400.0).

	non_negative(Value0, Value) :-
		(	Value0 =< 0.0 ->
			Value = 0.0
		;	Value = Value0
		).

	positive_or_epsilon(Value0, Epsilon, Value) :-
		(	Value0 =< Epsilon ->
			Value = Epsilon
		;	Value = Value0
		).

	semi_major_axis(Mu, MeanMotionRadiansPerSecond, SemiMajorAxis) :-
		SemiMajorAxis is (Mu / (MeanMotionRadiansPerSecond * MeanMotionRadiansPerSecond)) ** (1.0 / 3.0).

	current_mean_motion(Mu, SemiMajorAxis, MeanMotionRadiansPerSecond0, MeanMotionDot, MeanMotionDdot, SecondsSinceEpoch, CurrentMeanMotion) :-
		CurrentMeanMotion0 is MeanMotionRadiansPerSecond0 + MeanMotionDot * SecondsSinceEpoch + 0.5 * MeanMotionDdot * SecondsSinceEpoch * SecondsSinceEpoch,
		positive_mean_motion(CurrentMeanMotion0, CurrentMeanMotion1),
		current_mean_motion_from_axis(Mu, SemiMajorAxis, CurrentMeanMotion1, CurrentMeanMotion).

	current_mean_motion_from_axis(Mu, SemiMajorAxis, FallbackMeanMotion, MeanMotion) :-
		(	SemiMajorAxis =< 0.0 ->
			MeanMotion = FallbackMeanMotion
		;	MeanMotionFromAxis is sqrt(Mu / (SemiMajorAxis * SemiMajorAxis * SemiMajorAxis)),
			positive_mean_motion(MeanMotionFromAxis, MeanMotion)
		).

	positive_mean_motion(MeanMotion0, MeanMotion) :-
		(	MeanMotion0 =< 1.0e-12 ->
			MeanMotion = 1.0e-12
		;	MeanMotion = MeanMotion0
		).

	drag_adjusted_elements(SemiMajorAxis0, Eccentricity0, BStar, SecondsSinceEpoch, SemiMajorAxis, Eccentricity) :-
		AbsSecondsSinceEpoch is abs(SecondsSinceEpoch),
		DragExponent is -6.0 * abs(BStar) * AbsSecondsSinceEpoch / 86400.0,
		DragScale is exp(DragExponent),
		SemiMajorAxis is SemiMajorAxis0 * DragScale,
		Eccentricity1 is Eccentricity0 * exp(DragExponent / 3.0),
		clamp_eccentricity(Eccentricity1, Eccentricity).

	deep_space_drag_adjusted_elements(SemiMajorAxis0, Eccentricity0, BStar, DaysSinceEpoch, SemiMajorAxis, Eccentricity) :-
		AbsDaysSinceEpoch is abs(DaysSinceEpoch),
		DragExponent is -1.5 * abs(BStar) * AbsDaysSinceEpoch,
		DragScale is exp(DragExponent),
		SemiMajorAxis is SemiMajorAxis0 * DragScale,
		Eccentricity1 is Eccentricity0 * exp(DragExponent / 2.0),
		clamp_eccentricity(Eccentricity1, Eccentricity).

	clamp_eccentricity(Eccentricity0, Eccentricity) :-
		(	Eccentricity0 < 1.0e-9 ->
			Eccentricity = 1.0e-9
		;	Eccentricity0 > 0.999999 ->
			Eccentricity = 0.999999
		;	Eccentricity = Eccentricity0
		).

	j2_secular_rates(SemiMajorAxis, Eccentricity, Inclination, MeanMotion, RightAscensionRate, ArgumentOfPerigeeRate, MeanAnomalyRate) :-
		earth_equatorial_radius(EarthRadius),
		earth_j2(J2),
		BetaSquared0 is 1.0 - Eccentricity * Eccentricity,
		non_negative(BetaSquared0, BetaSquared),
		Beta is sqrt(BetaSquared),
		SemilatusRectum0 is SemiMajorAxis * BetaSquared,
		positive_or_epsilon(SemilatusRectum0, 1.0e-9, SemilatusRectum),
		RadiusRatio is EarthRadius / SemilatusRectum,
		CosInclination is cos(Inclination),
		CosInclinationSquared is CosInclination * CosInclination,
		CommonRate is 1.5 * J2 * MeanMotion * RadiusRatio * RadiusRatio,
		RightAscensionRate is -CommonRate * CosInclination,
		ArgumentOfPerigeeRate is 0.5 * CommonRate * (5.0 * CosInclinationSquared - 1.0),
		MeanAnomalyRate is 0.5 * CommonRate * Beta * (3.0 * CosInclinationSquared - 1.0).

	deep_space_secular_rates(SemiMajorAxis, Eccentricity, Inclination, MeanMotion, RightAscensionRate, ArgumentOfPerigeeRate, MeanAnomalyRate) :-
		j2_secular_rates(SemiMajorAxis, Eccentricity, Inclination, MeanMotion, BaseRightAscensionRate, BaseArgumentOfPerigeeRate, BaseMeanAnomalyRate),
		RightAscensionRate is 0.35 * BaseRightAscensionRate,
		ArgumentOfPerigeeRate is 0.35 * BaseArgumentOfPerigeeRate,
		MeanAnomalyRate is 0.35 * BaseMeanAnomalyRate.

	deep_space_tle(tle(_, _, _, _, orbit(_, _, _, _, _, MeanMotionRevolutionsPerDay), _, _, _)) :-
		OrbitalPeriodMinutes is 1440.0 / MeanMotionRevolutionsPerDay,
		deep_space_period_threshold_minutes(ThresholdMinutes),
		OrbitalPeriodMinutes >= ThresholdMinutes.

	deep_space_resonance_class(MeanMotionRevolutionsPerDay, synchronous) :-
		abs(MeanMotionRevolutionsPerDay - 1.0027) =< 0.05,
		!.
	deep_space_resonance_class(MeanMotionRevolutionsPerDay, half_day) :-
		MeanMotionRevolutionsPerDay >= 1.8,
		MeanMotionRevolutionsPerDay =< 2.2,
		!.
	deep_space_resonance_class(_, other).

	approximate_deep_space_perturbations(ResonanceClass, DaysSinceEpoch, BStar, Inclination, Eccentricity, MeanMotionRevolutionsPerDay, DeltaSemiMajorAxisFactor, DeltaEccentricity, DeltaInclination, DeltaRightAscension, DeltaArgumentOfPerigee, DeltaMeanAnomaly) :-
		solar_long_period_perturbations(DaysSinceEpoch, Inclination, Eccentricity, MeanMotionRevolutionsPerDay, SolarDeltaSemiMajorAxisFactor, SolarDeltaEccentricity, SolarDeltaInclination, SolarDeltaRightAscension, SolarDeltaArgumentOfPerigee, SolarDeltaMeanAnomaly),
		lunar_long_period_perturbations(DaysSinceEpoch, Inclination, Eccentricity, MeanMotionRevolutionsPerDay, LunarDeltaSemiMajorAxisFactor, LunarDeltaEccentricity, LunarDeltaInclination, LunarDeltaRightAscension, LunarDeltaArgumentOfPerigee, LunarDeltaMeanAnomaly),
		deep_space_resonance_perturbations(ResonanceClass, DaysSinceEpoch, BStar, Eccentricity, MeanMotionRevolutionsPerDay, ResonanceDeltaSemiMajorAxisFactor, ResonanceDeltaEccentricity, ResonanceDeltaInclination, ResonanceDeltaRightAscension, ResonanceDeltaArgumentOfPerigee, ResonanceDeltaMeanAnomaly),
		DeltaSemiMajorAxisFactor is SolarDeltaSemiMajorAxisFactor + LunarDeltaSemiMajorAxisFactor + ResonanceDeltaSemiMajorAxisFactor,
		DeltaEccentricity is SolarDeltaEccentricity + LunarDeltaEccentricity + ResonanceDeltaEccentricity,
		DeltaInclination is SolarDeltaInclination + LunarDeltaInclination + ResonanceDeltaInclination,
		DeltaRightAscension is SolarDeltaRightAscension + LunarDeltaRightAscension + ResonanceDeltaRightAscension,
		DeltaArgumentOfPerigee is SolarDeltaArgumentOfPerigee + LunarDeltaArgumentOfPerigee + ResonanceDeltaArgumentOfPerigee,
		DeltaMeanAnomaly is SolarDeltaMeanAnomaly + LunarDeltaMeanAnomaly + ResonanceDeltaMeanAnomaly.

	solar_long_period_perturbations(DaysSinceEpoch, Inclination, Eccentricity, MeanMotionRevolutionsPerDay, DeltaSemiMajorAxisFactor, DeltaEccentricity, DeltaInclination, DeltaRightAscension, DeltaArgumentOfPerigee, DeltaMeanAnomaly) :-
		deep_space_phase(DaysSinceEpoch, 1.0 / 365.2421897, Phase),
		Scale is 4.0e-5 * (1.0 + Eccentricity + abs(sin(Inclination)) + 0.05 * MeanMotionRevolutionsPerDay),
		DeltaSemiMajorAxisFactor is 0.60 * Scale * cos(Phase + 0.17),
		DeltaEccentricity is 0.18 * Scale * sin(Phase - 0.31),
		DeltaInclination is 0.12 * Scale * cos(Phase + 0.63),
		DeltaRightAscension is 0.95 * Scale * sin(Phase),
		DeltaArgumentOfPerigee is 1.35 * Scale * cos(Phase + 0.41),
		DeltaMeanAnomaly is 2.10 * Scale * sin(Phase - 0.24).

	lunar_long_period_perturbations(DaysSinceEpoch, Inclination, Eccentricity, MeanMotionRevolutionsPerDay, DeltaSemiMajorAxisFactor, DeltaEccentricity, DeltaInclination, DeltaRightAscension, DeltaArgumentOfPerigee, DeltaMeanAnomaly) :-
		deep_space_phase(DaysSinceEpoch, 1.0 / 27.321582, Phase),
		Scale is 6.0e-5 * (1.0 + 1.5 * Eccentricity + 0.5 * abs(cos(Inclination)) + 0.03 * MeanMotionRevolutionsPerDay),
		DeltaSemiMajorAxisFactor is 0.75 * Scale * cos(Phase - 0.12),
		DeltaEccentricity is 0.24 * Scale * sin(Phase + 0.37),
		DeltaInclination is 0.16 * Scale * cos(Phase - 0.54),
		DeltaRightAscension is 1.10 * Scale * sin(Phase + 0.08),
		DeltaArgumentOfPerigee is 1.55 * Scale * cos(Phase + 0.52),
		DeltaMeanAnomaly is 2.40 * Scale * sin(Phase - 0.19).

	deep_space_resonance_perturbations(synchronous, DaysSinceEpoch, BStar, Eccentricity, _MeanMotionRevolutionsPerDay, DeltaSemiMajorAxisFactor, DeltaEccentricity, DeltaInclination, DeltaRightAscension, DeltaArgumentOfPerigee, DeltaMeanAnomaly) :-
		deep_space_phase(DaysSinceEpoch, 1.00273790935, Phase),
		Scale is 9.0e-5 * (1.0 + 12.0 * abs(BStar) + 2.0 * Eccentricity),
		DeltaSemiMajorAxisFactor is 0.90 * Scale * cos(Phase + 0.11),
		DeltaEccentricity is 0.10 * Scale * sin(Phase - 0.17),
		DeltaInclination is 0.05 * Scale * cos(Phase + 0.07),
		DeltaRightAscension is 1.20 * Scale * sin(Phase),
		DeltaArgumentOfPerigee is 1.45 * Scale * cos(Phase + 0.28),
		DeltaMeanAnomaly is 2.85 * Scale * sin(Phase - 0.33).

	deep_space_resonance_perturbations(half_day, DaysSinceEpoch, BStar, Eccentricity, _MeanMotionRevolutionsPerDay, DeltaSemiMajorAxisFactor, DeltaEccentricity, DeltaInclination, DeltaRightAscension, DeltaArgumentOfPerigee, DeltaMeanAnomaly) :-
		deep_space_phase(DaysSinceEpoch, 2.0, Phase),
		Scale is 1.1e-4 * (1.0 + 10.0 * abs(BStar) + 2.5 * Eccentricity),
		DeltaSemiMajorAxisFactor is 1.00 * Scale * cos(Phase - 0.09),
		DeltaEccentricity is 0.14 * Scale * sin(Phase + 0.22),
		DeltaInclination is 0.07 * Scale * cos(Phase - 0.18),
		DeltaRightAscension is 1.35 * Scale * sin(Phase + 0.13),
		DeltaArgumentOfPerigee is 1.65 * Scale * cos(Phase + 0.47),
		DeltaMeanAnomaly is 3.10 * Scale * sin(Phase - 0.41).

	deep_space_resonance_perturbations(other, DaysSinceEpoch, BStar, Eccentricity, MeanMotionRevolutionsPerDay, DeltaSemiMajorAxisFactor, DeltaEccentricity, DeltaInclination, DeltaRightAscension, DeltaArgumentOfPerigee, DeltaMeanAnomaly) :-
		CyclesPerDay is max(0.1, MeanMotionRevolutionsPerDay / 4.0),
		deep_space_phase(DaysSinceEpoch, CyclesPerDay, Phase),
		Scale is 5.0e-5 * (1.0 + 8.0 * abs(BStar) + 1.5 * Eccentricity),
		DeltaSemiMajorAxisFactor is 0.55 * Scale * cos(Phase + 0.05),
		DeltaEccentricity is 0.08 * Scale * sin(Phase - 0.14),
		DeltaInclination is 0.04 * Scale * cos(Phase + 0.03),
		DeltaRightAscension is 0.75 * Scale * sin(Phase),
		DeltaArgumentOfPerigee is 0.95 * Scale * cos(Phase + 0.16),
		DeltaMeanAnomaly is 1.60 * Scale * sin(Phase - 0.21).

	deep_space_phase(DaysSinceEpoch, CyclesPerDay, Phase) :-
		Phase0 is 2.0 * pi * CyclesPerDay * DaysSinceEpoch,
		normalize_angle_radians(Phase0, Phase).

	deg_to_rad(Degrees, Radians) :-
		Radians is Degrees * pi / 180.0.

%	rad_to_deg(Radians, Degrees) :-
%		Degrees is Radians * 180.0 / pi.

	normalize_angle_radians(Angle, NormalizedAngle) :-
		TwoPi is 2.0 * pi,
		Normalized0 is Angle - floor(Angle / TwoPi) * TwoPi,
		(	Normalized0 < 0.0 ->
			NormalizedAngle is Normalized0 + TwoPi
		;	NormalizedAngle = Normalized0
		).

	solve_kepler_equation(MeanAnomaly, Eccentricity, EccentricAnomaly) :-
		(	Eccentricity < 0.8 ->
			InitialGuess = MeanAnomaly
		;	InitialGuess = pi
		),
		solve_kepler_equation(MeanAnomaly, Eccentricity, InitialGuess, 0, EccentricAnomaly).

	solve_kepler_equation(MeanAnomaly, Eccentricity, Estimate, Iteration, EccentricAnomaly) :-
		Delta is (Estimate - Eccentricity * sin(Estimate) - MeanAnomaly) / (1.0 - Eccentricity * cos(Estimate)),
		NextEstimate is Estimate - Delta,
		(	abs(Delta) < 1.0e-12 ->
			EccentricAnomaly = NextEstimate
		;	Iteration >= 15 ->
			EccentricAnomaly = NextEstimate
		;	NextIteration is Iteration + 1,
			solve_kepler_equation(MeanAnomaly, Eccentricity, NextEstimate, NextIteration, EccentricAnomaly)
		).

	true_anomaly(EccentricAnomaly, Eccentricity, TrueAnomaly) :-
		PositiveRootTerm0 is 1.0 + Eccentricity,
		non_negative(PositiveRootTerm0, PositiveRootTerm),
		NegativeRootTerm0 is 1.0 - Eccentricity,
		non_negative(NegativeRootTerm0, NegativeRootTerm),
		SinHalfTrueAnomaly is sqrt(PositiveRootTerm) * sin(EccentricAnomaly / 2.0),
		CosHalfTrueAnomaly is sqrt(NegativeRootTerm) * cos(EccentricAnomaly / 2.0),
		TrueAnomaly0 is 2.0 * atan2(SinHalfTrueAnomaly, CosHalfTrueAnomaly),
		normalize_angle_radians(TrueAnomaly0, TrueAnomaly).

	orbital_plane_coordinates(SemiMajorAxis, Eccentricity, EccentricAnomaly, X, Y) :-
		RootTerm0 is 1.0 - Eccentricity * Eccentricity,
		non_negative(RootTerm0, RootTerm1),
		RootTerm is sqrt(RootTerm1),
		X is SemiMajorAxis * (cos(EccentricAnomaly) - Eccentricity),
		Y is SemiMajorAxis * RootTerm * sin(EccentricAnomaly).

	mean_elements_to_eci(SemiMajorAxis, Eccentricity, Inclination, RightAscension, ArgumentOfPerigee, MeanAnomaly, X, Y, Z) :-
		solve_kepler_equation(MeanAnomaly, Eccentricity, EccentricAnomaly),
		orbital_plane_coordinates(SemiMajorAxis, Eccentricity, EccentricAnomaly, PerifocalX, PerifocalY),
		orbital_to_eci(PerifocalX, PerifocalY, RightAscension, Inclination, ArgumentOfPerigee, X, Y, Z).

	mean_elements_position(mean_elements(SemiMajorAxis, Eccentricity, Inclination, RightAscension, ArgumentOfPerigee, MeanAnomaly, _MeanMotion), X, Y, Z) :-
		mean_elements_to_eci(SemiMajorAxis, Eccentricity, Inclination, RightAscension, ArgumentOfPerigee, MeanAnomaly, X, Y, Z).

	mean_elements_to_eci_state(SemiMajorAxis, Eccentricity, Inclination, RightAscension, ArgumentOfPerigee, MeanAnomaly, MeanMotion, X, Y, Z, VX, VY, VZ) :-
		solve_kepler_equation(MeanAnomaly, Eccentricity, EccentricAnomaly),
		orbital_plane_coordinates(SemiMajorAxis, Eccentricity, EccentricAnomaly, PerifocalX, PerifocalY),
		orbital_plane_velocity(SemiMajorAxis, Eccentricity, EccentricAnomaly, MeanMotion, PerifocalVX, PerifocalVY),
		orbital_to_eci(PerifocalX, PerifocalY, RightAscension, Inclination, ArgumentOfPerigee, X, Y, Z),
		orbital_to_eci(PerifocalVX, PerifocalVY, RightAscension, Inclination, ArgumentOfPerigee, VX, VY, VZ).

	mean_elements_state(mean_elements(SemiMajorAxis, Eccentricity, Inclination, RightAscension, ArgumentOfPerigee, MeanAnomaly, MeanMotion), X, Y, Z, VX, VY, VZ) :-
		mean_elements_to_eci_state(SemiMajorAxis, Eccentricity, Inclination, RightAscension, ArgumentOfPerigee, MeanAnomaly, MeanMotion, X, Y, Z, VX, VY, VZ).

	orbital_plane_velocity(SemiMajorAxis, Eccentricity, EccentricAnomaly, MeanMotion, VX, VY) :-
		RootTerm0 is 1.0 - Eccentricity * Eccentricity,
		non_negative(RootTerm0, RootTerm1),
		RootTerm is sqrt(RootTerm1),
		Denominator0 is 1.0 - Eccentricity * cos(EccentricAnomaly),
		positive_or_epsilon(Denominator0, 1.0e-12, Denominator),
		VelocityFactor is MeanMotion / Denominator,
		VX is -SemiMajorAxis * sin(EccentricAnomaly) * VelocityFactor,
		VY is SemiMajorAxis * RootTerm * cos(EccentricAnomaly) * VelocityFactor.

	short_period_corrected_position(SemiMajorAxis, Eccentricity, EccentricAnomaly, Inclination, RightAscension, ArgumentOfPerigee, TrueAnomaly, MeanMotion, X, Y, Z) :-
		earth_equatorial_radius(EarthRadius),
		earth_j2(J2),
		Radius is SemiMajorAxis * (1.0 - Eccentricity * cos(EccentricAnomaly)),
		BetaSquared0 is 1.0 - Eccentricity * Eccentricity,
		non_negative(BetaSquared0, BetaSquared),
		Beta is sqrt(BetaSquared),
		SemilatusRectum0 is SemiMajorAxis * BetaSquared,
		positive_or_epsilon(SemilatusRectum0, 1.0e-9, SemilatusRectum),
		ArgumentOfLatitude0 is ArgumentOfPerigee + TrueAnomaly,
		normalize_angle_radians(ArgumentOfLatitude0, ArgumentOfLatitude),
		CosInclination is cos(Inclination),
		SinInclination is sin(Inclination),
		CosInclinationSquared is CosInclination * CosInclination,
		X1mth2 is 1.0 - CosInclinationSquared,
		X3thm1 is 3.0 * CosInclinationSquared - 1.0,
		X7thm1 is 7.0 * CosInclinationSquared - 1.0,
		Temp1 is 0.5 * J2 * EarthRadius * EarthRadius / SemilatusRectum,
		Temp2 is Temp1 / SemilatusRectum,
		Sin2U is sin(2.0 * ArgumentOfLatitude),
		Cos2U is cos(2.0 * ArgumentOfLatitude),
		CorrectedRadius is Radius * (1.0 - 1.5 * Temp2 * Beta * X3thm1) + 0.5 * Temp1 * X1mth2 * Cos2U,
		CorrectedArgumentOfLatitude is ArgumentOfLatitude - 0.25 * Temp2 * X7thm1 * Sin2U,
		CorrectedRightAscension is RightAscension + 1.5 * Temp2 * CosInclination * Sin2U,
		CorrectedInclination is Inclination + 1.5 * Temp2 * CosInclination * SinInclination * Cos2U,
		short_period_position(CorrectedRadius, CorrectedArgumentOfLatitude, CorrectedRightAscension, CorrectedInclination, MeanMotion, X, Y, Z).

	short_period_elements_position(short_period_elements(SemiMajorAxis, Eccentricity, EccentricAnomaly, Inclination, RightAscension, _RightAscensionRate, ArgumentOfPerigee, _ArgumentOfPerigeeRate, TrueAnomaly, MeanMotion), X, Y, Z) :-
		short_period_corrected_position(SemiMajorAxis, Eccentricity, EccentricAnomaly, Inclination, RightAscension, ArgumentOfPerigee, TrueAnomaly, MeanMotion, X, Y, Z).

	short_period_position(Radius, ArgumentOfLatitude, RightAscension, Inclination, _MeanMotion, X, Y, Z) :-
		CosU is cos(ArgumentOfLatitude),
		SinU is sin(ArgumentOfLatitude),
		CosRightAscension is cos(RightAscension),
		SinRightAscension is sin(RightAscension),
		CosInclination is cos(Inclination),
		SinInclination is sin(Inclination),
		Xmx is -SinRightAscension * CosInclination,
		Xmy is CosRightAscension * CosInclination,
		Ux is Xmx * SinU + CosRightAscension * CosU,
		Uy is Xmy * SinU + SinRightAscension * CosU,
		Uz is SinInclination * SinU,
		X is Radius * Ux,
		Y is Radius * Uy,
		Z is Radius * Uz.

	short_period_corrected_state(SemiMajorAxis, Eccentricity, EccentricAnomaly, Inclination, RightAscension, RightAscensionRate, ArgumentOfPerigee, ArgumentOfPerigeeRate, TrueAnomaly, MeanMotion, X, Y, Z, VX, VY, VZ) :-
		earth_equatorial_radius(EarthRadius),
		earth_j2(J2),
		Radius is SemiMajorAxis * (1.0 - Eccentricity * cos(EccentricAnomaly)),
		BetaSquared0 is 1.0 - Eccentricity * Eccentricity,
		non_negative(BetaSquared0, BetaSquared),
		Beta is sqrt(BetaSquared),
		SemilatusRectum0 is SemiMajorAxis * BetaSquared,
		positive_or_epsilon(SemilatusRectum0, 1.0e-9, SemilatusRectum),
		ArgumentOfLatitude0 is ArgumentOfPerigee + TrueAnomaly,
		normalize_angle_radians(ArgumentOfLatitude0, ArgumentOfLatitude),
		CosInclination is cos(Inclination),
		SinInclination is sin(Inclination),
		CosInclinationSquared is CosInclination * CosInclination,
		X1mth2 is 1.0 - CosInclinationSquared,
		X3thm1 is 3.0 * CosInclinationSquared - 1.0,
		X7thm1 is 7.0 * CosInclinationSquared - 1.0,
		Temp1 is 0.5 * J2 * EarthRadius * EarthRadius / SemilatusRectum,
		Temp2 is Temp1 / SemilatusRectum,
		Sin2U is sin(2.0 * ArgumentOfLatitude),
		Cos2U is cos(2.0 * ArgumentOfLatitude),
		CorrectedRadius is Radius * (1.0 - 1.5 * Temp2 * Beta * X3thm1) + 0.5 * Temp1 * X1mth2 * Cos2U,
		CorrectedArgumentOfLatitude is ArgumentOfLatitude - 0.25 * Temp2 * X7thm1 * Sin2U,
		CorrectedRightAscension is RightAscension + 1.5 * Temp2 * CosInclination * Sin2U,
		CorrectedInclination is Inclination + 1.5 * Temp2 * CosInclination * SinInclination * Cos2U,
		short_period_position(CorrectedRadius, CorrectedArgumentOfLatitude, CorrectedRightAscension, CorrectedInclination, MeanMotion, X, Y, Z),
		eccentric_anomaly_rate(Eccentricity, EccentricAnomaly, MeanMotion, EccentricAnomalyRate),
		RadiusRate is SemiMajorAxis * Eccentricity * sin(EccentricAnomaly) * EccentricAnomalyRate,
		TrueAnomalyRate is Beta * EccentricAnomalyRate / (1.0 - Eccentricity * cos(EccentricAnomaly)),
		ArgumentOfLatitudeRate is ArgumentOfPerigeeRate + TrueAnomalyRate,
		CorrectedRadiusRate is RadiusRate * (1.0 - 1.5 * Temp2 * Beta * X3thm1) - Temp1 * X1mth2 * Sin2U * ArgumentOfLatitudeRate,
		CorrectedArgumentOfLatitudeRate is ArgumentOfLatitudeRate * (1.0 - 0.5 * Temp2 * X7thm1 * Cos2U),
		CorrectedRightAscensionRate is RightAscensionRate + 3.0 * Temp2 * CosInclination * Cos2U * ArgumentOfLatitudeRate,
		CorrectedInclinationRate is -3.0 * Temp2 * CosInclination * SinInclination * Sin2U * ArgumentOfLatitudeRate,
		short_period_velocity(CorrectedRadius, CorrectedRadiusRate, CorrectedArgumentOfLatitude, CorrectedArgumentOfLatitudeRate, CorrectedRightAscension, CorrectedRightAscensionRate, CorrectedInclination, CorrectedInclinationRate, VX, VY, VZ).

	short_period_elements_state(short_period_elements(SemiMajorAxis, Eccentricity, EccentricAnomaly, Inclination, RightAscension, RightAscensionRate, ArgumentOfPerigee, ArgumentOfPerigeeRate, TrueAnomaly, MeanMotion), X, Y, Z, VX, VY, VZ) :-
		short_period_corrected_state(SemiMajorAxis, Eccentricity, EccentricAnomaly, Inclination, RightAscension, RightAscensionRate, ArgumentOfPerigee, ArgumentOfPerigeeRate, TrueAnomaly, MeanMotion, X, Y, Z, VX, VY, VZ).

	eccentric_anomaly_rate(Eccentricity, EccentricAnomaly, MeanMotion, EccentricAnomalyRate) :-
		Denominator0 is 1.0 - Eccentricity * cos(EccentricAnomaly),
		positive_or_epsilon(Denominator0, 1.0e-12, Denominator),
		EccentricAnomalyRate is MeanMotion / Denominator.

	short_period_velocity(Radius, RadiusRate, ArgumentOfLatitude, ArgumentOfLatitudeRate, RightAscension, RightAscensionRate, Inclination, InclinationRate, VX, VY, VZ) :-
		CosU is cos(ArgumentOfLatitude),
		SinU is sin(ArgumentOfLatitude),
		CosRightAscension is cos(RightAscension),
		SinRightAscension is sin(RightAscension),
		CosInclination is cos(Inclination),
		SinInclination is sin(Inclination),
		Ux is CosRightAscension * CosU - SinRightAscension * CosInclination * SinU,
		Uy is SinRightAscension * CosU + CosRightAscension * CosInclination * SinU,
		Uz is SinInclination * SinU,
		DUxDU is -CosRightAscension * SinU - SinRightAscension * CosInclination * CosU,
		DUyDU is -SinRightAscension * SinU + CosRightAscension * CosInclination * CosU,
		DUzDU is SinInclination * CosU,
		DUxDRightAscension is -SinRightAscension * CosU - CosRightAscension * CosInclination * SinU,
		DUyDRightAscension is CosRightAscension * CosU - SinRightAscension * CosInclination * SinU,
		DUzDRightAscension is 0.0,
		DUxDInclination is SinRightAscension * SinInclination * SinU,
		DUyDInclination is -CosRightAscension * SinInclination * SinU,
		DUzDInclination is CosInclination * SinU,
		VX is RadiusRate * Ux + Radius * (DUxDU * ArgumentOfLatitudeRate + DUxDRightAscension * RightAscensionRate + DUxDInclination * InclinationRate),
		VY is RadiusRate * Uy + Radius * (DUyDU * ArgumentOfLatitudeRate + DUyDRightAscension * RightAscensionRate + DUyDInclination * InclinationRate),
		VZ is RadiusRate * Uz + Radius * (DUzDU * ArgumentOfLatitudeRate + DUzDRightAscension * RightAscensionRate + DUzDInclination * InclinationRate).

	eci_state_to_ecef(JulianDate, eci(XInertial, YInertial, ZInertial), eci(XVelocityInertial, YVelocityInertial, ZVelocityInertial), ecef(XEarthFixed, YEarthFixed, ZEarthFixed), ecef(XVelocityEarthFixed, YVelocityEarthFixed, ZVelocityEarthFixed)) :-
		earth_fixed_rotation_terms(JulianDate, CosTheta, SinTheta),
		XEarthFixed is CosTheta * XInertial + SinTheta * YInertial,
		YEarthFixed is -SinTheta * XInertial + CosTheta * YInertial,
		ZEarthFixed = ZInertial,
		RotatedXVelocity is CosTheta * XVelocityInertial + SinTheta * YVelocityInertial,
		RotatedYVelocity is -SinTheta * XVelocityInertial + CosTheta * YVelocityInertial,
		earth_rotation_rate(EarthRotationRate),
		XVelocityEarthFixed is RotatedXVelocity + EarthRotationRate * YEarthFixed,
		YVelocityEarthFixed is RotatedYVelocity - EarthRotationRate * XEarthFixed,
		ZVelocityEarthFixed = ZVelocityInertial.

	ecef_velocity_to_enu(geographic(Latitude, Longitude, _Height), VX, VY, VZ, East, North, Up) :-
		deg_to_rad(Latitude, LatitudeRadians),
		deg_to_rad(Longitude, LongitudeRadians),
		SinLatitude is sin(LatitudeRadians),
		CosLatitude is cos(LatitudeRadians),
		SinLongitude is sin(LongitudeRadians),
		CosLongitude is cos(LongitudeRadians),
		East is -SinLongitude * VX + CosLongitude * VY,
		North is -SinLatitude * CosLongitude * VX - SinLatitude * SinLongitude * VY + CosLatitude * VZ,
		Up is CosLatitude * CosLongitude * VX + CosLatitude * SinLongitude * VY + SinLatitude * VZ.

	orbital_to_eci(PerifocalX, PerifocalY, RightAscension, Inclination, ArgumentOfPerigee, X, Y, Z) :-
		CosOmega is cos(RightAscension),
		SinOmega is sin(RightAscension),
		CosI is cos(Inclination),
		SinI is sin(Inclination),
		CosOmegaSmall is cos(ArgumentOfPerigee),
		SinOmegaSmall is sin(ArgumentOfPerigee),
		X is (CosOmega * CosOmegaSmall - SinOmega * SinOmegaSmall * CosI) * PerifocalX + (-CosOmega * SinOmegaSmall - SinOmega * CosOmegaSmall * CosI) * PerifocalY,
		Y is (SinOmega * CosOmegaSmall + CosOmega * SinOmegaSmall * CosI) * PerifocalX + (-SinOmega * SinOmegaSmall + CosOmega * CosOmegaSmall * CosI) * PerifocalY,
		Z is (SinOmegaSmall * SinI) * PerifocalX + (CosOmegaSmall * SinI) * PerifocalY.

	eci_to_ecef(JulianDate, eci(XInertial, YInertial, ZInertial), ecef(XEarthFixed, YEarthFixed, ZEarthFixed)) :-
		earth_fixed_rotation_terms(JulianDate, CosTheta, SinTheta),
		XEarthFixed is CosTheta * XInertial + SinTheta * YInertial,
		YEarthFixed is -SinTheta * XInertial + CosTheta * YInertial,
		ZEarthFixed = ZInertial.

	earth_fixed_rotation_terms(JulianDate, CosTheta, SinTheta) :-
		gmst_angle(JulianDate, GreenwichMeanSiderealAngle),
		CosTheta is cos(GreenwichMeanSiderealAngle),
		SinTheta is sin(GreenwichMeanSiderealAngle).

	gmst_angle(JulianDate, Angle) :-
		CenturiesSinceJ2000 is (JulianDate - 2451545.0) / 36525.0,
		Degrees is 280.46061837 + 360.98564736629 * (JulianDate - 2451545.0) + 0.000387933 * CenturiesSinceJ2000 * CenturiesSinceJ2000 - (CenturiesSinceJ2000 * CenturiesSinceJ2000 * CenturiesSinceJ2000) / 38710000.0,
		NormalizedDegrees is Degrees - floor(Degrees / 360.0) * 360.0,
		deg_to_rad(NormalizedDegrees, Angle).

	ground_track_samples(CurrentJulianDate, EndJulianDate, StepDays, Model, TLE, [sample(DateTime, Geographic)| Samples]) :-
		CurrentJulianDate =< EndJulianDate + 1.0e-12,
		!,
		julian_date_time(CurrentJulianDate, DateTime),
		propagate_julian_date(TLE, CurrentJulianDate, Model, _ECI, _ECEF, Geographic),
		NextJulianDate is CurrentJulianDate + StepDays,
		ground_track_samples(NextJulianDate, EndJulianDate, StepDays, Model, TLE, Samples).
	ground_track_samples(_, _, _, _, _, []).

	date_time_julian_date(date_time(Year, Month, Day, Hours, Minutes, Seconds), JulianDate) :-
		valid_date_time(date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		date(JulianDayNumber, Year, Month, Day),
		DayFraction is (Hours + Minutes / 60.0 + Seconds / 3600.0) / 24.0,
		JulianDate is JulianDayNumber - 0.5 + DayFraction.

	julian_date_time(JulianDate, date_time(Year, Month, Day, Hours, Minutes, Seconds)) :-
		ShiftedJulianDate is JulianDate + 0.5,
		JulianDayNumber is floor(ShiftedJulianDate),
		DayFraction is ShiftedJulianDate - JulianDayNumber,
		date(JulianDayNumber, Year0, Month0, Day0),
		Hours0 is floor(DayFraction * 24.0),
		MinutesFraction is DayFraction * 24.0 - Hours0,
		Minutes0 is floor(MinutesFraction * 60.0),
		Seconds0 is (MinutesFraction * 60.0 - Minutes0) * 60.0,
		normalize_time_components(Year0, Month0, Day0, Hours0, Minutes0, Seconds0, Year, Month, Day, Hours, Minutes, Seconds).

	normalize_time_components(Year0, Month0, Day0, Hours0, Minutes0, Seconds0, Year, Month, Day, Hours, Minutes, Seconds) :-
		(	Seconds0 >= 59.9999995 ->
			Seconds1 = 0.0,
			Minutes1 is Minutes0 + 1
		;	Seconds1 = Seconds0,
			Minutes1 = Minutes0
		),
		(	Minutes1 >= 60 ->
			Minutes2 = 0,
			Hours1 is Hours0 + 1
		;	Minutes2 = Minutes1,
			Hours1 = Hours0
		),
		(	Hours1 >= 24 ->
			Hours2 = 0,
			date(CurrentJulianDayNumber, Year0, Month0, Day0),
			NextJulianDayNumber is CurrentJulianDayNumber + 1,
			date(NextJulianDayNumber, Year, Month, Day)
		;	Hours2 = Hours1,
			Year = Year0,
			Month = Month0,
			Day = Day0
		),
		Hours = Hours2,
		Minutes = Minutes2,
		Seconds = Seconds1.

	valid_date_time(date_time(Year, Month, Day, Hours, Minutes, Seconds)) :-
		valid_date(Year, Month, Day),
		valid_time(Hours, Minutes, 0),
		number(Seconds), Seconds >= 0.0, Seconds < 60.0.

	field_codes(Codes, Start, End, FieldCodes) :-
		Length is End - Start + 1,
		Start0 is Start - 1,
		drop(Start0, Codes, Tail),
		take(Length, Tail, FieldCodes).

	trim_codes(Codes0, Codes) :-
		trim_left_codes(Codes0, Codes1),
		trim_right_codes(Codes1, Codes).

	trim_right_codes(Codes0, Codes) :-
		reverse(Codes0, ReversedCodes0),
		trim_left_codes(ReversedCodes0, ReversedCodes),
		reverse(ReversedCodes, Codes).

	trim_left_codes([Code| Codes0], Codes) :-
		white_space_code(Code),
		!,
		trim_left_codes(Codes0, Codes).
	trim_left_codes(Codes, Codes).

	canonical_float_codes([0'.| Codes], [0'0, 0'.| Codes]) :-
		!.
	canonical_float_codes([Sign, 0'.| Codes], [Sign, 0'0, 0'.| Codes]) :-
		(	Sign =:= 0'+
		;	Sign =:= 0'-
		),
		!.
	canonical_float_codes(Codes, Codes).

	all_digit_codes([]).
	all_digit_codes([Code| Codes]) :-
		digit_code_value(Code, _),
		all_digit_codes(Codes).

	digit_code_value(Code, Value) :-
		Code >= 0'0,
		Code =< 0'9,
		Value is Code - 0'0.

	code_atom(Code, Atom) :-
		lowercase_code(Code, LowercaseCode),
		atom_codes(Atom, [LowercaseCode]).

	lowercase_code(Code, LowercaseCode) :-
		Code >= 0'A,
		Code =< 0'Z,
		!,
		LowercaseCode is Code + 32.
	lowercase_code(Code, Code).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

:- end_object.
