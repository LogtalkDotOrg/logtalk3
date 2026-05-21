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


:- object(nmea,
	implements(nmea_protocol),
	imports(options)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-05-21,
		comment is 'Parser for NMEA 0183 sentences with typed semantic decoding for selected GPS/GNSS sentence types.'
	]).

	:- uses(list, [
		append/3, length/2, reverse/2
	]).

	:- uses(reader, [
		file_to_codes/2, stream_to_codes/2
	]).

	parse(Source, Sentences) :-
		parse(Source, [], Sentences).

	parse(Source, UserOptions, Sentences) :-
		source_codes(Source, Codes),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(checksum(ChecksumPolicy), Options),
		^^option(unknown_type(UnknownTypePolicy), Options),
		phrase(sentences(Lines), Codes),
		lines_to_sentences(Lines, ChecksumPolicy, UnknownTypePolicy, Sentences).

	talker(nmea_sentence(Talker, _, _, _), Talker).

	sentence_type(nmea_sentence(_, Type, _, _), Type).

	fields(nmea_sentence(_, _, Fields, _), Fields).

	checksum(nmea_sentence(_, _, _, Checksum), Checksum).

	data(nmea_sentence(_, gga, Fields, _), Data) :-
		!,
		gga_data(Fields, Data).
	data(nmea_sentence(_, rmc, Fields, _), Data) :-
		!,
		rmc_data(Fields, Data).
	data(nmea_sentence(_, gsa, Fields, _), Data) :-
		!,
		gsa_data(Fields, Data).
	data(nmea_sentence(_, gsv, Fields, _), Data) :-
		!,
		gsv_data(Fields, Data).
	data(nmea_sentence(_, vtg, Fields, _), Data) :-
		!,
		vtg_data(Fields, Data).
	data(nmea_sentence(_, gll, Fields, _), Data) :-
		gll_data(Fields, Data).

	valid_option(checksum(Policy)) :-
		nonvar(Policy),
		valid_checksum_policy(Policy).
	valid_option(unknown_type(Policy)) :-
		nonvar(Policy),
		valid_unknown_type_policy(Policy).

	default_option(checksum(required)).
	default_option(unknown_type(keep)).

	valid_checksum_policy(required).
	valid_checksum_policy(optional).
	valid_checksum_policy(ignore).

	valid_unknown_type_policy(keep).
	valid_unknown_type_policy(error).

	source_codes(Source, _) :-
		var(Source),
		!,
		instantiation_error.
	source_codes(file(File), Codes) :-
		( 	var(File) ->
			instantiation_error
		; 	file_to_codes(File, Codes)
		),
		!.
	source_codes(stream(Stream), Codes) :-
		( 	var(Stream) ->
			instantiation_error
		; 	stream_to_codes(Stream, Codes)
		),
		!.
	source_codes(atom(Atom), Codes) :-
		( 	var(Atom) ->
			instantiation_error
		; 	atom_codes(Atom, Codes)
		),
		!.
	source_codes(chars(Chars), Codes) :-
		( 	var(Chars) ->
			instantiation_error
		; 	chars_to_codes(Chars, Codes)
		),
		!.
	source_codes(codes(Codes), Codes) :-
		( 	var(Codes) ->
			instantiation_error
		; 	true
		),
		!.
	source_codes(Source, _) :-
		domain_error(nmea_source, Source).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	sentences([Line| Lines]) -->
		blank_lines,
		line(Line),
		line_end,
		!,
		sentences(Lines).
	sentences([Line]) -->
		blank_lines,
		line(Line),
		eos,
		!.
	sentences([]) -->
		blank_lines,
		eos.

	blank_lines -->
		line_end,
		!,
		blank_lines.
	blank_lines -->
		[].

	line([Code| Codes]) -->
		[Code],
		{Code =\= 10, Code =\= 13},
		line_rest(Codes).

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

	lines_to_sentences([], _, _, []).
	lines_to_sentences([Line| Lines], ChecksumPolicy, UnknownTypePolicy, [Sentence| Sentences]) :-
		line_sentence(Line, ChecksumPolicy, UnknownTypePolicy, Sentence),
		lines_to_sentences(Lines, ChecksumPolicy, UnknownTypePolicy, Sentences).

	line_sentence(LineCodes, ChecksumPolicy, UnknownTypePolicy, Sentence) :-
		( 	parse_line(LineCodes, ChecksumPolicy, UnknownTypePolicy, Sentence) ->
			true
		; 	atom_codes(Line, LineCodes),
			domain_error(nmea_sentence, Line)
		).

	parse_line(LineCodes, ChecksumPolicy, UnknownTypePolicy, Sentence) :-
		split_payload(LineCodes, PayloadCodes, ProvidedChecksum, ComputedChecksum),
		accept_checksum(ChecksumPolicy, ProvidedChecksum, ComputedChecksum),
		split_field_codes(PayloadCodes, [IdentifierCodes| FieldCodeLists]),
		IdentifierCodes \== [],
		normalize_talker_and_type(IdentifierCodes, Talker, Type),
		accept_type(Talker, Type, UnknownTypePolicy),
		field_code_lists_atoms(FieldCodeLists, Fields),
		validate_sentence_fields(Type, Fields),
		Sentence = nmea_sentence(Talker, Type, Fields, checksum(ProvidedChecksum, ComputedChecksum)).

	split_payload([0'$| Codes], PayloadCodes, ProvidedChecksum, ComputedChecksum) :-
		phrase(payload_checksum(PayloadCodes, ProvidedChecksum), Codes),
		PayloadCodes \== [],
		compute_checksum(PayloadCodes, ComputedChecksum).

	payload_checksum(PayloadCodes, Checksum) -->
		payload_codes(PayloadCodes),
		[0'*],
		hex_pair(Checksum).
	payload_checksum(PayloadCodes, missing) -->
		payload_codes(PayloadCodes).

	payload_codes([Code| Codes]) -->
		[Code],
		{Code =\= 0'*},
		!,
		payload_codes(Codes).
	payload_codes([]) -->
		[].

	hex_pair(Checksum) -->
		[Code1, Code2],
		{
			hex_digit_value(Code1, _),
			hex_digit_value(Code2, _),
			uppercase_code(Code1, UpperCode1),
			uppercase_code(Code2, UpperCode2),
			atom_codes(Checksum, [UpperCode1, UpperCode2])
		}.

	compute_checksum(PayloadCodes, Checksum) :-
		compute_checksum(PayloadCodes, 0, Value),
		checksum_atom(Value, Checksum).

	compute_checksum([], Value, Value).
	compute_checksum([Code| Codes], Value0, Value) :-
		Value1 is xor(Value0, Code),
		compute_checksum(Codes, Value1, Value).

	checksum_atom(Value, Checksum) :-
		High is (Value >> 4) /\ 15,
		Low is Value /\ 15,
		hex_nibble_code(High, HighCode),
		hex_nibble_code(Low, LowCode),
		atom_codes(Checksum, [HighCode, LowCode]).

	hex_nibble_code(Value, Code) :-
		( 	Value < 10 ->
			Code is 0'0 + Value
		; 	Code is 0'A + Value - 10
		).

	hex_digit_value(Code, Value) :-
		( 	0'0 =< Code, Code =< 0'9 ->
			Value is Code - 0'0
		; 	0'A =< Code, Code =< 0'F ->
			Value is Code - 0'A + 10
		; 	0'a =< Code, Code =< 0'f ->
			Value is Code - 0'a + 10
		;	fail
		).

	uppercase_code(Code, UpperCode) :-
		( 	0'a =< Code, Code =< 0'z ->
			UpperCode is Code - 32
		; 	UpperCode = Code
		).

	accept_checksum(required, ProvidedChecksum, ComputedChecksum) :-
		ProvidedChecksum \== missing,
		ProvidedChecksum == ComputedChecksum.
	accept_checksum(optional, missing, _) :-
		!.
	accept_checksum(optional, ProvidedChecksum, ComputedChecksum) :-
		ProvidedChecksum == ComputedChecksum.
	accept_checksum(ignore, _, _).

	split_field_codes(Codes, Fields) :-
		split_field_codes(Codes, [], [], Fields).

	split_field_codes([], FieldCodes0, Fields0, Fields) :-
		reverse(FieldCodes0, FieldCodes),
		reverse([FieldCodes| Fields0], Fields).
	split_field_codes([0',| Codes], FieldCodes0, Fields0, Fields) :-
		!,
		reverse(FieldCodes0, FieldCodes),
		split_field_codes(Codes, [], [FieldCodes| Fields0], Fields).
	split_field_codes([Code| Codes], FieldCodes0, Fields0, Fields) :-
		split_field_codes(Codes, [Code| FieldCodes0], Fields0, Fields).

	field_code_lists_atoms([], []).
	field_code_lists_atoms([FieldCodes| FieldCodeLists], [Field| Fields]) :-
		atom_codes(Field, FieldCodes),
		field_code_lists_atoms(FieldCodeLists, Fields).

	normalize_talker_and_type(IdentifierCodes, proprietary, Type) :-
		lowercase_codes(IdentifierCodes, [0'p| Rest]),
		!,
		proprietary_type_codes(Rest),
		atom_codes(Type, Rest).
	normalize_talker_and_type(IdentifierCodes, Talker, Type) :-
		lowercase_codes(IdentifierCodes, [TalkerCode1, TalkerCode2, TypeCode1, TypeCode2, TypeCode3]),
		alpha_code(TalkerCode1),
		alpha_code(TalkerCode2),
		alpha_code(TypeCode1),
		alpha_code(TypeCode2),
		alpha_code(TypeCode3),
		atom_codes(Talker, [TalkerCode1, TalkerCode2]),
		atom_codes(Type, [TypeCode1, TypeCode2, TypeCode3]).

	alpha_code(Code) :-
		0'a =< Code,
		Code =< 0'z.

	alnum_code(Code) :-
		alpha_code(Code).
	alnum_code(Code) :-
		0'0 =< Code,
		Code =< 0'9.

	all_alnum_codes([]).
	all_alnum_codes([Code| Codes]) :-
		alnum_code(Code),
		all_alnum_codes(Codes).

	proprietary_type_codes([ManufacturerCode1, ManufacturerCode2, ManufacturerCode3| FormatterCodes]) :-
		alpha_code(ManufacturerCode1),
		alpha_code(ManufacturerCode2),
		alpha_code(ManufacturerCode3),
		all_alnum_codes(FormatterCodes).

	lowercase_codes([], []).
	lowercase_codes([Code| Codes], [LowercaseCode| LowercaseCodes]) :-
		( 	0'A =< Code, Code =< 0'Z ->
			LowercaseCode is Code + 32
		; 	LowercaseCode = Code
		),
		lowercase_codes(Codes, LowercaseCodes).

	accept_type(proprietary, _, _) :-
		!.
	accept_type(_, _, keep) :-
		!.
	accept_type(_, Type, error) :-
		supported_standard_type(Type).

	supported_standard_type(gga).
	supported_standard_type(rmc).
	supported_standard_type(gsa).
	supported_standard_type(gsv).
	supported_standard_type(vtg).
	supported_standard_type(gll).

	validate_sentence_fields(Type, Fields) :-
		( 	supported_standard_type(Type) ->
			validate_supported_sentence(Type, Fields)
		; 	true
		).

	validate_supported_sentence(gga, Fields) :-
		gga_data(Fields, _).
	validate_supported_sentence(rmc, Fields) :-
		rmc_data(Fields, _).
	validate_supported_sentence(gsa, Fields) :-
		gsa_data(Fields, _).
	validate_supported_sentence(gsv, Fields) :-
		gsv_data(Fields, _).
	validate_supported_sentence(vtg, Fields) :-
		vtg_data(Fields, _).
	validate_supported_sentence(gll, Fields) :-
		gll_data(Fields, _).

	gga_data(
		[TimeField, LatitudeField, LatitudeDirectionField, LongitudeField, LongitudeDirectionField,
		 QualityField, SatellitesField, HDOPField, AltitudeField, AltitudeUnitField,
		 GeoidField, GeoidUnitField, DifferentialAgeField, StationField],
		gga(Time, Coordinate, fix(FixQuality, SatellitesUsed, HDOP), altitude(Altitude, GeoidSeparation), dgps(DifferentialAge, StationId))
	) :-
		parse_utc_time(TimeField, Time),
		parse_coordinate(LatitudeField, LatitudeDirectionField, LongitudeField, LongitudeDirectionField, Coordinate),
		parse_fix_quality(QualityField, FixQuality),
		parse_satellites_used(SatellitesField, SatellitesUsed),
		optional_float(HDOPField, HDOP),
		parse_meter_value(AltitudeField, AltitudeUnitField, Altitude),
		parse_meter_value(GeoidField, GeoidUnitField, GeoidSeparation),
		optional_float(DifferentialAgeField, DifferentialAge),
		optional_atom(StationField, StationId).

	rmc_data(Fields, rmc(Time, Status, Coordinate, movement(SpeedKnots, CourseDegrees), Date, MagneticVariation, Mode)) :-
		Fields = [TimeField, StatusField, LatitudeField, LatitudeDirectionField, LongitudeField, LongitudeDirectionField,
			SpeedField, CourseField, DateField, VariationField, VariationDirectionField| Tail],
		( 	Tail = [ModeField] ->
			true
		; 	Tail = [],
			ModeField = ''
		),
		parse_utc_time(TimeField, Time),
		parse_status(StatusField, Status),
		parse_coordinate(LatitudeField, LatitudeDirectionField, LongitudeField, LongitudeDirectionField, Coordinate),
		optional_float(SpeedField, SpeedKnots),
		optional_float(CourseField, CourseDegrees),
		parse_date(DateField, Date),
		parse_magnetic_variation(VariationField, VariationDirectionField, MagneticVariation),
		parse_mode(ModeField, Mode).

	gsa_data(Fields, gsa(SelectionMode, FixType, SatelliteIds, dop(PDOP, HDOP, VDOP), SystemId)) :-
		Fields = [SelectionModeField, FixTypeField,
			SatelliteField1, SatelliteField2, SatelliteField3, SatelliteField4,
			SatelliteField5, SatelliteField6, SatelliteField7, SatelliteField8,
			SatelliteField9, SatelliteField10, SatelliteField11, SatelliteField12,
			PDOPField, HDOPField, VDOPField| Tail],
		( 	Tail = [SystemIdField] ->
			true
		; 	Tail = [],
			SystemIdField = ''
		),
		parse_selection_mode(SelectionModeField, SelectionMode),
		parse_fix_type(FixTypeField, FixType),
		satellite_ids([
			SatelliteField1, SatelliteField2, SatelliteField3, SatelliteField4,
			SatelliteField5, SatelliteField6, SatelliteField7, SatelliteField8,
			SatelliteField9, SatelliteField10, SatelliteField11, SatelliteField12
		], SatelliteIds),
		optional_float(PDOPField, PDOP),
		optional_float(HDOPField, HDOP),
		optional_float(VDOPField, VDOP),
		parse_system_id(SystemIdField, SystemId).

	gsv_data([MessageCountField, MessageNumberField, SatellitesInViewField| SatelliteFields], gsv(MessageCount, MessageNumber, SatellitesInView, Satellites)) :-
		parse_message_count(MessageCountField, MessageCount),
		parse_message_number(MessageNumberField, MessageCount, MessageNumber),
		parse_satellites_in_view(SatellitesInViewField, SatellitesInView),
		satellite_entries(SatelliteFields, Satellites),
		validate_gsv_cardinality(MessageCount, MessageNumber, SatellitesInView, Satellites).

	vtg_data(Fields, vtg(track(TrueCourseDegrees, MagneticCourseDegrees), speed(SpeedKnots, SpeedKph), Mode)) :-
		Fields = [TrueCourseField, TrueMarkerField, MagneticCourseField, MagneticMarkerField,
			SpeedKnotsField, SpeedKnotsMarkerField, SpeedKphField, SpeedKphMarkerField| Tail],
		( 	Tail = [ModeField] ->
			true
		; 	Tail = [],
			ModeField = ''
		),
		parse_tagged_float(TrueCourseField, TrueMarkerField, 'T', TrueCourseDegrees),
		parse_tagged_float(MagneticCourseField, MagneticMarkerField, 'M', MagneticCourseDegrees),
		parse_tagged_float(SpeedKnotsField, SpeedKnotsMarkerField, 'N', SpeedKnots),
		parse_tagged_float(SpeedKphField, SpeedKphMarkerField, 'K', SpeedKph),
		parse_mode(ModeField, Mode).

	gll_data(Fields, gll(Coordinate, Time, Status, Mode)) :-
		Fields = [LatitudeField, LatitudeDirectionField, LongitudeField, LongitudeDirectionField, TimeField, StatusField| Tail],
		( 	Tail = [ModeField] ->
			true
		; 	Tail = [],
			ModeField = ''
		),
		parse_coordinate(LatitudeField, LatitudeDirectionField, LongitudeField, LongitudeDirectionField, Coordinate),
		parse_utc_time(TimeField, Time),
		parse_status(StatusField, Status),
		parse_mode(ModeField, Mode).

	parse_utc_time('', missing) :-
		!.
	parse_utc_time(Field, utc_time(Hour, Minute, Second, Fraction)) :-
		atom_codes(Field, Codes),
		split_decimal_codes(Codes, WholeCodes, FractionCodes),
		WholeCodes = [HourCode1, HourCode2, MinuteCode1, MinuteCode2, SecondCode1, SecondCode2],
		integer_codes([HourCode1, HourCode2], Hour),
		integer_codes([MinuteCode1, MinuteCode2], Minute),
		integer_codes([SecondCode1, SecondCode2], Second),
		( 	time::valid(Hour, Minute, Second) ->
			true
		; 	Second =:= 60,
			time::valid(Hour, Minute, 59)
		),
		fraction_codes(FractionCodes, Fraction).

	split_decimal_codes(Codes, WholeCodes, FractionCodes) :-
		( 	append(WholeCodes, [0'.| FractionCodes], Codes) ->
			FractionCodes \== []
		; 	WholeCodes = Codes,
			FractionCodes = []
		).

	fraction_codes([], fraction(0, 1)) :-
		!.
	fraction_codes(Codes, fraction(Numerator, Denominator)) :-
		integer_codes(Codes, RawNumerator),
		length(Codes, Digits),
		power_of_ten(Digits, RawDenominator),
		normalize_fraction(RawNumerator, RawDenominator, Numerator, Denominator).

	power_of_ten(0, 1) :-
		!.
	power_of_ten(Power, Value) :-
		Power > 0,
		NextPower is Power - 1,
		power_of_ten(NextPower, NextValue),
		Value is NextValue * 10.

	normalize_fraction(0, _, 0, 1) :-
		!.
	normalize_fraction(Numerator0, Denominator0, Numerator, Denominator) :-
		gcd(Numerator0, Denominator0, GCD),
		Numerator is Numerator0 // GCD,
		Denominator is Denominator0 // GCD.

	gcd(A, 0, A) :-
		!.
	gcd(A, B, GCD) :-
		Remainder is A mod B,
		gcd(B, Remainder, GCD).

	parse_date('', missing) :-
		!.
	parse_date(Field, date(Year, Month, Day)) :-
		atom_codes(Field, [DayCode1, DayCode2, MonthCode1, MonthCode2, YearCode1, YearCode2]),
		integer_codes([DayCode1, DayCode2], Day),
		integer_codes([MonthCode1, MonthCode2], Month),
		integer_codes([YearCode1, YearCode2], Year2),
		( 	Year2 >= 80 ->
			Year is 1900 + Year2
		; 	Year is 2000 + Year2
		),
		date::valid(Year, Month, Day).

	parse_coordinate('', '', '', '', missing) :-
		!.
	parse_coordinate(LatitudeField, LatitudeDirectionField, LongitudeField, LongitudeDirectionField, geographic(Latitude, Longitude)) :-
		parse_latitude(LatitudeField, LatitudeDirectionField, Latitude),
		parse_longitude(LongitudeField, LongitudeDirectionField, Longitude).

	parse_latitude(Field, DirectionField, Latitude) :-
		parse_coordinate_value(Field, 2, Value),
		direction_sign(latitude, DirectionField, Sign),
		Latitude is Value * Sign,
		Latitude >= -90.0,
		Latitude =< 90.0.

	parse_longitude(Field, DirectionField, Longitude) :-
		parse_coordinate_value(Field, 3, Value),
		direction_sign(longitude, DirectionField, Sign),
		Longitude is Value * Sign,
		Longitude >= -180.0,
		Longitude =< 180.0.

	parse_coordinate_value(Field, DegreeDigits, Value) :-
		atom_codes(Field, Codes),
		length(DegreeCodes, DegreeDigits),
		append(DegreeCodes, MinuteCodes, Codes),
		MinuteCodes \== [],
		integer_codes(DegreeCodes, Degrees),
		unsigned_decimal_codes(MinuteCodes, MinutesValue),
		MinutesValue >= 0.0,
		MinutesValue < 60.0,
		Value is Degrees + MinutesValue / 60.0.

	direction_sign(latitude, Field, Sign) :-
		normalized_char_atom(Field, Direction),
		( 	Direction == n ->
			Sign = 1.0
		; 	Direction == s,
			Sign = -1.0
		).
	direction_sign(longitude, Field, Sign) :-
		normalized_char_atom(Field, Direction),
		( 	Direction == e ->
			Sign = 1.0
		; 	Direction == w,
			Sign = -1.0
		).

	parse_fix_quality('', missing) :-
		!.
	parse_fix_quality(Field, FixQuality) :-
		integer_field(Field, Code),
		fix_quality(Code, FixQuality),
		!.

	fix_quality(0, invalid).
	fix_quality(1, gps).
	fix_quality(2, dgps).
	fix_quality(3, pps).
	fix_quality(4, rtk).
	fix_quality(5, float_rtk).
	fix_quality(6, estimated).
	fix_quality(7, manual).
	fix_quality(8, simulation).
	fix_quality(Code, unknown(Code)).

	parse_status('', missing) :-
		!.
	parse_status(Field, Status) :-
		normalized_char_atom(Field, Code),
		( 	Code == a ->
			Status = active
		; 	Code == v ->
			Status = void
		; 	Status = unknown(Code)
		).

	parse_selection_mode('', missing) :-
		!.
	parse_selection_mode(Field, SelectionMode) :-
		normalized_char_atom(Field, Code),
		( 	Code == a ->
			SelectionMode = automatic
		; 	Code == m ->
			SelectionMode = manual
		; 	SelectionMode = unknown(Code)
		).

	parse_fix_type('', missing) :-
		!.
	parse_fix_type(Field, FixType) :-
		integer_field(Field, Code),
		( 	Code =:= 1 ->
			FixType = no_fix
		; 	Code =:= 2 ->
			FixType = fix_2d
		; 	Code =:= 3 ->
			FixType = fix_3d
		; 	FixType = unknown(Code)
		).

	parse_mode('', missing) :-
		!.
	parse_mode(Field, Mode) :-
		normalized_char_atom(Field, Code),
		mode_atom(Code, Mode),
		!.

	mode_atom(a, autonomous).
	mode_atom(d, differential).
	mode_atom(e, estimated).
	mode_atom(m, manual).
	mode_atom(s, simulator).
	mode_atom(n, data_not_valid).
	mode_atom(Code, unknown(Code)).

	parse_system_id('', missing) :-
		!.
	parse_system_id(Field, SystemId) :-
		integer_field(Field, Code),
		system_id(Code, SystemId),
		!.

	system_id(1, gps).
	system_id(2, glonass).
	system_id(3, galileo).
	system_id(4, beidou).
	system_id(5, qzss).
	system_id(6, navic).
	system_id(Code, unknown(Code)).

	parse_magnetic_variation('', '', missing) :-
		!.
	parse_magnetic_variation(ValueField, DirectionField, magnetic_variation(Value, Direction)) :-
		float_field(ValueField, Value),
		normalized_char_atom(DirectionField, Code),
		( 	Code == e ->
			Direction = east
		; 	Code == w,
			Direction = west
		).

	parse_meter_value('', '', missing) :-
		!.
	parse_meter_value(ValueField, UnitField, Value) :-
		signed_float_field(ValueField, Value),
		normalized_char_atom(UnitField, Unit),
		Unit == m.

	parse_tagged_float('', '', _, missing) :-
		!.
	parse_tagged_float(ValueField, MarkerField, Marker, Value) :-
		float_field(ValueField, Value),
		char_code(Marker, MarkerCode),
		uppercase_code(MarkerCode, UppercaseMarkerCode),
		uppercase_char_atom(MarkerField, UppercaseMarkerCode).

	uppercase_char_atom(Field, Code) :-
		char_code(Field, FieldCode),
		uppercase_code(FieldCode, Code).

	normalized_char_atom(Field, Atom) :-
		char_code(Field, Code),
		( 	0'A =< Code, Code =< 0'Z ->
			LowercaseCode is Code + 32
		; 	LowercaseCode = Code
		),
		char_code(Atom, LowercaseCode).

	optional_integer('', missing) :-
		!.
	optional_integer(Field, Integer) :-
		integer_field(Field, Integer).

	optional_float('', missing) :-
		!.
	optional_float(Field, Float) :-
		float_field(Field, Float).

	optional_atom('', missing) :-
		!.
	optional_atom(Field, Field).

	parse_satellites_used(Field, SatellitesUsed) :-
		optional_integer(Field, SatellitesUsed),
		valid_optional_integer_range(SatellitesUsed, 0, 99).

	parse_message_count(Field, MessageCount) :-
		integer_field(Field, MessageCount),
		valid_integer_range(MessageCount, 1, 99).

	parse_message_number(Field, MessageCount, MessageNumber) :-
		integer_field(Field, MessageNumber),
		MessageNumber >= 1,
		MessageNumber =< MessageCount.

	parse_satellites_in_view(Field, SatellitesInView) :-
		integer_field(Field, SatellitesInView),
		valid_integer_range(SatellitesInView, 0, 99).

	validate_gsv_cardinality(1, 1, 0, []) :-
		!.
	validate_gsv_cardinality(MessageCount, MessageNumber, SatellitesInView, Satellites) :-
		expected_gsv_message_count(SatellitesInView, MessageCount),
		length(Satellites, SatelliteCount),
		PreviousSatelliteCount is (MessageNumber - 1) * 4,
		PreviousSatelliteCount < SatellitesInView,
		RemainingSatelliteCount is SatellitesInView - PreviousSatelliteCount,
		( 	RemainingSatelliteCount > 4 ->
			ExpectedSatelliteCount = 4
		; 	ExpectedSatelliteCount = RemainingSatelliteCount
		),
		SatelliteCount =:= ExpectedSatelliteCount.

	expected_gsv_message_count(0, 1) :-
		!.
	expected_gsv_message_count(SatellitesInView, MessageCount) :-
		SatellitesInView > 0,
		MessageCount is ((SatellitesInView - 1) // 4) + 1.

	valid_optional_integer_range(missing, _, _) :-
		!.
	valid_optional_integer_range(Integer, Minimum, Maximum) :-
		valid_integer_range(Integer, Minimum, Maximum).

	valid_integer_range(Integer, Minimum, Maximum) :-
		integer(Integer),
		Integer >= Minimum,
		Integer =< Maximum.

	integer_field(Field, Integer) :-
		atom_codes(Field, Codes),
		integer_codes(Codes, Integer).

	integer_codes(Codes, Integer) :-
		Codes \== [],
		digit_codes(Codes),
		number_codes(Integer, Codes),
		integer(Integer).

	float_field(Field, Float) :-
		atom_codes(Field, Codes),
		unsigned_decimal_codes(Codes, Float).

	signed_float_field(Field, Float) :-
		atom_codes(Field, Codes),
		signed_decimal_codes(Codes, Float).

	unsigned_decimal_codes(Codes, Number) :-
		( 	append(IntegerCodes, [0'.| FractionCodes], Codes),
			IntegerCodes \== [],
			FractionCodes \== [],
			digit_codes(IntegerCodes),
			digit_codes(FractionCodes) ->
			true
		; 	digit_codes(Codes)
		),
		number_codes(Number, Codes).

	signed_decimal_codes([0'-| Codes], Number) :-
		!,
		unsigned_decimal_codes(Codes, UnsignedNumber),
		Number is -UnsignedNumber.
	signed_decimal_codes(Codes, Number) :-
		unsigned_decimal_codes(Codes, Number).

	digit_codes([Code| Codes]) :-
		digit_code(Code),
		digit_codes(Codes).
	digit_codes([]).

	digit_code(Code) :-
		0'0 =< Code,
		Code =< 0'9.

	satellite_ids([], []).
	satellite_ids([''| Fields], SatelliteIds) :-
		!,
		satellite_ids(Fields, SatelliteIds).
	satellite_ids([Field| Fields], [SatelliteId| SatelliteIds]) :-
		integer_field(Field, SatelliteId),
		SatelliteId > 0,
		satellite_ids(Fields, SatelliteIds).

	satellite_entries([], []).
	satellite_entries([PRNField, ElevationField, AzimuthField, SnrField| Fields], [satellite(PRN, Elevation, Azimuth, Snr)| Satellites]) :-
		integer_field(PRNField, PRN),
		PRN > 0,
		integer_field(ElevationField, Elevation),
		valid_integer_range(Elevation, 0, 90),
		integer_field(AzimuthField, Azimuth),
		valid_integer_range(Azimuth, 0, 359),
		optional_integer(SnrField, Snr),
		valid_optional_integer_range(Snr, 0, 99),
		satellite_entries(Fields, Satellites).

:- end_object.
