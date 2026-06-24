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


:- object(ccsds_ccs(_CalendarVariant_, _FractionOctets_),
	implements(ccsds_time_code_protocol)).

	:- info([
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2026-06-24,
		comment is 'CCSDS calendar segmented time code parser and generator.',
		parameters is [
			'CalendarVariant' - 'Calendar representation. Supported values are ``calendar`` and ``day_of_year``.',
			'FractionOctets' - 'Number of BCD fraction octets. Supported values are non-negative integers.'
		]
	]).

	:- public(year/2).
	:- mode(year(+compound, -integer), one).
	:- info(year/2, [
		comment is 'Extracts the year value from a CCS time term.',
		argnames is ['TimeCode', 'Year']
	]).

	:- public(day_of_year/2).
	:- mode(day_of_year(+compound, -integer), zero_or_one).
	:- info(day_of_year/2, [
		comment is 'Extracts the day of year from an ordinal CCS time term.',
		argnames is ['TimeCode', 'DayOfYear']
	]).

	:- public(month/2).
	:- mode(month(+compound, -integer), zero_or_one).
	:- info(month/2, [
		comment is 'Extracts the month from a calendar CCS time term.',
		argnames is ['TimeCode', 'Month']
	]).

	:- public(day/2).
	:- mode(day(+compound, -integer), zero_or_one).
	:- info(day/2, [
		comment is 'Extracts the day of month from a calendar CCS time term.',
		argnames is ['TimeCode', 'Day']
	]).

	:- public(time_of_day/4).
	:- mode(time_of_day(+compound, -integer, -integer, -number), one).
	:- info(time_of_day/4, [
		comment is 'Extracts the hour, minute, and seconds-with-fraction from a CCS time term.',
		argnames is ['TimeCode', 'Hour', 'Minute', 'Seconds']
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(reader, [
		file_to_bytes/2, stream_to_bytes/2
	]).

	parse(Source, _) :-
		var(Source),
		instantiation_error.
	parse(file(File), TimeCode) :-
		!,
		file_to_bytes(File, Bytes),
		parse_bytes(Bytes, TimeCode).
	parse(stream(Stream), TimeCode) :-
		!,
		stream_to_bytes(Stream, Bytes),
		parse_bytes(Bytes, TimeCode).
	parse(bytes(Bytes), TimeCode) :-
		!,
		parse_bytes(Bytes, TimeCode).
	parse(Source, _) :-
		domain_error(ccsds_time_code_source, Source).

	generate(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate(file(File), TimeCode) :-
		!,
		generate(TimeCode, Bytes, []),
		open(File, write, Stream, [type(binary)]),
		(	catch(
				write_bytes(Bytes, Stream),
				Error,
				(	catch(close(Stream), _, true),
					throw(Error)
				)
			) ->
			close(Stream)
		; 	catch(close(Stream), _, true),
			fail
		).
	generate(stream(Stream), TimeCode) :-
		!,
		generate(TimeCode, Bytes, []),
		write_bytes(Bytes, Stream).
	generate(bytes(Bytes), TimeCode) :-
		!,
		generate(TimeCode, Bytes, []).
	generate(Sink, _) :-
		domain_error(ccsds_time_code_sink, Sink).

	generate(TimeCode, _, _) :-
		var(TimeCode),
		instantiation_error.
	generate(TimeCode, Bytes, Tail) :-
		valid(TimeCode),
		!,
		encode_time(TimeCode, Bytes, Tail).
	generate(TimeCode, _, _) :-
		domain_error(ccsds_time_code_term, TimeCode).

	valid(TimeCode) :-
		valid_parameters,
		(	TimeCode = ccs_calendar_time(Year, Month, Day, Hour, Minute, Second, Fraction) ->
			_CalendarVariant_ == calendar,
			valid_calendar_date(Year, Month, Day),
			valid_clock_time(Hour, Minute, Second),
			valid_fraction(Fraction)
		;	TimeCode = ccs_ordinal_time(Year, DayOfYear, Hour, Minute, Second, Fraction) ->
			_CalendarVariant_ == day_of_year,
			valid_ordinal_date(Year, DayOfYear),
			valid_clock_time(Hour, Minute, Second),
			valid_fraction(Fraction)
		;	fail
		).

	format(ccs).

	epoch(none).

	unix_seconds(TimeCode, Seconds) :-
		(	var(TimeCode) ->
			instantiation_error
		;	valid(TimeCode) ->
			fraction_scale(Scale),
			time_code_to_calendar(TimeCode, Year, Month, Day, Hour, Minute, Second, Fraction),
			days_from_civil(Year, Month, Day, Days),
			Seconds is Days * 86400 + Hour * 3600 + Minute * 60 + Second + Fraction / Scale
		;	domain_error(ccs_time_code_term, TimeCode)
		).

	from_unix_seconds(Seconds, TimeCode) :-
		(	var(Seconds) ->
			instantiation_error
		;	number(Seconds) ->
			fraction_scale(Scale),
			UnitsPerDay is 86400 * Scale,
			rounded_units(Seconds, Scale, TotalUnits),
			Days is floor(TotalUnits / UnitsPerDay),
			DayUnits is TotalUnits - Days * UnitsPerDay,
			civil_from_days(Days, Year, Month, Day),
			(	valid_year(Year) ->
				decompose_day_units(DayUnits, Scale, Hour, Minute, Second, Fraction),
				calendar_to_time_code(Year, Month, Day, Hour, Minute, Second, Fraction, TimeCode),
				(	valid(TimeCode) ->
					true
				;	domain_error(ccs_time_code_unix_seconds, Seconds)
				)
			;	domain_error(ccs_time_code_unix_seconds, Seconds)
			)
		;	domain_error(ccs_time_code_unix_seconds, Seconds)
		).

	year(ccs_calendar_time(Year, _, _, _, _, _, _), Year).
	year(ccs_ordinal_time(Year, _, _, _, _, _), Year).

	day_of_year(ccs_ordinal_time(_, DayOfYear, _, _, _, _), DayOfYear).

	month(ccs_calendar_time(_, Month, _, _, _, _, _), Month).

	day(ccs_calendar_time(_, _, Day, _, _, _, _), Day).

	time_of_day(ccs_calendar_time(_, _, _, Hour, Minute, Second, Fraction), Hour, Minute, Seconds) :-
		fraction_scale(Scale),
		Seconds is Second + Fraction / Scale.
	time_of_day(ccs_ordinal_time(_, _, Hour, Minute, Second, Fraction), Hour, Minute, Seconds) :-
		fraction_scale(Scale),
		Seconds is Second + Fraction / Scale.

	parse_bytes(Bytes, TimeCode) :-
		expected_length(ExpectedLength),
		(	valid(list(byte, ExpectedLength), Bytes),
			decode_time(Bytes, TimeCode) ->
			true
		;	domain_error(ccs_time_code_byte_sequence, Bytes)
		).

	expected_length(ExpectedLength) :-
		ExpectedLength is 7 + _FractionOctets_.

	decode_time(Bytes, TimeCode) :-
		decode_bcd_octets(2, Bytes, Rest, Year),
		(	_CalendarVariant_ == calendar ->
			decode_bcd_octets(1, Rest, Rest1, Month),
			decode_bcd_octets(1, Rest1, Rest2, Day),
			decode_clock_fields(Rest2, Rest3, Hour, Minute, Second),
			decode_fraction(Rest3, Fraction),
			TimeCode = ccs_calendar_time(Year, Month, Day, Hour, Minute, Second, Fraction)
		;	_CalendarVariant_ == day_of_year,
			decode_bcd_octets(2, Rest, Rest1, DayOfYear),
			decode_clock_fields(Rest1, Rest2, Hour, Minute, Second),
			decode_fraction(Rest2, Fraction),
			TimeCode = ccs_ordinal_time(Year, DayOfYear, Hour, Minute, Second, Fraction)
		),
		valid(TimeCode).

	encode_time(TimeCode, Bytes, Tail) :-
		(	TimeCode = ccs_calendar_time(Year, Month, Day, Hour, Minute, Second, Fraction) ->
			encode_bcd_octets(2, Year, Bytes, Rest),
			encode_bcd_octets(1, Month, Rest, Rest1),
			encode_bcd_octets(1, Day, Rest1, Rest2),
			encode_clock_fields(Hour, Minute, Second, Rest2, Rest3),
			encode_fraction(Fraction, Rest3, Tail)
		;	TimeCode = ccs_ordinal_time(Year, DayOfYear, Hour, Minute, Second, Fraction),
			encode_bcd_octets(2, Year, Bytes, Rest),
			encode_bcd_octets(2, DayOfYear, Rest, Rest1),
			encode_clock_fields(Hour, Minute, Second, Rest1, Rest2),
			encode_fraction(Fraction, Rest2, Tail)
		).

	decode_clock_fields(Bytes, Rest, Hour, Minute, Second) :-
		decode_bcd_octets(1, Bytes, Rest1, Hour),
		decode_bcd_octets(1, Rest1, Rest2, Minute),
		decode_bcd_octets(1, Rest2, Rest, Second).

	encode_clock_fields(Hour, Minute, Second, Bytes, Tail) :-
		encode_bcd_octets(1, Hour, Bytes, Rest),
		encode_bcd_octets(1, Minute, Rest, Rest1),
		encode_bcd_octets(1, Second, Rest1, Tail).

	decode_fraction(Bytes, Fraction) :-
		decode_bcd_octets(_FractionOctets_, Bytes, [], Fraction).

	encode_fraction(Fraction, Bytes, Tail) :-
		encode_bcd_octets(_FractionOctets_, Fraction, Bytes, Tail).

	decode_bcd_octets(0, Bytes, Bytes, 0) :-
		!.
	decode_bcd_octets(Octets, Bytes, Rest, Value) :-
		decode_bcd_octets(Octets, Bytes, Rest, 0, Value).

	decode_bcd_octets(0, Bytes, Bytes, Value, Value) :-
		!.
	decode_bcd_octets(Octets, [Byte| Bytes], Rest, Acc, Value) :-
		decode_bcd_octet(Byte, OctetValue),
		Acc1 is Acc * 100 + OctetValue,
		Octets1 is Octets - 1,
		decode_bcd_octets(Octets1, Bytes, Rest, Acc1, Value).

	encode_bcd_octets(0, 0, Tail, Tail) :-
		!.
	encode_bcd_octets(Octets, Value, Bytes, Tail) :-
		max_bcd_value(Octets, Max),
		Value >= 0,
		Value =< Max,
		Octets1 is Octets - 1,
		power100(Octets1, Divisor),
		OctetValue is Value // Divisor,
		RestValue is Value mod Divisor,
		encode_bcd_octet(OctetValue, Byte),
		Bytes = [Byte| Rest],
		encode_bcd_octets(Octets1, RestValue, Rest, Tail).

	decode_bcd_octet(Byte, Value) :-
		Tens is (Byte >> 4) /\ 0x0F,
		Ones is Byte /\ 0x0F,
		Tens =< 9,
		Ones =< 9,
		Value is Tens * 10 + Ones.

	encode_bcd_octet(Value, Byte) :-
		Value >= 0,
		Value =< 99,
		Tens is Value // 10,
		Ones is Value mod 10,
		Byte is (Tens << 4) \/ Ones.

	decompose_day_units(DayUnits, Scale, Hour, Minute, Second, Fraction) :-
		UnitsPerHour is 3600 * Scale,
		UnitsPerMinute is 60 * Scale,
		Hour is DayUnits // UnitsPerHour,
		HourUnits is DayUnits mod UnitsPerHour,
		Minute is HourUnits // UnitsPerMinute,
		MinuteUnits is HourUnits mod UnitsPerMinute,
		Second is MinuteUnits // Scale,
		Fraction is MinuteUnits mod Scale.

	time_code_to_calendar(ccs_calendar_time(Year, Month, Day, Hour, Minute, Second, Fraction), Year, Month, Day, Hour, Minute, Second, Fraction).
	time_code_to_calendar(ccs_ordinal_time(Year, DayOfYear, Hour, Minute, Second, Fraction), Year, Month, Day, Hour, Minute, Second, Fraction) :-
		calendar_from_ordinal(Year, DayOfYear, Month, Day).

	calendar_to_time_code(Year, Month, Day, Hour, Minute, Second, Fraction, ccs_calendar_time(Year, Month, Day, Hour, Minute, Second, Fraction)) :-
		_CalendarVariant_ == calendar,
		!.
	calendar_to_time_code(Year, Month, Day, Hour, Minute, Second, Fraction, ccs_ordinal_time(Year, DayOfYear, Hour, Minute, Second, Fraction)) :-
		ordinal_from_calendar(Year, Month, Day, DayOfYear).

	valid_parameters :-
		valid_calendar_variant(_CalendarVariant_),
		integer(_FractionOctets_),
		_FractionOctets_ >= 0.

	valid_calendar_variant(Variant) :-
		(	Variant == calendar ->
			true
		;	Variant == day_of_year
		).

	valid_calendar_date(Year, Month, Day) :-
		valid_year(Year),
		integer(Month),
		Month >= 1,
		Month =< 12,
		integer(Day),
		Day >= 1,
		days_in_month(Month, Year, MaxDay),
		Day =< MaxDay.

	valid_ordinal_date(Year, DayOfYear) :-
		valid_year(Year),
		integer(DayOfYear),
		DayOfYear >= 1,
		max_day_of_year(Year, MaxDayOfYear),
		DayOfYear =< MaxDayOfYear.

	valid_year(Year) :-
		integer(Year),
		Year >= 0,
		Year =< 9999.

	valid_clock_time(Hour, Minute, Second) :-
		integer(Hour),
		Hour >= 0,
		Hour =< 23,
		integer(Minute),
		Minute >= 0,
		Minute =< 59,
		integer(Second),
		Second >= 0,
		Second =< 59.

	valid_fraction(Fraction) :-
		integer(Fraction),
		Fraction >= 0,
		max_fraction(MaxFraction),
		Fraction =< MaxFraction.

	max_fraction(MaxFraction) :-
		fraction_scale(Scale),
		MaxFraction is Scale - 1.

	rounded_units(Seconds, Scale, Units) :-
		ScaledSeconds is Seconds * Scale,
		(	integer(ScaledSeconds) ->
			Units = ScaledSeconds
		;	Units is round(ScaledSeconds)
		).

	fraction_scale(Scale) :-
		Digits is _FractionOctets_ * 2,
		power10(Digits, Scale).

	power10(0, 1) :-
		!.
	power10(Exponent, Value) :-
		Exponent > 0,
		Exponent1 is Exponent - 1,
		power10(Exponent1, Partial),
		Value is Partial * 10.

	power100(0, 1) :-
		!.
	power100(Exponent, Value) :-
		Exponent > 0,
		Exponent1 is Exponent - 1,
		power100(Exponent1, Partial),
		Value is Partial * 100.

	max_bcd_value(0, 0) :-
		!.
	max_bcd_value(Octets, Max) :-
		Digits is Octets * 2,
		power10(Digits, Scale),
		Max is Scale - 1.

	ordinal_from_calendar(Year, Month, Day, DayOfYear) :-
		ordinal_from_calendar(Year, 1, Month, Day, 0, DayOfYear).

	ordinal_from_calendar(_, Month, Month, Day, Acc, DayOfYear) :-
		!,
		DayOfYear is Acc + Day.
	ordinal_from_calendar(Year, CurrentMonth, Month, Day, Acc, DayOfYear) :-
		days_in_month(CurrentMonth, Year, Days),
		Acc1 is Acc + Days,
		CurrentMonth1 is CurrentMonth + 1,
		ordinal_from_calendar(Year, CurrentMonth1, Month, Day, Acc1, DayOfYear).

	calendar_from_ordinal(Year, DayOfYear, Month, Day) :-
		calendar_from_ordinal(Year, DayOfYear, 1, Month, Day).

	calendar_from_ordinal(Year, DayOfYear, Month0, Month, Day) :-
		days_in_month(Month0, Year, Days),
		(	DayOfYear =< Days ->
			Month = Month0,
			Day = DayOfYear
		;	DayOfYear1 is DayOfYear - Days,
			Month1 is Month0 + 1,
			calendar_from_ordinal(Year, DayOfYear1, Month1, Month, Day)
		).

	max_day_of_year(Year, 366) :-
		leap_year(Year),
		!.
	max_day_of_year(_, 365).

	days_in_month(1, _, 31).
	days_in_month(2, Year, 29) :-
		(	leap_year(Year) ->
			Days = 29
		;	Days = 28
		).
	days_in_month(3, _, 31).
	days_in_month(4, _, 30).
	days_in_month(5, _, 31).
	days_in_month(6, _, 30).
	days_in_month(7, _, 31).
	days_in_month(8, _, 31).
	days_in_month(9, _, 30).
	days_in_month(10, _, 31).
	days_in_month(11, _, 30).
	days_in_month(12, _, 31).

	leap_year(Year) :-
		0 =:= Year mod 400,
		!.
	leap_year(Year) :-
		0 =:= Year mod 4,
		Year mod 100 =\= 0.

	days_from_civil(Year, Month, Day, Days) :-
		(	Month =< 2 ->
			Year1 is Year - 1
		;	Year1 = Year
		),
		Era is floor(Year1 / 400),
		YearOfEra is Year1 - Era * 400,
		(	Month > 2 ->
			MonthPrime is Month - 3
		;	MonthPrime is Month + 9
		),
		DayOfYear is (153 * MonthPrime + 2) // 5 + Day - 1,
		DayOfEra is YearOfEra * 365 + YearOfEra // 4 - YearOfEra // 100 + DayOfYear,
		Days is Era * 146097 + DayOfEra - 719468.

	civil_from_days(Days, Year, Month, Day) :-
		Z is Days + 719468,
		(	Z < 0 ->
			Offset = 146096
		;	Offset = 0
		),
		Era is floor((Z - Offset) / 146097),
		DayOfEra is Z - Era * 146097,
		YearOfEra is (DayOfEra - DayOfEra // 1460 + DayOfEra // 36524 - DayOfEra // 146096) // 365,
		Year0 is YearOfEra + Era * 400,
		DayOfYear is DayOfEra - (365 * YearOfEra + YearOfEra // 4 - YearOfEra // 100),
		MonthPrime is (5 * DayOfYear + 2) // 153,
		Day is DayOfYear - (153 * MonthPrime + 2) // 5 + 1,
		(	MonthPrime < 10 ->
			Month is MonthPrime + 3
		;	Month is MonthPrime - 9
		),
		(	Month =< 2 ->
			Year is Year0 + 1
		;	Year = Year0
		).

	write_bytes([], _).
	write_bytes([Byte| Bytes], Stream) :-
		put_byte(Stream, Byte),
		write_bytes(Bytes, Stream).

:- end_object.
