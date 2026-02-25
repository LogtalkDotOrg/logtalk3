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



:- object(date,
	implements(datep)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2026-02-25,
		comment is 'Date predicates.'
	]).

	today(Year, Month, Day) :-
		os::date_time(Year, Month, Day, _, _, _, _).

	leap_year(Year) :-
		(	0 =:= mod(Year, 4), 0 =\= mod(Year, 100) ->
			true
		;	0 =:= mod(Year, 400)
		).

	name_of_day(1, 'Sunday', 'Sun').
	name_of_day(2, 'Monday', 'Mon').
	name_of_day(3, 'Tuesday', 'Tue').
	name_of_day(4, 'Wednesday', 'Wed').
	name_of_day(5, 'Thursday', 'Thu').
	name_of_day(6, 'Friday', 'Fri').
	name_of_day(7, 'Saturday', 'Sat').

	name_of_month( 1, 'January', 'Jan').
	name_of_month( 2, 'February', 'Feb').
	name_of_month( 3, 'March', 'Mar').
	name_of_month( 4, 'April', 'Apr').
	name_of_month( 5, 'May', 'May').
	name_of_month( 6, 'June', 'Jun').
	name_of_month( 7, 'July', 'Jul').
	name_of_month( 8, 'August', 'Aug').
	name_of_month( 9, 'September', 'Sep').
	name_of_month(10, 'October', 'Oct').
	name_of_month(11, 'November', 'Nov').
	name_of_month(12, 'December', 'Dec').

	days_in_month( 1, _, 31).
	days_in_month( 2, Year, Days) :-
		(	leap_year(Year) ->
			Days = 29
		; Days = 28
		).
	days_in_month( 3, _, 31).
	days_in_month( 4, _, 30).
	days_in_month( 5, _, 31).
	days_in_month( 6, _, 30).
	days_in_month( 7, _, 31).
	days_in_month( 8, _, 31).
	days_in_month( 9, _, 30).
	days_in_month(10, _, 31).
	days_in_month(11, _, 30).
	days_in_month(12, _, 31).

	valid(Year, Month, Day) :-
		integer(Year),
		integer(Month), Month >= 1, Month =< 12,
		integer(Day),
		days_in_month(Month, Year, Days),
		Day >= 1, Day =< Days.

	date_time_to_unix(DateTime, UnixTime) :-
		valid_date_time(DateTime, strict),
		date_time_julian_day(DateTime, JulianDay),
		epoch_julian_day(EpochJulianDay),
		extract_time(DateTime, Hours, Minutes, Seconds),
		UnixTime is (JulianDay - EpochJulianDay) * 86400 + Hours * 3600 + Minutes * 60 + Seconds.

	unix_to_date_time(UnixTime, date_time(Year, Month, Day, Hours, Minutes, Seconds)) :-
		integer(UnixTime),
		epoch_julian_day(EpochJulianDay),
		unix_day_offset(UnixTime, DayOffset),
		SecondsOfDay is UnixTime - DayOffset * 86400,
		JulianDay is EpochJulianDay + DayOffset,
		julian_day_date(JulianDay, Year, Month, Day),
		Hours is SecondsOfDay // 3600,
		Minutes is (SecondsOfDay mod 3600) // 60,
		Seconds is SecondsOfDay mod 60.

	add_duration(DateTime, Duration, ResultDateTime) :-
		date_time_to_unix(DateTime, UnixTime),
		duration_seconds(Duration, DeltaSeconds),
		ResultUnixTime is UnixTime + DeltaSeconds,
		unix_to_date_time(ResultUnixTime, ResultDateTime).

	subtract_duration(DateTime, Duration, ResultDateTime) :-
		date_time_to_unix(DateTime, UnixTime),
		duration_seconds(Duration, DeltaSeconds),
		ResultUnixTime is UnixTime - DeltaSeconds,
		unix_to_date_time(ResultUnixTime, ResultDateTime).

	duration_between(StartDateTime, EndDateTime, Duration) :-
		date_time_to_unix(StartDateTime, StartUnixTime),
		date_time_to_unix(EndDateTime, EndUnixTime),
		DeltaSeconds is EndUnixTime - StartUnixTime,
		(   var(Duration) ->
			Duration = DeltaSeconds
		;   integer(Duration) ->
			Duration =:= DeltaSeconds
		;   Duration = duration(_, _, _, _) ->
			seconds_duration(DeltaSeconds, Duration)
		;   fail
		).

	utc_to_local(UTCDateTime, Offset, LocalDateTime) :-
		date_time_to_unix(UTCDateTime, UnixTime),
		offset_seconds(Offset, OffsetSeconds),
		LocalUnixTime is UnixTime + OffsetSeconds,
		unix_to_date_time(LocalUnixTime, LocalDateTime).

	local_to_utc(LocalDateTime, Offset, UTCDateTime) :-
		date_time_to_unix(LocalDateTime, UnixTime),
		offset_seconds(Offset, OffsetSeconds),
		UTCUnixTime is UnixTime - OffsetSeconds,
		unix_to_date_time(UTCUnixTime, UTCDateTime).

	day_of_year(DateLike, DayOfYear) :-
		date_like(DateLike, Year, Month, Day),
		date_julian_day(Year, Month, Day, JulianDay),
		date_julian_day(Year, 1, 1, FirstJulianDay),
		DayOfYear is JulianDay - FirstJulianDay + 1.

	week_of_year_iso(DateLike, week(WeekNumber, WeekYear)) :-
		date_like(DateLike, Year, Month, Day),
		date_julian_day(Year, Month, Day, JulianDay),
		D4 is (((JulianDay + 31741 - (JulianDay mod 7)) mod 146097) mod 36524) mod 1461,
		L is D4 // 1460,
		WeekNumber is (((D4 - L) mod 365) + L) // 7 + 1,
		(	Month =:= 1,
			(Day =:= 1; Day =:= 2; Day =:= 3),
			WeekNumber > 1 ->
			WeekYear is Year - 1
		;	Month =:= 12,
			(Day =:= 29; Day =:= 30; Day =:= 31),
			WeekNumber =:= 1 ->
			WeekYear is Year + 1
		;	WeekYear = Year
		).

	weekday(DateLike, Weekday) :-
		date_like(DateLike, Year, Month, Day),
		date_julian_day(Year, Month, Day, JulianDay),
		Weekday is (JulianDay mod 7) + 1.

	normalize_date_time(date_time(Year0, Month0, Day0, Hours0, Minutes0, Seconds0), date_time(Year, Month, Day, Hours, Minutes, Seconds)) :-
		integer(Year0),
		integer(Month0),
		integer(Day0),
		integer(Hours0),
		integer(Minutes0),
		integer(Seconds0),
		TotalSeconds is Hours0 * 3600 + Minutes0 * 60 + Seconds0,
		DayOffset is TotalSeconds // 86400,
		SecondsOfDay is TotalSeconds - DayOffset * 86400,
		Day1 is Day0 + DayOffset,
		date_julian_day(Year0, Month0, Day1, JulianDay),
		julian_day_date(JulianDay, Year, Month, Day),
		Hours is SecondsOfDay // 3600,
		Minutes is (SecondsOfDay mod 3600) // 60,
		Seconds is SecondsOfDay mod 60.

	valid_date_time(DateTime) :-
		valid_date_time(DateTime, strict).

	valid_date_time(date_time(Year, Month, Day, Hours, Minutes, Seconds), strict) :-
		valid(Year, Month, Day),
		valid_time(Hours, Minutes, Seconds).
	valid_date_time(date_time(Year, Month, Day, Hours, Minutes, Seconds), relaxed) :-
		integer(Year),
		integer(Month),
		integer(Day),
		integer(Hours),
		integer(Minutes),
		integer(Seconds).

	epoch_julian_day(2440588).

	unix_day_offset(UnixTime, DayOffset) :-
		(   UnixTime >= 0 ->
			DayOffset is UnixTime // 86400
		;   AbsUnixTime is -UnixTime,
			DayOffset is -((AbsUnixTime + 86399) // 86400)
		).

	date_like(date(Year, Month, Day), Year, Month, Day) :-
		valid(Year, Month, Day).
	date_like(date_time(Year, Month, Day, Hours, Minutes, Seconds), Year, Month, Day) :-
		valid_time(Hours, Minutes, Seconds),
		valid(Year, Month, Day).

	extract_time(date_time(_, _, _, Hours, Minutes, Seconds), Hours, Minutes, Seconds).

	valid_time(Hours, Minutes, Seconds) :-
		integer(Hours), Hours >= 0, Hours =< 23,
		integer(Minutes), Minutes >= 0, Minutes =< 59,
		integer(Seconds), Seconds >= 0, Seconds =< 59.

	date_time_julian_day(date_time(Year, Month, Day, _, _, _), JulianDay) :-
		date_julian_day(Year, Month, Day, JulianDay).

	date_julian_day(Year0, Month, Day, JulianDay) :-
		(Year0 < 0 -> Year is Year0 + 1; Year = Year0),
		A is (14 - Month) // 12,
		Y is Year + 4800 - A,
		M is Month + 12 * A - 3,
		D is Day + ((153 * M + 2) // 5) + 365 * Y + Y // 4,
		JulianDay is D - Y // 100 + Y // 400 - 32045.

	julian_day_date(JulianDay, Year0, Month, Day) :-
		A is JulianDay + 32045,
		B is (4 * (A + 36524)) // 146097 - 1,
		C is A - (146097 * B) // 4,
		D is (4 * (C + 365)) // 1461 - 1,
		E is C - (1461 * D) // 4,
		M is (5 * (E - 1) + 2) // 153,
		Day is E - (153 * M + 2) // 5,
		Month is M + (3 - (12 * (M // 10))),
		Year is 100 * B + D - 4800 + M // 10,
		(Year < 1 -> Year0 is Year - 1; Year0 = Year).

	duration_seconds(duration(Days, Hours, Minutes, Seconds), TotalSeconds) :-
		integer(Days),
		integer(Hours),
		integer(Minutes),
		integer(Seconds),
		TotalSeconds is Days * 86400 + Hours * 3600 + Minutes * 60 + Seconds.
	duration_seconds(TotalSeconds, TotalSeconds) :-
		integer(TotalSeconds).

	seconds_duration(Seconds, duration(Days, Hours, Minutes, RemainingSeconds)) :-
		(	Seconds < 0 ->
			Sign = -1,
			AbsSeconds is -Seconds
		;	Sign = 1,
			AbsSeconds = Seconds
		),
		Days0 is AbsSeconds // 86400,
		Hours0 is (AbsSeconds mod 86400) // 3600,
		Minutes0 is (AbsSeconds mod 3600) // 60,
		RemainingSeconds0 is AbsSeconds mod 60,
		Days is Sign * Days0,
		Hours is Sign * Hours0,
		Minutes is Sign * Minutes0,
		RemainingSeconds is Sign * RemainingSeconds0.

	offset_seconds('Z', 0).
	offset_seconds(Offset, OffsetSeconds) :-
		atom(Offset),
		atom_chars(Offset, [Sign, H1, H2, ':', M1, M2]),
		(Sign == ('+'); Sign == ('-')),
		digits_value(H1, H2, Hours),
		digits_value(M1, M2, Minutes),
		Hours =< 23,
		Minutes =< 59,
		Seconds is Hours * 3600 + Minutes * 60,
		(	Sign == ('+') ->
			OffsetSeconds = Seconds
		;	OffsetSeconds is -Seconds
		).

	digits_value(C1, C2, Value) :-
		char_code(C1, Code1),
		char_code(C2, Code2),
		Code1 >= 0'0, Code1 =< 0'9,
		Code2 >= 0'0, Code2 =< 0'9,
		Value is (Code1 - 0'0) * 10 + (Code2 - 0'0).

:- end_object.
