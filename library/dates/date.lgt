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
		version is 2:3:0,
		author is 'Paulo Moura',
		date is 2026-04-08,
		comment is 'Date predicates.'
	]).

	:- uses(list, [
		append/2, append/3, length/2
	]).

	today(Year, Month, Day) :-
		os::date_time(Year, Month, Day, _, _, _, _).

	leap_year(Year) :-
		(	0 =:= mod(Year, 4), 0 =\= mod(Year, 100) ->
			true
		;	0 =:= mod(Year, 400)
		).

	name_of_day(1, 'Monday', 'Mon').
	name_of_day(2, 'Tuesday', 'Tue').
	name_of_day(3, 'Wednesday', 'Wed').
	name_of_day(4, 'Thursday', 'Thu').
	name_of_day(5, 'Friday', 'Fri').
	name_of_day(6, 'Saturday', 'Sat').
	name_of_day(7, 'Sunday', 'Sun').

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
		valid_date_time(DateTime),
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

	add_duration(date_time(Year, Month, Day, Hours, Minutes, Seconds),
			 duration(DeltaYears, DeltaMonths, DeltaDays, DeltaHours, DeltaMinutes, DeltaSeconds),
			 ResultDateTime) :-
		!,
		integer(DeltaYears),
		integer(DeltaMonths),
		integer(DeltaDays),
		integer(DeltaHours),
		integer(DeltaMinutes),
		integer(DeltaSeconds),
		valid_date_time(date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		% Step 1: apply the year/month delta using calendar arithmetic with day clamping
		year_month_shift(Year, Month, DeltaYears, DeltaMonths, NewYear, NewMonth),
		days_in_month(NewMonth, NewYear, MaxDay),
		ClampedDay is min(Day, MaxDay),
		% Step 2: apply the fixed part (days + time) via Unix seconds arithmetic
		date_time_to_unix(date_time(NewYear, NewMonth, ClampedDay, Hours, Minutes, Seconds), UnixTime0),
		DeltaSec is DeltaDays * 86400 + DeltaHours * 3600 + DeltaMinutes * 60 + DeltaSeconds,
		ResultUnixTime is UnixTime0 + DeltaSec,
		unix_to_date_time(ResultUnixTime, ResultDateTime).

	add_duration(DateTime, Duration, ResultDateTime) :-
		date_time_to_unix(DateTime, UnixTime),
		duration_seconds(Duration, DeltaSeconds),
		ResultUnixTime is UnixTime + DeltaSeconds,
		unix_to_date_time(ResultUnixTime, ResultDateTime).

	subtract_duration(date_time(Year, Month, Day, Hours, Minutes, Seconds),
				  duration(DeltaYears, DeltaMonths, DeltaDays, DeltaHours, DeltaMinutes, DeltaSeconds),
				  ResultDateTime) :-
		!,
		integer(DeltaYears),
		integer(DeltaMonths),
		integer(DeltaDays),
		integer(DeltaHours),
		integer(DeltaMinutes),
		integer(DeltaSeconds),
		NegYears   is -DeltaYears,
		NegMonths  is -DeltaMonths,
		NegDays    is -DeltaDays,
		NegHours   is -DeltaHours,
		NegMinutes is -DeltaMinutes,
		NegSeconds is -DeltaSeconds,
		add_duration(date_time(Year, Month, Day, Hours, Minutes, Seconds),
				 duration(NegYears, NegMonths, NegDays, NegHours, NegMinutes, NegSeconds),
				 ResultDateTime).

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
		;   Duration = duration(_, _, _, _, _, _) ->
			calendar_duration_between(StartDateTime, EndDateTime, Duration)
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

	format_date_time(DateTime, OffsetSeconds, Format, String) :-
		valid_date_time(DateTime),
		integer(OffsetSeconds),
		format_date_time_(Format, DateTime, OffsetSeconds, String).

	day_of_year(DateLike, DayOfYear) :-
		date_like(DateLike, Year, Month, Day),
		date_julian_day(Year, Month, Day, JulianDay),
		date_julian_day(Year, 1, 1, FirstJulianDay),
		DayOfYear is JulianDay - FirstJulianDay + 1.

	day_of_year_date(Year, DayOfYear, date(Year, Month, Day)) :-
		integer(Year),
		integer(DayOfYear),
		( leap_year(Year) -> MaxDay is 366 ; MaxDay is 365 ),
		DayOfYear >= 1,
		DayOfYear =< MaxDay,
		date_julian_day(Year, 1, 1, FirstJulianDay),
		JulianDay is FirstJulianDay + DayOfYear - 1,
		julian_day_date(JulianDay, Year, Month, Day).

	month_weekday_date(Year, Month, Week, Weekday, date(Year, Month, Day)) :-
		integer(Year),
		integer(Month), Month >= 1, Month =< 12,
		integer(Week), Week >= 1, Week =< 5,
		integer(Weekday), Weekday >= 1, Weekday =< 7,
		weekday(date(Year, Month, 1), FirstWeekday),
		Delta is (Weekday - FirstWeekday + 7) mod 7,
		FirstDay is 1 + Delta,
		days_in_month(Month, Year, Length),
		( 	Week =:= 5 ->
			last_month_weekday(FirstDay, Length, Day)
		;	Day is FirstDay + (Week - 1) * 7,
			Day =< Length
		).

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

	last_month_weekday(Day0, Length, Day) :-
		NextDay is Day0 + 7,
		( 	NextDay =< Length ->
			last_month_weekday(NextDay, Length, Day)
		;	Day = Day0
		).

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

	valid_date_time(date_time(Year, Month, Day, Hours, Minutes, Seconds)) :-
		valid(Year, Month, Day),
		valid_time(Hours, Minutes, Seconds).

	before(DateTime1, DateTime2) :-
		date_time_to_unix(DateTime1, Unix1),
		date_time_to_unix(DateTime2, Unix2),
		Unix1 < Unix2.

	after(DateTime1, DateTime2) :-
		date_time_to_unix(DateTime1, Unix1),
		date_time_to_unix(DateTime2, Unix2),
		Unix1 > Unix2.

	same_instant(DateTime1, DateTime2) :-
		date_time_to_unix(DateTime1, Unix1),
		date_time_to_unix(DateTime2, Unix2),
		Unix1 =:= Unix2.

	compare_date_time(Order, DateTime1, DateTime2) :-
		date_time_to_unix(DateTime1, Unix1),
		date_time_to_unix(DateTime2, Unix2),
		compare(Order, Unix1, Unix2).

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
		!,
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

	offset_seconds('Z', 0) :-
		!.
	offset_seconds(Offset, OffsetSeconds) :-
		atom(Offset),
		atom_chars(Offset, [Sign, H1, H2, ':', M1, M2]),
		once((Sign == ('+'); Sign == ('-'))),
		digits_value(H1, H2, Hours),
		digits_value(M1, M2, Minutes),
		Hours =< 23,
		Minutes =< 59,
		Seconds is Hours * 3600 + Minutes * 60,
		(	Sign == ('+') ->
			OffsetSeconds = Seconds
		;	OffsetSeconds is -Seconds
		).

	format_date_time_(rfc3339, date_time(Year, Month, Day, Hours, Minutes, Seconds), OffsetSeconds, String) :-
		fixed_width_non_negative_integer_codes(Year, 4, YearCodes),
		two_digit_codes(Month, MonthCodes),
		two_digit_codes(Day, DayCodes),
		two_digit_codes(Hours, HoursCodes),
		two_digit_codes(Minutes, MinutesCodes),
		two_digit_codes(Seconds, SecondsCodes),
		rfc3339_offset_codes(OffsetSeconds, OffsetCodes),
		append([
			YearCodes, [0'-], MonthCodes, [0'-], DayCodes,
			[0'T],
			HoursCodes, [0':], MinutesCodes, [0':], SecondsCodes,
			OffsetCodes
		], Codes),
		atom_codes(String, Codes).
	format_date_time_(iso8601, DateTime, OffsetSeconds, String) :-
		format_date_time_(rfc3339, DateTime, OffsetSeconds, String).
	format_date_time_(atom, DateTime, OffsetSeconds, String) :-
		format_date_time_(rfc3339, DateTime, OffsetSeconds, String).
	format_date_time_(rfc2822, DateTime, OffsetSeconds, String) :-
		DateTime = date_time(Year, Month, Day, Hours, Minutes, Seconds),
		weekday(DateTime, Weekday),
		name_of_day(Weekday, _, DayShort),
		name_of_month(Month, _, MonthShort),
		atom_codes(DayShort, DayShortCodes),
		atom_codes(MonthShort, MonthShortCodes),
		two_digit_codes(Day, DayCodes),
		minimum_width_non_negative_integer_codes(Year, 4, YearCodes),
		two_digit_codes(Hours, HoursCodes),
		two_digit_codes(Minutes, MinutesCodes),
		two_digit_codes(Seconds, SecondsCodes),
		rfc2822_offset_codes(OffsetSeconds, OffsetCodes),
		append([
			DayShortCodes, [0',, 32],
			DayCodes, [32], MonthShortCodes, [32], YearCodes, [32],
			HoursCodes, [0':], MinutesCodes, [0':], SecondsCodes, [32],
			OffsetCodes
		], Codes),
		atom_codes(String, Codes).
	format_date_time_(rfc5322, DateTime, OffsetSeconds, String) :-
		format_date_time_(rfc2822, DateTime, OffsetSeconds, String).
	format_date_time_(rss, DateTime, OffsetSeconds, String) :-
		format_date_time_(rfc2822, DateTime, OffsetSeconds, String).
	format_date_time_(http_date, DateTime, OffsetSeconds, String) :-
		subtract_duration(DateTime, OffsetSeconds, GMTDateTime),
		GMTDateTime = date_time(Year, Month, Day, Hours, Minutes, Seconds),
		weekday(GMTDateTime, Weekday),
		name_of_day(Weekday, _, DayShort),
		name_of_month(Month, _, MonthShort),
		atom_codes(DayShort, DayShortCodes),
		atom_codes(MonthShort, MonthShortCodes),
		two_digit_codes(Day, DayCodes),
		minimum_width_non_negative_integer_codes(Year, 4, YearCodes),
		two_digit_codes(Hours, HoursCodes),
		two_digit_codes(Minutes, MinutesCodes),
		two_digit_codes(Seconds, SecondsCodes),
		append([
			DayShortCodes, [0',, 32],
			DayCodes, [32], MonthShortCodes, [32], YearCodes, [32],
			HoursCodes, [0':], MinutesCodes, [0':], SecondsCodes, [32],
			[0'G, 0'M, 0'T]
		], Codes),
		atom_codes(String, Codes).
	format_date_time_(rfc1123, DateTime, OffsetSeconds, String) :-
		format_date_time_(http_date, DateTime, OffsetSeconds, String).
	format_date_time_(unix_date, DateTime, _, String) :-
		DateTime = date_time(Year, Month, Day, Hours, Minutes, Seconds),
		weekday(DateTime, Weekday),
		name_of_day(Weekday, _, DayShort),
		name_of_month(Month, _, MonthShort),
		atom_codes(DayShort, DayShortCodes),
		atom_codes(MonthShort, MonthShortCodes),
		space_padded_day_codes(Day, DayCodes),
		minimum_width_non_negative_integer_codes(Year, 4, YearCodes),
		two_digit_codes(Hours, HoursCodes),
		two_digit_codes(Minutes, MinutesCodes),
		two_digit_codes(Seconds, SecondsCodes),
		append([
			DayShortCodes, [32], MonthShortCodes, [32], DayCodes, [32],
			HoursCodes, [0':], MinutesCodes, [0':], SecondsCodes, [32],
			YearCodes
		], Codes),
		atom_codes(String, Codes).
	format_date_time_(common_log, date_time(Year, Month, Day, Hours, Minutes, Seconds), OffsetSeconds, String) :-
		name_of_month(Month, _, MonthShort),
		atom_codes(MonthShort, MonthShortCodes),
		two_digit_codes(Day, DayCodes),
		minimum_width_non_negative_integer_codes(Year, 4, YearCodes),
		two_digit_codes(Hours, HoursCodes),
		two_digit_codes(Minutes, MinutesCodes),
		two_digit_codes(Seconds, SecondsCodes),
		rfc2822_offset_codes(OffsetSeconds, OffsetCodes),
		append([
			DayCodes, [0'/], MonthShortCodes, [0'/], YearCodes, [0':],
			HoursCodes, [0':], MinutesCodes, [0':], SecondsCodes, [32],
			OffsetCodes
		], Codes),
		atom_codes(String, Codes).
	format_date_time_(date_short, date_time(Year, Month, Day, _, _, _), _, String) :-
		fixed_width_non_negative_integer_codes(Year, 4, YearCodes),
		two_digit_codes(Month, MonthCodes),
		two_digit_codes(Day, DayCodes),
		append([
			YearCodes, [0'-], MonthCodes, [0'-], DayCodes
		], Codes),
		atom_codes(String, Codes).
	format_date_time_(date_medium, date_time(Year, Month, Day, _, _, _), _, String) :-
		name_of_month(Month, _, MonthShort),
		atom_codes(MonthShort, MonthShortCodes),
		minimum_width_non_negative_integer_codes(Day, 1, DayCodes),
		minimum_width_non_negative_integer_codes(Year, 4, YearCodes),
		append([
			DayCodes, [32], MonthShortCodes, [32], YearCodes
		], Codes),
		atom_codes(String, Codes).
	format_date_time_(date_long, date_time(Year, Month, Day, _, _, _), _, String) :-
		name_of_month(Month, MonthName, _),
		atom_codes(MonthName, MonthNameCodes),
		minimum_width_non_negative_integer_codes(Day, 1, DayCodes),
		minimum_width_non_negative_integer_codes(Year, 4, YearCodes),
		append([
			MonthNameCodes, [32], DayCodes, [0',, 32], YearCodes
		], Codes),
		atom_codes(String, Codes).
	format_date_time_(date_full, DateTime, _, String) :-
		DateTime = date_time(Year, Month, Day, _, _, _),
		weekday(DateTime, Weekday),
		name_of_day(Weekday, DayName, _),
		name_of_month(Month, MonthName, _),
		atom_codes(DayName, DayNameCodes),
		atom_codes(MonthName, MonthNameCodes),
		minimum_width_non_negative_integer_codes(Day, 1, DayCodes),
		minimum_width_non_negative_integer_codes(Year, 4, YearCodes),
		append([
			DayNameCodes, [0',, 32], MonthNameCodes, [32], DayCodes, [0',, 32], YearCodes
		], Codes),
		atom_codes(String, Codes).
	format_date_time_(time_short, date_time(_, _, _, Hours, Minutes, _), _, String) :-
		two_digit_codes(Hours, HoursCodes),
		two_digit_codes(Minutes, MinutesCodes),
		append([
			HoursCodes, [0':], MinutesCodes
		], Codes),
		atom_codes(String, Codes).
	format_date_time_(time_medium, date_time(_, _, _, Hours, Minutes, Seconds), _, String) :-
		two_digit_codes(Hours, HoursCodes),
		two_digit_codes(Minutes, MinutesCodes),
		two_digit_codes(Seconds, SecondsCodes),
		append([
			HoursCodes, [0':], MinutesCodes, [0':], SecondsCodes
		], Codes),
		atom_codes(String, Codes).
	format_date_time_(time_long, date_time(_, _, _, Hours, Minutes, Seconds), OffsetSeconds, String) :-
		two_digit_codes(Hours, HoursCodes),
		two_digit_codes(Minutes, MinutesCodes),
		two_digit_codes(Seconds, SecondsCodes),
		rfc3339_offset_codes(OffsetSeconds, OffsetCodes),
		append([
			HoursCodes, [0':], MinutesCodes, [0':], SecondsCodes, [32], OffsetCodes
		], Codes),
		atom_codes(String, Codes).
	format_date_time_(time_full, date_time(_, _, _, Hours, Minutes, Seconds), OffsetSeconds, String) :-
		twelve_hour_clock(Hours, TwelveHours, Meridiem),
		minimum_width_non_negative_integer_codes(TwelveHours, 1, HourCodes),
		two_digit_codes(Minutes, MinutesCodes),
		two_digit_codes(Seconds, SecondsCodes),
		atom_codes(Meridiem, MeridiemCodes),
		rfc3339_offset_codes(OffsetSeconds, OffsetCodes),
		append([
			HourCodes, [0':], MinutesCodes, [0':], SecondsCodes, [32], MeridiemCodes, [32], OffsetCodes
		], Codes),
		atom_codes(String, Codes).
	format_date_time_(date_time_short, date_time(Year, Month, Day, Hours, Minutes, _), _, String) :-
		fixed_width_non_negative_integer_codes(Year, 4, YearCodes),
		two_digit_codes(Month, MonthCodes),
		two_digit_codes(Day, DayCodes),
		two_digit_codes(Hours, HoursCodes),
		two_digit_codes(Minutes, MinutesCodes),
		append([
			YearCodes, [0'-], MonthCodes, [0'-], DayCodes, [32],
			HoursCodes, [0':], MinutesCodes
		], Codes),
		atom_codes(String, Codes).
	format_date_time_(date_time_medium, date_time(Year, Month, Day, Hours, Minutes, Seconds), _, String) :-
		name_of_month(Month, _, MonthShort),
		atom_codes(MonthShort, MonthShortCodes),
		minimum_width_non_negative_integer_codes(Day, 1, DayCodes),
		minimum_width_non_negative_integer_codes(Year, 4, YearCodes),
		two_digit_codes(Hours, HoursCodes),
		two_digit_codes(Minutes, MinutesCodes),
		two_digit_codes(Seconds, SecondsCodes),
		append([
			DayCodes, [32], MonthShortCodes, [32], YearCodes, [32],
			HoursCodes, [0':], MinutesCodes, [0':], SecondsCodes
		], Codes),
		atom_codes(String, Codes).
	format_date_time_(date_time_long, date_time(Year, Month, Day, Hours, Minutes, Seconds), OffsetSeconds, String) :-
		name_of_month(Month, MonthName, _),
		atom_codes(MonthName, MonthNameCodes),
		minimum_width_non_negative_integer_codes(Day, 1, DayCodes),
		minimum_width_non_negative_integer_codes(Year, 4, YearCodes),
		twelve_hour_clock(Hours, TwelveHours, Meridiem),
		minimum_width_non_negative_integer_codes(TwelveHours, 1, HourCodes),
		two_digit_codes(Minutes, MinutesCodes),
		two_digit_codes(Seconds, SecondsCodes),
		atom_codes(Meridiem, MeridiemCodes),
		rfc3339_offset_codes(OffsetSeconds, OffsetCodes),
		append([
			MonthNameCodes, [32], DayCodes, [0',, 32], YearCodes, [32],
			HourCodes, [0':], MinutesCodes, [0':], SecondsCodes, [32], MeridiemCodes, [32], OffsetCodes
		], Codes),
		atom_codes(String, Codes).
	format_date_time_(date_time_full, DateTime, OffsetSeconds, String) :-
		DateTime = date_time(Year, Month, Day, Hours, Minutes, Seconds),
		weekday(DateTime, Weekday),
		name_of_day(Weekday, DayName, _),
		name_of_month(Month, MonthName, _),
		atom_codes(DayName, DayNameCodes),
		atom_codes(MonthName, MonthNameCodes),
		minimum_width_non_negative_integer_codes(Day, 1, DayCodes),
		minimum_width_non_negative_integer_codes(Year, 4, YearCodes),
		twelve_hour_clock(Hours, TwelveHours, Meridiem),
		minimum_width_non_negative_integer_codes(TwelveHours, 1, HourCodes),
		two_digit_codes(Minutes, MinutesCodes),
		two_digit_codes(Seconds, SecondsCodes),
		atom_codes(Meridiem, MeridiemCodes),
		rfc3339_offset_codes(OffsetSeconds, OffsetCodes),
		append([
			DayNameCodes, [0',, 32], MonthNameCodes, [32], DayCodes, [0',, 32], YearCodes, [32],
			HourCodes, [0':], MinutesCodes, [0':], SecondsCodes, [32], MeridiemCodes, [32], OffsetCodes
		], Codes),
		atom_codes(String, Codes).

	rfc3339_offset_codes(0, [0'Z]) :-
		!.
	rfc3339_offset_codes(OffsetSeconds, [SignCode, HourTens, HourOnes, 0':, MinuteTens, MinuteOnes]) :-
		offset_seconds_components(OffsetSeconds, SignCode, OffsetHours, OffsetMinutes),
		two_digit_codes(OffsetHours, [HourTens, HourOnes]),
		two_digit_codes(OffsetMinutes, [MinuteTens, MinuteOnes]).

	rfc2822_offset_codes(OffsetSeconds, [SignCode, HourTens, HourOnes, MinuteTens, MinuteOnes]) :-
		offset_seconds_components(OffsetSeconds, SignCode, OffsetHours, OffsetMinutes),
		two_digit_codes(OffsetHours, [HourTens, HourOnes]),
		two_digit_codes(OffsetMinutes, [MinuteTens, MinuteOnes]).

	offset_seconds_components(OffsetSeconds, SignCode, OffsetHours, OffsetMinutes) :-
		integer(OffsetSeconds),
		AbsOffsetSeconds is abs(OffsetSeconds),
		0 is AbsOffsetSeconds mod 60,
		AbsOffsetSeconds =< 86340,
		OffsetHours is AbsOffsetSeconds // 3600,
		OffsetMinutes is (AbsOffsetSeconds mod 3600) // 60,
		( 	OffsetSeconds < 0 ->
			SignCode = 0'-
		;	SignCode = 0'+
		).

	two_digit_codes(Integer, [TensCode, OnesCode]) :-
		integer(Integer),
		Integer >= 0,
		Integer =< 99,
		TensCode is Integer // 10 + 0'0,
		OnesCode is Integer mod 10 + 0'0.

	space_padded_day_codes(Integer, [32, DayCode]) :-
		integer(Integer),
		Integer >= 0,
		Integer < 10,
		DayCode is Integer + 0'0.
	space_padded_day_codes(Integer, Codes) :-
		two_digit_codes(Integer, Codes).

	twelve_hour_clock(Hours, TwelveHours, 'AM') :-
		integer(Hours),
		Hours >= 0,
		Hours < 12,
		( 	Hours =:= 0 ->
			TwelveHours = 12
		;	TwelveHours = Hours
		).
	twelve_hour_clock(Hours, TwelveHours, 'PM') :-
		integer(Hours),
		Hours >= 12,
		Hours =< 23,
		( 	Hours =:= 12 ->
			TwelveHours = 12
		;	TwelveHours is Hours - 12
		).

	fixed_width_non_negative_integer_codes(Integer, Width, Codes) :-
		integer(Integer),
		Integer >= 0,
		number_codes(Integer, RawCodes),
		length(RawCodes, RawLength),
		RawLength =< Width,
		Padding is Width - RawLength,
		zero_codes(Padding, ZeroCodes),
		append(ZeroCodes, RawCodes, Codes).

	minimum_width_non_negative_integer_codes(Integer, Width, Codes) :-
		integer(Integer),
		Integer >= 0,
		number_codes(Integer, RawCodes),
		length(RawCodes, RawLength),
		( 	RawLength >= Width ->
			Codes = RawCodes
		;	Padding is Width - RawLength,
			zero_codes(Padding, ZeroCodes),
			append(ZeroCodes, RawCodes, Codes)
		).

	zero_codes(0, []) :-
		!.
	zero_codes(Count, [0'0| Codes]) :-
		Count > 0,
		NextCount is Count - 1,
		zero_codes(NextCount, Codes).

	digits_value(C1, C2, Value) :-
		char_code(C1, Code1),
		char_code(C2, Code2),
		Code1 >= 0'0, Code1 =< 0'9,
		Code2 >= 0'0, Code2 =< 0'9,
		Value is (Code1 - 0'0) * 10 + (Code2 - 0'0).

	% year_month_shift(+Year, +Month, +DeltaYears, +DeltaMonths, -NewYear, -NewMonth)
	% Advances Year/Month by DeltaYears years and DeltaMonths months using floored
	% integer division so that negative month indices map correctly to prior years.
	year_month_shift(Year, Month, DeltaYears, DeltaMonths, NewYear, NewMonth) :-
		MonthIndex is (Year - 1) * 12 + (Month - 1) + DeltaYears * 12 + DeltaMonths,
		NewYear is MonthIndex div 12 + 1,
		NewMonth is MonthIndex mod 12 + 1.

	% apply_months_to_datetime(+DateTime, +DeltaMonths, -NewDateTime)
	% Applies DeltaMonths calendar months to DateTime, clamping the day to the
	% last valid day of the resulting month if necessary.
	apply_months_to_datetime(date_time(Year, Month, Day, H, Mi, Se), DeltaMonths, date_time(NewYear, NewMonth, NewDay, H, Mi, Se)) :-
		year_month_shift(Year, Month, 0, DeltaMonths, NewYear, NewMonth),
		days_in_month(NewMonth, NewYear, MaxDay),
		NewDay is min(Day, MaxDay).

	% calendar_duration_between(+StartDateTime, +EndDateTime, -Duration)
	% Decomposes the interval into whole calendar months plus an exact day/time residual.
	% For backward intervals all fields are negated.
	calendar_duration_between(StartDateTime, EndDateTime, duration(Years, Months, Days, Hours, Minutes, Seconds)) :-
		date_time_to_unix(StartDateTime, StartUnix),
		date_time_to_unix(EndDateTime, EndUnix),
		(   StartUnix =< EndUnix ->
			StartDateTime = date_time(SYear, SMonth, _, _, _, _),
			EndDateTime   = date_time(EYear, EMonth, _, _, _, _),
			RawDeltaMonths is (EYear - SYear) * 12 + (EMonth - SMonth),
			apply_months_to_datetime(StartDateTime, RawDeltaMonths, Intermediate0),
			date_time_to_unix(Intermediate0, T0),
			(   T0 > EndUnix ->
				DeltaMonths is RawDeltaMonths - 1
			;   DeltaMonths = RawDeltaMonths
			),
			apply_months_to_datetime(StartDateTime, DeltaMonths, Intermediate),
			Years  is DeltaMonths // 12,
			Months is DeltaMonths mod 12,
			date_time_to_unix(Intermediate, IntUnix),
			RemainSecs is EndUnix - IntUnix,
			seconds_duration(RemainSecs, duration(Days, Hours, Minutes, Seconds))
		;   % Backward interval: compute as forward and negate all fields
			calendar_duration_between(EndDateTime, StartDateTime, duration(Y0, Mo0, D0, H0, Mi0, Se0)),
			Years   is -Y0,
			Months  is -Mo0,
			Days    is -D0,
			Hours   is -H0,
			Minutes is -Mi0,
			Seconds is -Se0
		).

:- end_object.
