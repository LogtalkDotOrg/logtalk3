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
		version is 2:1:0,
		author is 'Paulo Moura',
		date is 2026-04-08,
		comment is 'Unit tests for the "dates" library.'
	]).

	cover(date).
	cover(time).

	test(date_date_time_to_unix_2_01, deterministic(UnixTime == 0)) :-
		date::date_time_to_unix('date_time'(1970, 1, 1, 0, 0, 0), UnixTime).

	test(date_unix_to_date_time_2_01, deterministic(DateTime == 'date_time'(1970, 1, 1, 0, 0, 0))) :-
		date::unix_to_date_time(0, DateTime).

	test(date_unix_to_date_time_2_02, deterministic(DateTime == 'date_time'(1969, 12, 31, 23, 59, 59))) :-
		date::unix_to_date_time(-1, DateTime).

	test(date_add_duration_3_01, deterministic(ResultDateTime == 'date_time'(1970, 1, 2, 0, 0, 1))) :-
		date::add_duration('date_time'(1970, 1, 1, 0, 0, 0), duration(1, 0, 0, 1), ResultDateTime).

	test(date_subtract_duration_3_01, deterministic(ResultDateTime == 'date_time'(1969, 12, 31, 23, 59, 59))) :-
		date::subtract_duration('date_time'(1970, 1, 1, 0, 0, 0), 1, ResultDateTime).

	test(date_duration_between_3_01, deterministic(Duration == 61)) :-
		date::duration_between('date_time'(1970, 1, 1, 0, 0, 0), 'date_time'(1970, 1, 1, 0, 1, 1), Duration).

	test(date_duration_between_3_02, deterministic(duration(Days, Hours, Minutes, Seconds) == duration(0, 0, 1, 1))) :-
		date::duration_between('date_time'(1970, 1, 1, 0, 0, 0), 'date_time'(1970, 1, 1, 0, 1, 1), duration(Days, Hours, Minutes, Seconds)).

	test(date_utc_to_local_3_01, deterministic(LocalDateTime == 'date_time'(2026, 2, 25, 13, 15, 30))) :-
		date::utc_to_local('date_time'(2026, 2, 25, 11, 45, 30), '+01:30', LocalDateTime).

	test(date_local_to_utc_3_01, deterministic(UTCDateTime == 'date_time'(2026, 2, 25, 11, 45, 30))) :-
		date::local_to_utc('date_time'(2026, 2, 25, 13, 15, 30), '+01:30', UTCDateTime).

	test(date_day_of_year_2_01, deterministic(DayOfYear == 60)) :-
		date::day_of_year(date(2024, 2, 29), DayOfYear).

	test(date_day_of_year_date_3_01, deterministic(Date == date(2024, 2, 29))) :-
		date::day_of_year_date(2024, 60, Date).

	test(date_day_of_year_date_3_02, deterministic(Date == date(2023, 12, 31))) :-
		date::day_of_year_date(2023, 365, Date).

	test(date_day_of_year_date_3_03, false) :-
		date::day_of_year_date(2023, 366, _).

	test(date_month_weekday_date_5_01, deterministic(Date == date(2024, 3, 10))) :-
		date::month_weekday_date(2024, 3, 2, 7, Date).

	test(date_month_weekday_date_5_02, deterministic(Date == date(2024, 10, 27))) :-
		date::month_weekday_date(2024, 10, 5, 7, Date).

	test(date_name_of_day_3_01, deterministic(Name-Short == 'Monday'-'Mon')) :-
		date::name_of_day(1, Name, Short).

	test(date_name_of_day_3_02, deterministic(Name-Short == 'Sunday'-'Sun')) :-
		date::name_of_day(7, Name, Short).

	test(date_weekday_2_01, deterministic(Weekday == 1)) :-
		date::weekday(date(2024, 10, 14), Weekday).

	test(date_weekday_name_consistency_2_01, deterministic(Name-Short == 'Monday'-'Mon')) :-
		date::weekday(date(2024, 10, 14), Weekday),
		date::name_of_day(Weekday, Name, Short).

	test(date_week_of_year_iso_2_01, deterministic(Week == week(53, 2004))) :-
		date::week_of_year_iso(date(2005, 1, 1), Week).

	test(date_normalize_date_time_2_01, deterministic(Normalized == 'date_time'(2024, 3, 1, 0, 1, 10))) :-
		date::normalize_date_time('date_time'(2024, 2, 29, 23, 60, 70), Normalized).

	test(date_valid_date_time_1_01, true) :-
		date::valid_date_time('date_time'(2024, 2, 29, 23, 59, 59)).

	test(date_valid_date_time_1_02, false) :-
		date::valid_date_time('date_time'(2024, 2, 30, 23, 59, 59)).

	% calendar-aware add_duration/3 with duration/6 (year/month period arithmetic)

	% add 1 month to January 31 of a leap year: day clamped to February 29
	test(date_add_duration_3_calendar_01, deterministic(R == 'date_time'(2024, 2, 29, 0, 0, 0))) :-
		date::add_duration('date_time'(2024, 1, 31, 0, 0, 0), duration(0, 1, 0, 0, 0, 0), R).

	% add 1 month to January 31 of a non-leap year: day clamped to February 28
	test(date_add_duration_3_calendar_02, deterministic(R == 'date_time'(2023, 2, 28, 0, 0, 0))) :-
		date::add_duration('date_time'(2023, 1, 31, 0, 0, 0), duration(0, 1, 0, 0, 0, 0), R).

	% add 1 year to February 29 of a leap year: day clamped to February 28 in subsequent non-leap year
	test(date_add_duration_3_calendar_03, deterministic(R == 'date_time'(2025, 2, 28, 0, 0, 0))) :-
		date::add_duration('date_time'(2024, 2, 29, 0, 0, 0), duration(1, 0, 0, 0, 0, 0), R).

	% add 3 months crossing a year boundary with day clamping: November 30 -> February 28
	test(date_add_duration_3_calendar_04, deterministic(R == 'date_time'(2025, 2, 28, 0, 0, 0))) :-
		date::add_duration('date_time'(2024, 11, 30, 0, 0, 0), duration(0, 3, 0, 0, 0, 0), R).

	% add mixed duration: 1 year 2 months and 15 days/1h/30m/45s
	test(date_add_duration_3_calendar_05, deterministic(R == 'date_time'(2025, 4, 15, 1, 30, 45))) :-
		date::add_duration('date_time'(2024, 1, 31, 0, 0, 0), duration(1, 2, 15, 1, 30, 45), R).

	% add 0 years 0 months (day/time only via duration/6): same as adding days directly
	test(date_add_duration_3_calendar_06, deterministic(R == 'date_time'(2024, 1, 16, 0, 0, 0))) :-
		date::add_duration('date_time'(2024, 1, 1, 0, 0, 0), duration(0, 0, 15, 0, 0, 0), R).

	% calendar-aware subtract_duration/3 with duration/6

	% subtract 1 month from March 31 of a leap year: day clamped to February 29
	test(date_subtract_duration_3_calendar_01, deterministic(R == 'date_time'(2024, 2, 29, 0, 0, 0))) :-
		date::subtract_duration('date_time'(2024, 3, 31, 0, 0, 0), duration(0, 1, 0, 0, 0, 0), R).

	% subtract 1 month from March 31 of a non-leap year: day clamped to February 28
	test(date_subtract_duration_3_calendar_02, deterministic(R == 'date_time'(2023, 2, 28, 0, 0, 0))) :-
		date::subtract_duration('date_time'(2023, 3, 31, 0, 0, 0), duration(0, 1, 0, 0, 0, 0), R).

	% subtract 1 year: same day one year back
	test(date_subtract_duration_3_calendar_03, deterministic(R == 'date_time'(2022, 3, 31, 0, 0, 0))) :-
		date::subtract_duration('date_time'(2023, 3, 31, 0, 0, 0), duration(1, 0, 0, 0, 0, 0), R).

	% calendar-aware duration_between/3 with duration/6 output
	% The duration/6 skeleton must be provided so the calendar-aware branch is selected
	% (a free variable would select the default integer-seconds branch).

	% simple whole month: January 1 to February 1 = 1 month
	test(date_duration_between_3_calendar_01, deterministic(duration(Yr, Mo, Da, Hr, Mi, Se) == duration(0, 1, 0, 0, 0, 0))) :-
		date::duration_between('date_time'(2023, 1, 1, 0, 0, 0), 'date_time'(2023, 2, 1, 0, 0, 0), duration(Yr, Mo, Da, Hr, Mi, Se)).

	% January 31 to March 1 (non-leap): 1 month (to clamped Feb 28) + 1 residual day
	test(date_duration_between_3_calendar_02, deterministic(duration(Yr, Mo, Da, Hr, Mi, Se) == duration(0, 1, 1, 0, 0, 0))) :-
		date::duration_between('date_time'(2023, 1, 31, 0, 0, 0), 'date_time'(2023, 3, 1, 0, 0, 0), duration(Yr, Mo, Da, Hr, Mi, Se)).

	% January 31 to February 27 (non-leap): clamped Feb 28 overshoots, result is 0 months + 27 days
	test(date_duration_between_3_calendar_03, deterministic(duration(Yr, Mo, Da, Hr, Mi, Se) == duration(0, 0, 27, 0, 0, 0))) :-
		date::duration_between('date_time'(2023, 1, 31, 0, 0, 0), 'date_time'(2023, 2, 27, 0, 0, 0), duration(Yr, Mo, Da, Hr, Mi, Se)).

	% multi-year: January 1 2020 to March 15 2023 = 3 years 2 months 14 days
	test(date_duration_between_3_calendar_04, deterministic(duration(Yr, Mo, Da, Hr, Mi, Se) == duration(3, 2, 14, 0, 0, 0))) :-
		date::duration_between('date_time'(2020, 1, 1, 0, 0, 0), 'date_time'(2023, 3, 15, 0, 0, 0), duration(Yr, Mo, Da, Hr, Mi, Se)).

	% round-trip: add duration, then compute duration_between back to the original
	test(date_duration_between_3_calendar_05, deterministic(duration(Yr, Mo, Da, Hr, Mi, Se) == duration(1, 2, 0, 0, 0, 0))) :-
		date::add_duration('date_time'(2022, 6, 15, 0, 0, 0), duration(1, 2, 0, 0, 0, 0), End),
		date::duration_between('date_time'(2022, 6, 15, 0, 0, 0), End, duration(Yr, Mo, Da, Hr, Mi, Se)).

:- end_object.
