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


:- protocol(datep).

	:- info([
		version is 2:3:0,
		author is 'Paulo Moura',
		date is 2026-04-08,
		comment is 'Date protocol.',
		see_also is [date, timep]
	]).

	:- public(today/3).
	:- mode(today(-integer, -integer, -integer), one).
	:- info(today/3, [
		comment is 'Returns current date.',
		argnames is ['Year', 'Month', 'Day']
	]).

	:- public(leap_year/1).
	:- mode(leap_year(+integer), zero_or_one).
	:- info(leap_year/1, [
		comment is 'True if the argument is a leap year.',
		argnames is ['Year']
	]).

	:- public(name_of_day/3).
	:- mode(name_of_day(?integer, ?atom, ?atom), zero_or_more).
	:- info(name_of_day/3, [
		comment is 'Name and short name of day using ISO weekday numbering (Monday=1, ..., Sunday=7).',
		argnames is ['Index', 'Name', 'Short']
	]).

	:- public(name_of_month/3).
	:- mode(name_of_month(?integer, ?atom, ?atom), zero_or_more).
	:- info(name_of_month/3, [
		comment is 'Name and short name of month.',
		argnames is ['Index', 'Name', 'Short']
	]).

	:- public(days_in_month/3).
	:- mode(days_in_month(?integer, +integer, ?integer), zero_or_more).
	:- info(days_in_month/3, [
		comment is 'Number of days in a month.',
		argnames is ['Month', 'Year', 'Days']
	]).

	:- public(valid/3).
	:- mode(valid(@integer, @integer, @integer), zero_or_one).
	:- info(valid/3, [
		comment is 'True if the arguments represent a valid date.',
		argnames is ['Year', 'Month', 'Day']
	]).

	:- public(date_time_to_unix/2).
	:- mode(date_time_to_unix(+compound, -integer), zero_or_one).
	:- info(date_time_to_unix/2, [
		comment is 'Converts a UTC date-time term ``date_time(Year,Month,Day,Hours,Minutes,Seconds)`` to Unix epoch seconds.',
		argnames is ['DateTime', 'UnixTime']
	]).

	:- public(unix_to_date_time/2).
	:- mode(unix_to_date_time(+integer, -compound), one).
	:- info(unix_to_date_time/2, [
		comment is 'Converts Unix epoch seconds to a UTC date-time term ``date_time(Year,Month,Day,Hours,Minutes,Seconds)``.',
		argnames is ['UnixTime', 'DateTime']
	]).

	:- public(add_duration/3).
	:- mode(add_duration(+compound, +nonvar, -compound), zero_or_one).
	:- info(add_duration/3, [
		comment is 'Adds a duration to a datetime. Duration can be integer seconds, ``duration(Days,Hours,Minutes,Seconds)``, or a calendar-aware ``duration(Years,Months,Days,Hours,Minutes,Seconds)``. For the 6-arity form, the year and month delta is applied first using calendar arithmetic, clamping the day to the last valid day of the resulting month when necessary (e.g. January 31 plus one month gives February 28 or 29), and the remaining day and time delta is then applied via fixed-length arithmetic.',
		argnames is ['DateTime', 'Duration', 'ResultDateTime']
	]).

	:- public(subtract_duration/3).
	:- mode(subtract_duration(+compound, +nonvar, -compound), zero_or_one).
	:- info(subtract_duration/3, [
		comment is 'Subtracts a duration from a datetime. Duration can be integer seconds, ``duration(Days,Hours,Minutes,Seconds)``, or a calendar-aware ``duration(Years,Months,Days,Hours,Minutes,Seconds)``. For the 6-arity form, the year and month delta is subtracted first using calendar arithmetic with end-of-month day clamping, and the remaining day and time delta is then subtracted via fixed-length arithmetic.',
		argnames is ['DateTime', 'Duration', 'ResultDateTime']
	]).

	:- public(duration_between/3).
	:- mode(duration_between(+compound, +compound, ?term), zero_or_one).
	:- info(duration_between/3, [
		comment is 'Computes the difference between two datetimes as integer seconds, as ``duration(Days,Hours,Minutes,Seconds)``, or as a calendar-aware ``duration(Years,Months,Days,Hours,Minutes,Seconds)``. For the 6-arity form, the year and month components count the largest whole number of calendar months between the two datetimes (consistent with the day-clamping semantics of ``add_duration/3``), and the remaining days and time fields are the exact residual. For backward intervals all fields are negative.',
		argnames is ['StartDateTime', 'EndDateTime', 'Duration']
	]).

	:- public(utc_to_local/3).
	:- mode(utc_to_local(+compound, +atom, -compound), zero_or_one).
	:- info(utc_to_local/3, [
		comment is 'Converts a UTC datetime to a local datetime using an explicit timezone offset atom (``Z`` or ``±HH:MM``).',
		argnames is ['UTCDateTime', 'Offset', 'LocalDateTime']
	]).

	:- public(local_to_utc/3).
	:- mode(local_to_utc(+compound, +atom, -compound), zero_or_one).
	:- info(local_to_utc/3, [
		comment is 'Converts a local datetime to UTC using an explicit timezone offset atom (``Z`` or ``±HH:MM``).',
		argnames is ['LocalDateTime', 'Offset', 'UTCDateTime']
	]).

	:- public(format_date_time/4).
	:- mode(format_date_time(+compound, +integer, +atom, -atom), zero_or_one).
	:- info(format_date_time/4, [
		comment is 'Formats a date-time using an explicit UTC offset in seconds. Supported format identifiers are ``rfc3339``, ``iso8601``, ``atom``, ``rfc2822``, ``rfc5322``, ``rss``, ``http_date``, ``rfc1123``, ``unix_date``, ``common_log``, ``date_short``, ``date_medium``, ``date_long``, ``date_full``, ``time_short``, ``time_medium``, ``time_long``, ``time_full``, ``date_time_short``, ``date_time_medium``, ``date_time_long``, and ``date_time_full``. RFC 3339, ISO 8601, Atom, and the ``date_*`` and ``date_time_*`` styles require a four-digit non-negative year. Formats that include numeric offsets require an offset expressible in whole minutes. HTTP-date and RFC 1123 output are always normalized to GMT. The style presets are English-only presentation formats. Fails if the format or date-time are not valid.',
		argnames is ['DateTime', 'OffsetSeconds', 'Format', 'String']
	]).

	:- public(day_of_year/2).
	:- mode(day_of_year(+compound, ?integer), zero_or_one).
	:- info(day_of_year/2, [
		comment is 'Computes the day of year (1-366) for a ``date(Year,Month,Day)`` or ``date_time(...)`` term.',
		argnames is ['DateLike', 'DayOfYear']
	]).

	:- public(day_of_year_date/3).
	:- mode(day_of_year_date(+integer, +integer, -compound), zero_or_one).
	:- info(day_of_year_date/3, [
		comment is 'Computes the calendar date corresponding to a year and day of year (1-366) as ``date(Year,Month,Day)``.',
		argnames is ['Year', 'DayOfYear', 'Date']
	]).

	:- public(month_weekday_date/5).
	:- mode(month_weekday_date(+integer, +integer, +integer, +integer, -compound), zero_or_one).
	:- info(month_weekday_date/5, [
		comment is 'Computes the calendar date for the nth or last weekday in a month as ``date(Year,Month,Day)``. Week values 1-4 select the nth weekday and week value 5 selects the last one. Weekday uses ISO numbering (Monday=1, ..., Sunday=7).',
		argnames is ['Year', 'Month', 'Week', 'Weekday', 'Date']
	]).

	:- public(week_of_year_iso/2).
	:- mode(week_of_year_iso(+compound, ?compound), zero_or_one).
	:- info(week_of_year_iso/2, [
		comment is 'Computes ISO week for a ``date(Year,Month,Day)`` or ``date_time(...)`` term as ``week(Week,Year)``.',
		argnames is ['DateLike', 'ISOWeek']
	]).

	:- public(weekday/2).
	:- mode(weekday(+compound, ?integer), zero_or_one).
	:- info(weekday/2, [
		comment is 'Computes ISO weekday number (Monday=1, ..., Sunday=7) for a ``date(Year,Month,Day)`` or ``date_time(...)`` term.',
		argnames is ['DateLike', 'Weekday']
	]).

	:- public(normalize_date_time/2).
	:- mode(normalize_date_time(+compound, -compound), one).
	:- info(normalize_date_time/2, [
		comment is 'Normalizes a datetime term by carrying overflows/underflows in date and time fields.',
		argnames is ['DateTime', 'NormalizedDateTime']
	]).

	:- public(valid_date_time/1).
	:- mode(valid_date_time(@compound), zero_or_one).
	:- info(valid_date_time/1, [
		comment is 'True iff a datetime term is valid in strict mode.',
		argnames is ['DateTime']
	]).

	:- public(before/2).
	:- mode(before(+compound, +compound), zero_or_one).
	:- info(before/2, [
		comment is 'True iff ``DateTime1`` represents an instant strictly before ``DateTime2``.',
		argnames is ['DateTime1', 'DateTime2']
	]).

	:- public(after/2).
	:- mode(after(+compound, +compound), zero_or_one).
	:- info(after/2, [
		comment is 'True iff ``DateTime1`` represents an instant strictly after ``DateTime2``.',
		argnames is ['DateTime1', 'DateTime2']
	]).

	:- public(same_instant/2).
	:- mode(same_instant(+compound, +compound), zero_or_one).
	:- info(same_instant/2, [
		comment is 'True iff ``DateTime1`` and ``DateTime2`` represent the same instant (equal Unix epoch seconds).',
		argnames is ['DateTime1', 'DateTime2']
	]).

	:- public(compare_date_time/3).
	:- mode(compare_date_time(?atom, +compound, +compound), zero_or_one).
	:- info(compare_date_time/3, [
		comment is 'Three-way comparison of two datetime terms. ``Order`` is unified with ``<``, ``=``, or ``>``. Suitable for use with ``sort/3``.',
		argnames is ['Order', 'DateTime1', 'DateTime2']
	]).

:- end_protocol.
