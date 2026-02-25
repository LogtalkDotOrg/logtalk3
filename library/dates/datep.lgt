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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2026-02-25,
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
		comment is 'Name and short name of day.',
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
		comment is 'Adds a duration to a datetime. Duration can be integer seconds or ``duration(Days,Hours,Minutes,Seconds)``.',
		argnames is ['DateTime', 'Duration', 'ResultDateTime']
	]).

	:- public(subtract_duration/3).
	:- mode(subtract_duration(+compound, +nonvar, -compound), zero_or_one).
	:- info(subtract_duration/3, [
		comment is 'Subtracts a duration from a datetime. Duration can be integer seconds or ``duration(Days,Hours,Minutes,Seconds)``.',
		argnames is ['DateTime', 'Duration', 'ResultDateTime']
	]).

	:- public(duration_between/3).
	:- mode(duration_between(+compound, +compound, ?term), zero_or_one).
	:- info(duration_between/3, [
		comment is 'Computes the difference between two datetimes as integer seconds or as ``duration(Days,Hours,Minutes,Seconds)``.',
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

	:- public(day_of_year/2).
	:- mode(day_of_year(+compound, ?integer), zero_or_one).
	:- info(day_of_year/2, [
		comment is 'Computes the day of year (1-366) for a ``date(Year,Month,Day)`` or ``date_time(...)`` term.',
		argnames is ['DateLike', 'DayOfYear']
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

	:- public(valid_date_time/2).
	:- mode(valid_date_time(@compound, +atom), zero_or_one).
	:- info(valid_date_time/2, [
		comment is 'Validates a datetime term in ``strict`` or ``relaxed`` mode.',
		argnames is ['DateTime', 'Mode']
	]).

:- end_protocol.
