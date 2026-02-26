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
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-02-25,
		comment is 'Unit tests for the "dates" library.'
	]).

	cover(date).
	cover(time).

	test(date_date_time_to_unix_2_01, true(UnixTime == 0)) :-
		date::date_time_to_unix('date_time'(1970, 1, 1, 0, 0, 0), UnixTime).

	test(date_unix_to_date_time_2_01, true(DateTime == 'date_time'(1970, 1, 1, 0, 0, 0))) :-
		date::unix_to_date_time(0, DateTime).

	test(date_unix_to_date_time_2_02, true(DateTime == 'date_time'(1969, 12, 31, 23, 59, 59))) :-
		date::unix_to_date_time(-1, DateTime).

	test(date_add_duration_3_01, true(ResultDateTime == 'date_time'(1970, 1, 2, 0, 0, 1))) :-
		date::add_duration('date_time'(1970, 1, 1, 0, 0, 0), duration(1, 0, 0, 1), ResultDateTime).

	test(date_subtract_duration_3_01, true(ResultDateTime == 'date_time'(1969, 12, 31, 23, 59, 59))) :-
		date::subtract_duration('date_time'(1970, 1, 1, 0, 0, 0), 1, ResultDateTime).

	test(date_duration_between_3_01, true(Duration == 61)) :-
		date::duration_between('date_time'(1970, 1, 1, 0, 0, 0), 'date_time'(1970, 1, 1, 0, 1, 1), Duration).

	test(date_duration_between_3_02, true(duration(Days, Hours, Minutes, Seconds) == duration(0, 0, 1, 1))) :-
		date::duration_between('date_time'(1970, 1, 1, 0, 0, 0), 'date_time'(1970, 1, 1, 0, 1, 1), duration(Days, Hours, Minutes, Seconds)).

	test(date_utc_to_local_3_01, true(LocalDateTime == 'date_time'(2026, 2, 25, 13, 15, 30))) :-
		date::utc_to_local('date_time'(2026, 2, 25, 11, 45, 30), '+01:30', LocalDateTime).

	test(date_local_to_utc_3_01, true(UTCDateTime == 'date_time'(2026, 2, 25, 11, 45, 30))) :-
		date::local_to_utc('date_time'(2026, 2, 25, 13, 15, 30), '+01:30', UTCDateTime).

	test(date_day_of_year_2_01, true(DayOfYear == 60)) :-
		date::day_of_year(date(2024, 2, 29), DayOfYear).

	test(date_weekday_2_01, true(Weekday == 1)) :-
		date::weekday(date(2024, 10, 14), Weekday).

	test(date_week_of_year_iso_2_01, true(Week == week(53, 2004))) :-
		date::week_of_year_iso(date(2005, 1, 1), Week).

	test(date_normalize_date_time_2_01, true(Normalized == 'date_time'(2024, 3, 1, 0, 1, 10))) :-
		date::normalize_date_time('date_time'(2024, 2, 29, 23, 60, 70), Normalized).

	test(date_valid_date_time_1_01, true) :-
		date::valid_date_time('date_time'(2024, 2, 29, 23, 59, 59)).

	test(date_valid_date_time_1_02, false) :-
		date::valid_date_time('date_time'(2024, 2, 30, 23, 59, 59)).

	test(date_valid_date_time_2_01, true) :-
		date::valid_date_time('date_time'(2024, 2, 29, 23, 60, 70), relaxed).

	test(date_valid_date_time_2_02, false) :-
		date::valid_date_time('date_time'(2024, 2, 29, 23, 60, 70), strict).

:- end_object.
