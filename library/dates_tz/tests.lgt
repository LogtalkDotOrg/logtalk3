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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-08,
		comment is 'Unit tests for the "dates_tz" library.'
	]).

	cover(dates_tz).

	% Load required TZif data before running the test set.
	% Zones used: Etc/UTC (offset 0), America/New_York (EST/EDT), Asia/Kathmandu (NPT +05:45).
	setup :-
		^^file_path('../tzif/test_files/Etc/UTC', UTCPath),
		^^file_path('../tzif/test_files/America/New_York', NYPath),
		^^file_path('../tzif/test_files/Asia/Kathmandu', KTMPath),
		tzif::load(file(UTCPath, 'Etc/UTC')),
		tzif::load(file(NYPath, 'America/New_York')),
		tzif::load(file(KTMPath, 'Asia/Kathmandu')).

	cleanup :-
		tzif::clear_cache.

	% utc_to_local_tz/3 tests

	% Etc/UTC has offset 0 so the conversion is an identity.
	test(dates_tz_utc_to_local_tz_3_01, deterministic(Local == date_time(2024,1,15,12,0,0))) :-
		dates_tz::utc_to_local_tz(date_time(2024,1,15,12,0,0), 'Etc/UTC', Local).

	% America/New_York in winter (2024-01-15) is EST = UTC-5 (-18000 s).
	test(dates_tz_utc_to_local_tz_3_02, deterministic(Local == date_time(2024,1,15,7,0,0))) :-
		dates_tz::utc_to_local_tz(date_time(2024,1,15,12,0,0), 'America/New_York', Local).

	% America/New_York in summer (2024-07-01) is EDT = UTC-4 (-14400 s).
	test(dates_tz_utc_to_local_tz_3_03, deterministic(Local == date_time(2024,7,1,8,0,0))) :-
		dates_tz::utc_to_local_tz(date_time(2024,7,1,12,0,0), 'America/New_York', Local).

	% Asia/Kathmandu is NPT = UTC+5:45 (+20700 s). No DST.
	test(dates_tz_utc_to_local_tz_3_04, deterministic(Local == date_time(2024,1,15,17,45,0))) :-
		dates_tz::utc_to_local_tz(date_time(2024,1,15,12,0,0), 'Asia/Kathmandu', Local).

	% local_to_utc_tz/3 tests (strict)

	% Etc/UTC is the inverse of utc_to_local_tz with offset 0.
	test(dates_tz_local_to_utc_tz_3_01, deterministic(UTC == date_time(2024,1,15,12,0,0))) :-
		dates_tz::local_to_utc_tz(date_time(2024,1,15,12,0,0), 'Etc/UTC', UTC).

	% America/New_York winter: local 07:00 EST = UTC 12:00.
	test(dates_tz_local_to_utc_tz_3_02, deterministic(UTC == date_time(2024,1,15,12,0,0))) :-
		dates_tz::local_to_utc_tz(date_time(2024,1,15,7,0,0), 'America/New_York', UTC).

	% The strict variant fails for a fold (ambiguous) time.
	% 2024-11-03 01:30 America/New_York falls in the EST/EDT fall-back transition.
	test(dates_tz_local_to_utc_tz_3_03, fail) :-
		dates_tz::local_to_utc_tz(date_time(2024,11,3,1,30,0), 'America/New_York', _).

	% local_to_utc_tz_with_resolution/4 tests

	% Resolution 'first' picks the earlier (EDT) interpretation of the fold time.
	% EDT = UTC-4, so local 01:30 EDT -> UTC 05:30.
	test(dates_tz_local_to_utc_tz_with_resolution_4_01, deterministic(UTC == date_time(2024,11,3,5,30,0))) :-
		dates_tz::local_to_utc_tz_with_resolution(date_time(2024,11,3,1,30,0), 'America/New_York', first, UTC).

	% Resolution 'second' picks the later (EST) interpretation of the fold time.
	% EST = UTC-5, so local 01:30 EST -> UTC 06:30.
	test(dates_tz_local_to_utc_tz_with_resolution_4_02, deterministic(UTC == date_time(2024,11,3,6,30,0))) :-
		dates_tz::local_to_utc_tz_with_resolution(date_time(2024,11,3,1,30,0), 'America/New_York', second, UTC).

	% Resolution 'all' enumerates both interpretations in chronological order.
	test(dates_tz_local_to_utc_tz_with_resolution_4_03, deterministic(UTCs == [date_time(2024,11,3,5,30,0), date_time(2024,11,3,6,30,0)])) :-
		findall(UTC, dates_tz::local_to_utc_tz_with_resolution(date_time(2024,11,3,1,30,0), 'America/New_York', all, UTC), UTCs).

	% convert_zones/4 tests

	% New_York (EST, winter) to UTC: local 07:00 -> UTC 12:00.
	test(dates_tz_convert_zones_4_01, deterministic(Result == date_time(2024,1,15,12,0,0))) :-
		dates_tz::convert_zones(date_time(2024,1,15,7,0,0), 'America/New_York', 'Etc/UTC', Result).

	% UTC to Kathmandu (NPT): UTC 12:00 -> local 17:45.
	test(dates_tz_convert_zones_4_02, deterministic(Result == date_time(2024,1,15,17,45,0))) :-
		dates_tz::convert_zones(date_time(2024,1,15,12,0,0), 'Etc/UTC', 'Asia/Kathmandu', Result).

	% New_York (EST, winter) to Kathmandu:
	% local 07:00 EST -> UTC 12:00 -> NPT 17:45.
	test(dates_tz_convert_zones_4_03, deterministic(Result == date_time(2024,1,15,17,45,0))) :-
		dates_tz::convert_zones(date_time(2024,1,15,7,0,0), 'America/New_York', 'Asia/Kathmandu', Result).

	% convert_zones_with_resolution/5 tests

	% Resolving the fold with 'first' (EDT interpretation) then converting to UTC:
	% local 01:30 NY first -> UTC 05:30 -> UTC 05:30 (identity via Etc/UTC).
	test(dates_tz_convert_zones_with_resolution_5_01, deterministic(Result == date_time(2024,11,3,5,30,0))) :-
		dates_tz::convert_zones_with_resolution(date_time(2024,11,3,1,30,0), 'America/New_York', first, 'Etc/UTC', Result).

:- end_object.
