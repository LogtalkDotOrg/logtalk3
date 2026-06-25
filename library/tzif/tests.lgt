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
		version is 1:1:1,
		author is 'Paulo Moura',
		date is 2026-06-24,
		comment is 'Unit tests for the tzif library.'
	]).

	cover(tzif).

	cleanup :-
		tzif::clear_cache,
		^^clean_file('test_files/tzif_snapshot.pl'),
		^^clean_file('test_files/tzif_input.tzif'),
		generated_directory(Root),
		^^clean_directory(Root).

	raw_bytes_zone('America/New_York').

	raw_file_zone('America/New_York').

	raw_stream_zone('US/Eastern').

	tzif_for_zone([TZif| _], Zone, TZif) :-
		TZif = tzif(Zone, _, _),
		!.
	tzif_for_zone([_| TZifs], Zone, TZif) :-
		tzif_for_zone(TZifs, Zone, TZif).

	test(tzif_load_v1_2_01, deterministic(TimeType == time_type(3600, false, 'LMT', type_0))) :-
		v1_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), [TZif]),
		tzif::time_type(TZif, Zone, 0, TimeType).

	test(tzif_load_v1_leaps_2_01, deterministic(Leaps == [leap_second(1000000000, 1)])) :-
		v1_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), [TZif]),
		tzif::zone(TZif, Zone, zone_data(_, _, _, leap_seconds(Leaps), _, _, _)).

	test(tzif_load_file_source_2_01, deterministic(TimeType == time_type(-18000, false, 'EST', type_0))) :-
		v2_fixture_bytes(Bytes),
		^^file_path('test_files/tzif_input.tzif', Path),
		^^create_binary_file(Path, Bytes),
		raw_file_zone(Zone),
		tzif::load(file(Path, Zone), [TZif]),
		tzif::time_type(TZif, Zone, 900, TimeType).

	test(tzif_load_stream_source_2_01, deterministic(TimeType == time_type(-14400, true, 'EDT', transition(1000)))) :-
		v2_fixture_bytes(Bytes),
		^^file_path('test_files/tzif_input.tzif', Path),
		^^create_binary_file(Path, Bytes),
		open(Path, read, Stream, [type(binary)]),
		raw_stream_zone(Zone),
		tzif::load(stream(Stream, Zone), [TZif]),
		close(Stream),
		tzif::time_type(TZif, Zone, 1000, TimeType).

	test(tzif_zones_single_file_2_01, deterministic(Zones == [Zone])) :-
		v2_fixture_bytes(Bytes),
		^^file_path('test_files/tzif_input.tzif', Path),
		^^create_binary_file(Path, Bytes),
		raw_file_zone(Zone),
		tzif::load(file(Path, Zone), TZifs),
		tzif::zones(TZifs, Zones).

	test(tzif_load_1_caches_2_01, deterministic(TimeType == time_type(-14400, true, 'EDT', footer))) :-
		v2_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone)),
		tzif::time_type(date_time(2024, 7, 1, 12, 0, 0), TimeType).

	test(tzif_cache_1_caches_loaded_term_2_01, deterministic(TimeType == time_type(-14400, true, 'EDT', footer))) :-
		v2_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), TZifs),
		tzif::cache(TZifs),
		tzif::time_type(date_time(2024, 7, 1, 12, 0, 0), TimeType).

	test(tzif_v2_footer_winter_2_01, deterministic(TimeType == time_type(-18000, false, 'EST', footer))) :-
		v2_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), [TZif]),
		tzif::time_type(TZif, Zone, date_time(2024, 1, 15, 12, 0, 0), TimeType).

	test(tzif_v2_footer_summer_2_01, deterministic(TimeType == time_type(-14400, true, 'EDT', footer))) :-
		v2_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), [TZif]),
		tzif::time_type(TZif, Zone, date_time(2024, 7, 1, 12, 0, 0), TimeType).

	test(tzif_v3_signed_hours_2_01, deterministic(Before-After == false-true)) :-
		v3_signed_hours_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), [TZif]),
		tzif::daylight_saving_time(TZif, Zone, date_time(2024, 3, 30, 21, 59, 0), Before),
		tzif::daylight_saving_time(TZif, Zone, date_time(2024, 3, 30, 22, 0, 0), After).

	test(tzif_signed_offset_minutes_footer_2_01, deterministic(TimeType == time_type(20700, false, '+0545', footer))) :-
		signed_offset_minutes_footer_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), [TZif]),
		tzif::time_type(TZif, Zone, date_time(2024, 1, 15, 12, 0, 0), TimeType).

	test(tzif_no_transitions_footer_2_01, deterministic(TimeType == time_type(-14400, true, 'EDT', footer))) :-
		no_transitions_footer_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), [TZif]),
		tzif::time_type(TZif, Zone, date_time(2024, 7, 1, 12, 0, 0), TimeType).

	test(tzif_no_transitions_empty_footer_2_01, deterministic(TimeType == time_type(5400, false, 'XST', type_0))) :-
		no_transitions_empty_footer_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), [TZif]),
		tzif::time_type(TZif, Zone, 0, TimeType).

	test(tzif_overlapping_designations_2_01, deterministic(Abbreviation0-Abbreviation1 == 'EST'-'ST')) :-
		overlapping_designations_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), [TZif]),
		tzif::abbreviation(TZif, Zone, 0, Abbreviation0),
		tzif::abbreviation(TZif, Zone, 1, Abbreviation1).

	test(tzif_load_files_source_2_01, deterministic(UTCOffset-SummerOffset == 3600-(-14400))) :-
		generated_database_fixture(Root),
		tzif::load(files(Root, ['Europe/London', 'America/New_York']), TZifs),
		tzif_for_zone(TZifs, 'Europe/London', LondonTZif),
		tzif_for_zone(TZifs, 'America/New_York', NewYorkTZif),
		tzif::offset(LondonTZif, 'Europe/London', 0, UTCOffset),
		tzif::offset(NewYorkTZif, 'America/New_York', date_time(2024, 7, 1, 12, 0, 0), SummerOffset).

	test(tzif_load_directory_source_2_01, deterministic) :-
		generated_database_fixture(Root),
		tzif::load(directory(Root), TZifs),
		tzif::zones(TZifs, Zones),
		list::memberchk('Europe/London', Zones),
		list::memberchk('America/New_York', Zones).

	test(tzif_save_load_cached_queries_2_01, deterministic(Explicit-Cached == time_type(-14400, true, 'EDT', footer)-time_type(-14400, true, 'EDT', footer))) :-
		v2_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), TZifs),
		^^file_path('test_files/tzif_snapshot.pl', Path),
		tzif::save(TZifs, Path),
		tzif::load(snapshot(Path), LoadedTZifs),
		tzif::cache(LoadedTZifs),
		LoadedTZifs = [LoadedTZif],
		tzif::time_type(LoadedTZif, Zone, date_time(2024, 7, 1, 12, 0, 0), Explicit),
		tzif::time_type(date_time(2024, 7, 1, 12, 0, 0), Cached).

	test(tzif_save_cached_tzif_2_01, deterministic(TimeType == time_type(-14400, true, 'EDT', footer))) :-
		v2_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone)),
		^^file_path('test_files/tzif_snapshot.pl', Path),
		tzif::save(Path),
		tzif::load(snapshot(Path), [LoadedTZif]),
		tzif::time_type(LoadedTZif, Zone, date_time(2024, 7, 1, 12, 0, 0), TimeType).

	test(tzif_cache_source_2_01, deterministic(Source == bytes(Bytes, Zone))) :-
		v2_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), TZifs),
		^^file_path('test_files/tzif_snapshot.pl', Path),
		tzif::save(TZifs, Path),
		tzif::load(snapshot(Path), LoadedTZifs),
		tzif::cache(LoadedTZifs),
		tzif::cache_source(Source).

	test(tzif_cached_tzif_2_01, deterministic(Zones == [Zone])) :-
		v2_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone)),
		tzif::cached_tzif(TZif),
		tzif::zones([TZif], Zones).

	test(tzif_cached_zones_2_01, deterministic(Zones == [Zone])) :-
		v2_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone)),
		tzif::zones(Zones).

	test(tzif_cached_load_accumulates_zones_2_01, deterministic(Zones == ['America/New_York', 'Europe/London'])) :-
		v2_fixture_bytes(Bytes),
		generated_database_fixture(Root),
		os::path_concat(Root, 'Europe', EuropeDirectory),
		os::path_concat(EuropeDirectory, 'London', LondonPath),
		tzif::clear_cache,
		tzif::load(bytes(Bytes, 'America/New_York')),
		tzif::load(file(LondonPath, 'Europe/London')),
		tzif::zones(Zones).

	test(tzif_cached_load_replaces_zone_2_01, deterministic(Zone-Offset == 'America/New_York'-(-18000))) :-
		v1_fixture_bytes(OldBytes),
		v2_fixture_bytes(NewBytes),
		tzif::clear_cache,
		tzif::load(bytes(OldBytes, 'America/New_York')),
		tzif::load(bytes(NewBytes, 'America/New_York')),
		tzif::zones([Zone]),
		tzif::offset('America/New_York', 900, Offset).

	test(tzif_save_cached_multi_zone_snapshot_2_01, deterministic(One-Two-UTCOffset-SummerOffset == true-true-3600-(-14400))) :-
		generated_database_fixture(Root),
		tzif::clear_cache,
		tzif::load(directory(Root)),
		^^file_path('test_files/tzif_snapshot.pl', Path),
		tzif::save(Path),
		tzif::clear_cache,
		tzif::load(snapshot(Path), LoadedTZifs),
		tzif::zones(LoadedTZifs, Zones),
		list::memberchk('Europe/London', Zones),
		list::memberchk('America/New_York', Zones),
		tzif_for_zone(LoadedTZifs, 'Europe/London', LondonTZif),
		tzif_for_zone(LoadedTZifs, 'America/New_York', NewYorkTZif),
		One = true,
		Two = true,
		tzif::offset(LondonTZif, 'Europe/London', 0, UTCOffset),
		tzif::offset(NewYorkTZif, 'America/New_York', date_time(2024, 7, 1, 12, 0, 0), SummerOffset).

	test(tzif_cached_single_zone_convenience_requires_one_zone_2_01, error(domain_error(single_zone_tzif_cache, _))) :-
		generated_database_fixture(Root),
		tzif::load(directory(Root)),
		tzif::time_type(0, _).

	test(tzif_cached_zone_queries_2_01, deterministic(Offset-IsDST-Abbreviation == -14400-true-'EDT')) :-
		v2_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone)),
		tzif::offset(Zone, date_time(2024, 7, 1, 12, 0, 0), Offset),
		tzif::daylight_saving_time(Zone, date_time(2024, 7, 1, 12, 0, 0), IsDST),
		tzif::abbreviation(Zone, date_time(2024, 7, 1, 12, 0, 0), Abbreviation).

	test(tzif_local_cached_zone_queries_2_01, deterministic(Offset-IsDST-Abbreviation == -14400-true-'EDT')) :-
		real_tzif_file(new_york, Path),
		tzif::clear_cache,
		tzif::load(file(Path, 'America/New_York')),
		tzif::local_offset('America/New_York', date_time(2024, 7, 1, 12, 0, 0), Offset),
		tzif::local_daylight_saving_time('America/New_York', date_time(2024, 7, 1, 12, 0, 0), IsDST),
		tzif::local_abbreviation('America/New_York', date_time(2024, 7, 1, 12, 0, 0), Abbreviation).

	test(tzif_local_cached_single_zone_convenience_2_01, deterministic(Offset-IsDST-Abbreviation == 0-false-'UTC')) :-
		real_tzif_file(utc, Path),
		tzif::clear_cache,
		tzif::load(file(Path, 'Etc/UTC')),
		tzif::local_offset(date_time(2024, 1, 15, 12, 0, 0), Offset),
		tzif::local_daylight_saving_time(date_time(2024, 1, 15, 12, 0, 0), IsDST),
		tzif::local_abbreviation(date_time(2024, 1, 15, 12, 0, 0), Abbreviation).

	test(tzif_local_time_type_reified_cached_zone_2_01, deterministic(Offset-IsDST-Abbreviation == -14400-true-'EDT')) :-
		real_tzif_file(new_york, Path),
		tzif::clear_cache,
		tzif::load(file(Path, 'America/New_York')),
		tzif::local_time_type_reified('America/New_York', date_time(2024, 7, 1, 12, 0, 0), unique(time_type(Offset, IsDST, Abbreviation, _))).

	test(tzif_local_reified_cached_single_zone_convenience_2_01, deterministic(OffsetResult-DSTResult-AbbreviationResult == unique(0)-unique(false)-unique('UTC'))) :-
		real_tzif_file(utc, Path),
		tzif::clear_cache,
		tzif::load(file(Path, 'Etc/UTC')),
		tzif::local_offset_reified(date_time(2024, 1, 15, 12, 0, 0), OffsetResult),
		tzif::local_daylight_saving_time_reified(date_time(2024, 1, 15, 12, 0, 0), DSTResult),
		tzif::local_abbreviation_reified(date_time(2024, 1, 15, 12, 0, 0), AbbreviationResult).

	test(tzif_load_2_without_cache_2_01, error(existence_error(tzif_cache, tzif))) :-
		tzif::clear_cache,
		v2_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), _),
		tzif::time_type(date_time(2024, 7, 1, 12, 0, 0), _).

	test(tzif_invalid_single_source_zone_id_2_01, error(domain_error(time_zone, 'Test/Bytes'))) :-
		v2_fixture_bytes(Bytes),
		tzif::load(bytes(Bytes, 'Test/Bytes'), _).

	test(tzif_invalid_files_zone_id_2_01, error(domain_error(time_zone, 'Invalid/Zone'))) :-
		generated_database_fixture(Root),
		tzif::load(files(Root, ['Invalid/Zone']), _).

	test(tzif_directory_ignores_non_zone_files_2_01, deterministic(Zones == ['America/New_York'])) :-
		generated_database_with_non_tzif_files_fixture(Root),
		tzif::load(directory(Root), TZifs),
		tzif::zones(TZifs, Zones).

	test(tzif_clear_cache_2_01, error(existence_error(tzif_cache, tzif))) :-
		tzif::clear_cache,
		tzif::time_type(0, _).

	test(tzif_zones_without_cache_2_01, error(existence_error(tzif_cache, tzif))) :-
		tzif::clear_cache,
		tzif::zones(_).

	test(tzif_save_without_cache_2_01, error(existence_error(tzif_cache, tzif))) :-
		tzif::clear_cache,
		^^file_path('test_files/tzif_snapshot.pl', Path),
		tzif::save(Path).

	test(tzif_cache_invalid_term_2_01, error(domain_error(tzif, not_a_tzif))) :-
		tzif::cache([not_a_tzif]).

	test(tzif_invalid_snapshot_2_01, error(domain_error(tzif, not_a_tzif))) :-
		^^file_path('test_files/tzif_snapshot.pl', Path),
		open(Path, write, Stream),
		write(Stream, not_a_tzif),
		write(Stream, '.'),
		nl(Stream),
		close(Stream),
		tzif::load(snapshot(Path), _).

	test(tzif_malformed_type_index_2_01, fail) :-
		malformed_type_index_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), _).

	test(tzif_truncated_v2_2_01, fail) :-
		truncated_v2_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), _).

	test(tzif_real_etc_utc_2_01, deterministic(Offset-IsDST-Abbreviation == 0-false-'UTC')) :-
		real_tzif_file(utc, Path),
		tzif::load(file(Path, 'Etc/UTC'), [TZif]),
		tzif::offset(TZif, 'Etc/UTC', date_time(2024, 1, 15, 12, 0, 0), Offset),
		tzif::daylight_saving_time(TZif, 'Etc/UTC', date_time(2024, 1, 15, 12, 0, 0), IsDST),
		tzif::abbreviation(TZif, 'Etc/UTC', date_time(2024, 1, 15, 12, 0, 0), Abbreviation).

	test(tzif_real_america_new_york_winter_2_01, deterministic(Offset-IsDST-Abbreviation == -18000-false-'EST')) :-
		real_tzif_file(new_york, Path),
		tzif::load(file(Path, 'America/New_York'), [TZif]),
		tzif::offset(TZif, 'America/New_York', date_time(2024, 1, 15, 12, 0, 0), Offset),
		tzif::daylight_saving_time(TZif, 'America/New_York', date_time(2024, 1, 15, 12, 0, 0), IsDST),
		tzif::abbreviation(TZif, 'America/New_York', date_time(2024, 1, 15, 12, 0, 0), Abbreviation).

	test(tzif_real_america_new_york_summer_2_01, deterministic(Offset-IsDST-Abbreviation == -14400-true-'EDT')) :-
		real_tzif_file(new_york, Path),
		tzif::load(file(Path, 'America/New_York'), [TZif]),
		tzif::offset(TZif, 'America/New_York', date_time(2024, 7, 1, 12, 0, 0), Offset),
		tzif::daylight_saving_time(TZif, 'America/New_York', date_time(2024, 7, 1, 12, 0, 0), IsDST),
		tzif::abbreviation(TZif, 'America/New_York', date_time(2024, 7, 1, 12, 0, 0), Abbreviation).

	test(tzif_local_real_america_new_york_gap_strict_2_01, fail) :-
		real_tzif_file(new_york, Path),
		tzif::load(file(Path, 'America/New_York'), [TZif]),
		tzif::local_time_type(TZif, 'America/New_York', date_time(2024, 3, 10, 2, 30, 0), _).

	test(tzif_local_real_america_new_york_gap_reified_2_01, deterministic(Result == nonexistent)) :-
		real_tzif_file(new_york, Path),
		tzif::load(file(Path, 'America/New_York'), [TZif]),
		tzif::local_time_type_reified(TZif, 'America/New_York', date_time(2024, 3, 10, 2, 30, 0), Result).

	test(tzif_local_real_america_new_york_fold_strict_2_01, fail) :-
		real_tzif_file(new_york, Path),
		tzif::load(file(Path, 'America/New_York'), [TZif]),
		tzif::local_time_type(TZif, 'America/New_York', date_time(2024, 11, 3, 1, 30, 0), _).

	test(tzif_local_real_america_new_york_fold_time_type_reified_2_01, deterministic(Results == [-14400-true-'EDT', -18000-false-'EST'])) :-
		real_tzif_file(new_york, Path),
		tzif::load(file(Path, 'America/New_York'), [TZif]),
		tzif::local_time_type_reified(
			TZif,
			'America/New_York',
			date_time(2024, 11, 3, 1, 30, 0),
			ambiguous([
				time_type(Offset1, IsDST1, Abbreviation1, _),
				time_type(Offset2, IsDST2, Abbreviation2, _)
			])
		),
		Results = [Offset1-IsDST1-Abbreviation1, Offset2-IsDST2-Abbreviation2].

	test(tzif_local_real_america_new_york_fold_projected_reified_2_01, deterministic(OffsetResult-DSTResult-AbbreviationResult == ambiguous([-14400, -18000])-ambiguous([true, false])-ambiguous(['EDT', 'EST']))) :-
		real_tzif_file(new_york, Path),
		tzif::load(file(Path, 'America/New_York'), [TZif]),
		tzif::local_offset_reified(TZif, 'America/New_York', date_time(2024, 11, 3, 1, 30, 0), OffsetResult),
		tzif::local_daylight_saving_time_reified(TZif, 'America/New_York', date_time(2024, 11, 3, 1, 30, 0), DSTResult),
		tzif::local_abbreviation_reified(TZif, 'America/New_York', date_time(2024, 11, 3, 1, 30, 0), AbbreviationResult).

	test(tzif_local_real_america_new_york_fold_first_2_01, deterministic(Offset-IsDST-Abbreviation == -14400-true-'EDT')) :-
		real_tzif_file(new_york, Path),
		tzif::load(file(Path, 'America/New_York'), [TZif]),
		tzif::local_offset_with_resolution(TZif, 'America/New_York', date_time(2024, 11, 3, 1, 30, 0), first, Offset),
		tzif::local_daylight_saving_time_with_resolution(TZif, 'America/New_York', date_time(2024, 11, 3, 1, 30, 0), first, IsDST),
		tzif::local_abbreviation_with_resolution(TZif, 'America/New_York', date_time(2024, 11, 3, 1, 30, 0), first, Abbreviation).

	test(tzif_local_real_america_new_york_fold_second_2_01, deterministic(Offset-IsDST-Abbreviation == -18000-false-'EST')) :-
		real_tzif_file(new_york, Path),
		tzif::load(file(Path, 'America/New_York'), [TZif]),
		tzif::local_offset_with_resolution(TZif, 'America/New_York', date_time(2024, 11, 3, 1, 30, 0), second, Offset),
		tzif::local_daylight_saving_time_with_resolution(TZif, 'America/New_York', date_time(2024, 11, 3, 1, 30, 0), second, IsDST),
		tzif::local_abbreviation_with_resolution(TZif, 'America/New_York', date_time(2024, 11, 3, 1, 30, 0), second, Abbreviation).

	test(tzif_local_real_america_new_york_fold_all_2_01, deterministic(Results == [-14400-true-'EDT', -18000-false-'EST'])) :-
		real_tzif_file(new_york, Path),
		tzif::load(file(Path, 'America/New_York'), [TZif]),
		findall(Offset-IsDST-Abbreviation,
				(tzif::local_time_type_with_resolution(TZif, 'America/New_York', date_time(2024, 11, 3, 1, 30, 0), all, time_type(Offset, IsDST, Abbreviation, _))),
				Results).

	test(tzif_local_footer_gap_strict_2_01, fail) :-
		v2_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), [TZif]),
		tzif::local_time_type(TZif, Zone, date_time(2024, 3, 10, 2, 30, 0), _).

	test(tzif_local_footer_gap_reified_2_01, deterministic(Result == nonexistent)) :-
		v2_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), [TZif]),
		tzif::local_time_type_reified(TZif, Zone, date_time(2024, 3, 10, 2, 30, 0), Result).

	test(tzif_local_footer_fold_all_2_01, deterministic(Results == [-14400-true-'EDT', -18000-false-'EST'])) :-
		v2_fixture_bytes(Bytes),
		raw_bytes_zone(Zone),
		tzif::load(bytes(Bytes, Zone), [TZif]),
		findall(Offset-IsDST-Abbreviation,
				(tzif::local_time_type_with_resolution(TZif, Zone, date_time(2024, 11, 3, 1, 30, 0), all, time_type(Offset, IsDST, Abbreviation, _))),
				Results).

	test(tzif_real_asia_kathmandu_2_01, deterministic(Offset-IsDST-Abbreviation == 20700-false-'+0545')) :-
		real_tzif_file(kathmandu, Path),
		tzif::load(file(Path, 'Asia/Kathmandu'), [TZif]),
		tzif::offset(TZif, 'Asia/Kathmandu', date_time(2024, 1, 15, 12, 0, 0), Offset),
		tzif::daylight_saving_time(TZif, 'Asia/Kathmandu', date_time(2024, 1, 15, 12, 0, 0), IsDST),
		tzif::abbreviation(TZif, 'Asia/Kathmandu', date_time(2024, 1, 15, 12, 0, 0), Abbreviation).

	v1_fixture_bytes(Bytes) :-
		Block = block_spec([], [type_spec(3600, false, 0)], [0'L, 0'M, 0'T, 0], [leap_spec(1000000000, 1)], [], []),
		build_v1_tzif(Block, Bytes).

	v2_fixture_bytes(Bytes) :-
		Compatibility = block_spec([], [type_spec(1111, false, 0)], [0'O, 0'L, 0'D, 0], [], [], []),
		Active = block_spec([
			transition_spec(1000, 1)
		], [
			type_spec(-18000, false, 0),
			type_spec(-14400, true, 4)
		], [0'E, 0'S, 0'T, 0, 0'E, 0'D, 0'T, 0], [], [wall, wall], [local, local]),
		build_v2_or_v3_tzif(2, Compatibility, Active, 'EST5EDT,M3.2.0/2,M11.1.0/2', Bytes).

	v3_signed_hours_fixture_bytes(Bytes) :-
		Compatibility = block_spec([], [type_spec(0, false, 0)], [0'O, 0'L, 0'D, 0], [], [], []),
		Active = block_spec([
			transition_spec(0, 0)
		], [
			type_spec(3600, false, 0),
			type_spec(7200, true, 4)
		], [0'X, 0'S, 0'T, 0, 0'X, 0'D, 0'T, 0], [], [wall, wall], [local, local]),
		build_v2_or_v3_tzif(3, Compatibility, Active, 'XST-1XDT,M3.5.0/-1,M10.5.0/-2', Bytes).

	signed_offset_minutes_footer_fixture_bytes(Bytes) :-
		Compatibility = block_spec([], [type_spec(0, false, 0)], [0'O, 0'L, 0'D, 0], [], [], []),
		Active = block_spec([], [type_spec(0, false, 0)], [0'X, 0'S, 0'T, 0], [], [], []),
		build_v2_or_v3_tzif(2, Compatibility, Active, '<+0545>-5:45', Bytes).

	no_transitions_footer_fixture_bytes(Bytes) :-
		Compatibility = block_spec([], [type_spec(0, false, 0)], [0'Z, 0'E, 0'R, 0'0, 0], [], [], []),
		Active = block_spec([], [type_spec(-18000, false, 0)], [0'E, 0'S, 0'T, 0], [], [], []),
		build_v2_or_v3_tzif(2, Compatibility, Active, 'EST5EDT,M3.2.0/2,M11.1.0/2', Bytes).

	no_transitions_empty_footer_fixture_bytes(Bytes) :-
		Compatibility = block_spec([], [type_spec(0, false, 0)], [0'O, 0'L, 0'D, 0], [], [], []),
		Active = block_spec([], [type_spec(5400, false, 0)], [0'X, 0'S, 0'T, 0], [], [], []),
		build_v2_or_v3_tzif(2, Compatibility, Active, '', Bytes).

	overlapping_designations_fixture_bytes(Bytes) :-
		Block = block_spec([
			transition_spec(1, 1)
		], [
			type_spec(-18000, false, 0),
			type_spec(-18000, false, 1)
		], [0'E, 0'S, 0'T, 0], [], [], []),
		build_v1_tzif(Block, Bytes).

	malformed_type_index_bytes(Bytes) :-
		Block = block_spec([
			transition_spec(10, 2)
		], [
			type_spec(0, false, 0),
			type_spec(3600, true, 4)
		], [0'A, 0'B, 0'C, 0, 0'D, 0'E, 0'F, 0], [], [], []),
		build_v1_tzif(Block, Bytes).

	truncated_v2_bytes(Bytes) :-
		v2_fixture_bytes(FullBytes),
		remove_last_bytes(FullBytes, 6, Bytes).

	build_v1_tzif(Block, Bytes) :-
		block_bytes(4, Block, DataBytes, Counts),
		header_bytes(0, Counts, HeaderBytes),
		list::append(HeaderBytes, DataBytes, Bytes).

	build_v2_or_v3_tzif(Version, CompatibilityBlock, ActiveBlock, FooterAtom, Bytes) :-
		block_bytes(4, CompatibilityBlock, CompatibilityBytes, CompatibilityCounts),
		block_bytes(8, ActiveBlock, ActiveBytes, ActiveCounts),
		version_byte(Version, VersionByte),
		header_bytes(VersionByte, CompatibilityCounts, Header1),
		header_bytes(VersionByte, ActiveCounts, Header2),
		footer_bytes(FooterAtom, FooterBytes),
		list::append(Header1, CompatibilityBytes, Prefix),
		list::append(Prefix, Header2, Prefix2),
		list::append(Prefix2, ActiveBytes, Prefix3),
		list::append(Prefix3, FooterBytes, Bytes).

	version_byte(2, 0'2).
	version_byte(3, 0'3).

	block_bytes(TimeSize, block_spec(Transitions, Types, Designations, Leaps, StandardWalls, UTLocals), Bytes,
			counts(UTLocalCount, StandardWallCount, LeapCount, TransitionCount, TypeCount, CharCount)) :-
		transitions_bytes(TimeSize, Transitions, TransitionTimesBytes, TransitionIndexesBytes, TransitionCount),
		type_specs_bytes(Types, TypeBytes, 0, TypeCount),
		leap_specs_bytes(Leaps, TimeSize, LeapBytes, 0, LeapCount),
		indicator_bytes(standard, StandardWalls, StandardWallBytes, StandardWallCount),
		indicator_bytes(ut_local, UTLocals, UTLocalBytes, UTLocalCount),
		list::length(Designations, CharCount),
		list::append(TransitionTimesBytes, TransitionIndexesBytes, Prefix1),
		list::append(Prefix1, TypeBytes, Prefix2),
		list::append(Prefix2, Designations, Prefix3),
		list::append(Prefix3, LeapBytes, Prefix4),
		list::append(Prefix4, StandardWallBytes, Prefix5),
		list::append(Prefix5, UTLocalBytes, Bytes).

	transitions_bytes(TimeSize, Transitions, TransitionTimesBytes, TransitionIndexesBytes, Count) :-
		transition_times_bytes(Transitions, TimeSize, TransitionTimesBytes),
		transition_indexes_bytes(Transitions, TransitionIndexesBytes),
		list::length(Transitions, Count).

	transition_times_bytes([], _, []).
	transition_times_bytes([transition_spec(Time, _)| Transitions], TimeSize, Bytes) :-
		signed_bytes(TimeSize, Time, TransitionBytes),
		list::append(TransitionBytes, RestBytes, Bytes),
		transition_times_bytes(Transitions, TimeSize, RestBytes).

	transition_indexes_bytes([], []).
	transition_indexes_bytes([transition_spec(_, TypeIndex)| Transitions], [TypeIndex| Bytes]) :-
		transition_indexes_bytes(Transitions, Bytes).

	type_specs_bytes([], [], Count, Count).
	type_specs_bytes([type_spec(OffsetSeconds, IsDST, AbbreviationIndex)| Types], Bytes, Count0, Count) :-
		signed_bytes(4, OffsetSeconds, OffsetBytes),
		bool_byte(IsDST, IsDSTByte),
		list::append(OffsetBytes, [IsDSTByte, AbbreviationIndex], Prefix),
		list::append(Prefix, RestBytes, Bytes),
		Count1 is Count0 + 1,
		type_specs_bytes(Types, RestBytes, Count1, Count).

	bool_byte(true,  1).
	bool_byte(false, 0).

	leap_specs_bytes([], _, [], Count, Count).
	leap_specs_bytes([leap_spec(Occurrence, Correction)| Leaps], TimeSize, Bytes, Count0, Count) :-
		signed_bytes(TimeSize, Occurrence, OccurrenceBytes),
		signed_bytes(4, Correction, CorrectionBytes),
		list::append(OccurrenceBytes, CorrectionBytes, Prefix),
		list::append(Prefix, RestBytes, Bytes),
		Count1 is Count0 + 1,
		leap_specs_bytes(Leaps, TimeSize, RestBytes, Count1, Count).

	indicator_bytes(standard, Indicators, Bytes, Count) :-
		standard_indicator_bytes(Indicators, Bytes),
		list::length(Indicators, Count).
	indicator_bytes(ut_local, Indicators, Bytes, Count) :-
		ut_local_indicator_bytes(Indicators, Bytes),
		list::length(Indicators, Count).

	standard_indicator_bytes([], []).
	standard_indicator_bytes([wall| Indicators], [0| Bytes]) :-
		!,
		standard_indicator_bytes(Indicators, Bytes).
	standard_indicator_bytes([standard| Indicators], [1| Bytes]) :-
		standard_indicator_bytes(Indicators, Bytes).

	ut_local_indicator_bytes([], []).
	ut_local_indicator_bytes([local| Indicators], [0| Bytes]) :-
		!,
		ut_local_indicator_bytes(Indicators, Bytes).
	ut_local_indicator_bytes([ut| Indicators], [1| Bytes]) :-
		ut_local_indicator_bytes(Indicators, Bytes).

	header_bytes(VersionByte, counts(UTLocalCount, StandardWallCount, LeapCount, TransitionCount, TypeCount, CharCount), Bytes) :-
		unsigned_bytes(4, UTLocalCount, UTLocalBytes),
		unsigned_bytes(4, StandardWallCount, StandardWallBytes),
		unsigned_bytes(4, LeapCount, LeapBytes),
		unsigned_bytes(4, TransitionCount, TransitionBytes),
		unsigned_bytes(4, TypeCount, TypeBytes),
		unsigned_bytes(4, CharCount, CharBytes),
		zeros(15, Reserved),
		Prefix = [0'T, 0'Z, 0'i, 0'f, VersionByte| Reserved],
		list::append(Prefix, UTLocalBytes, Prefix1),
		list::append(Prefix1, StandardWallBytes, Prefix2),
		list::append(Prefix2, LeapBytes, Prefix3),
		list::append(Prefix3, TransitionBytes, Prefix4),
		list::append(Prefix4, TypeBytes, Prefix5),
		list::append(Prefix5, CharBytes, Bytes).

	footer_bytes('', [10, 10]) :-
		!.
	footer_bytes(FooterAtom, Bytes) :-
		atom_codes(FooterAtom, FooterCodes),
		Prefix = [10| FooterCodes],
		list::append(Prefix, [10], Bytes).

	unsigned_bytes(Size, Value, Bytes) :-
		unsigned_bytes(Size, Value, [], Bytes).

	unsigned_bytes(0, _, Bytes, Bytes) :-
		!.
	unsigned_bytes(Size, Value, Accumulator, Bytes) :-
		Size > 0,
		Shift is (Size - 1) * 8,
		Byte is (Value >> Shift) /\ 255,
		list::append(Accumulator, [Byte], NextAccumulator),
		NextSize is Size - 1,
		unsigned_bytes(NextSize, Value, NextAccumulator, Bytes).

	signed_bytes(Size, Value, Bytes) :-
		Bits is Size * 8,
		(	Value < 0 ->
			UnsignedValue is (1 << Bits) + Value
		;	UnsignedValue = Value
		),
		unsigned_bytes(Size, UnsignedValue, Bytes).

	zeros(0, []) :-
		!.
	zeros(Count, [0| Bytes]) :-
		Count > 0,
		NextCount is Count - 1,
		zeros(NextCount, Bytes).

	remove_last_bytes(Bytes, 0, Bytes) :-
		!.
	remove_last_bytes(Bytes, Count, Rest) :-
		Count > 0,
		list::append(Bytees, [_], Bytes),
		NextCount is Count - 1,
		remove_last_bytes(Bytees, NextCount, Rest).

	generated_database_fixture(Root) :-
		generated_directory(Root),
		^^clean_directory(Root),
		os::ensure_directory(Root),
		os::path_concat(Root, 'Europe', EuropeDirectory),
		os::path_concat(Root, 'America', AmericaDirectory),
		os::ensure_directory(EuropeDirectory),
		os::ensure_directory(AmericaDirectory),
		v1_fixture_bytes(LondonBytes),
		v2_fixture_bytes(NewYorkBytes),
		os::path_concat(EuropeDirectory, 'London', LondonPath),
		os::path_concat(AmericaDirectory, 'New_York', NewYorkPath),
		^^create_binary_file(LondonPath, LondonBytes),
		^^create_binary_file(NewYorkPath, NewYorkBytes).

	generated_database_with_non_tzif_files_fixture(Root) :-
		generated_directory(Root),
		^^clean_directory(Root),
		os::ensure_directory(Root),
		os::path_concat(Root, 'America', AmericaDirectory),
		os::ensure_directory(AmericaDirectory),
		v2_fixture_bytes(Bytes),
		os::path_concat(AmericaDirectory, 'New_York', Path),
		^^create_binary_file(Path, Bytes),
		os::path_concat(Root, 'leapseconds', MetadataPath),
		^^create_binary_file(MetadataPath, Bytes).

	generated_directory(Root) :-
		^^file_path('test_files/generated', Root).

	real_tzif_file(utc, Path) :-
		^^file_path('test_files/Etc/UTC', Path).
	real_tzif_file(new_york, Path) :-
		^^file_path('test_files/America/New_York', Path).
	real_tzif_file(kathmandu, Path) :-
		^^file_path('test_files/Asia/Kathmandu', Path).

:- end_object.
