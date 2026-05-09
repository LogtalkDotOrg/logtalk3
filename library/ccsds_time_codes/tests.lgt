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
		date is 2026-05-08,
		comment is 'Unit tests for the "ccsds_time_codes" library.'
	]).

	:- uses(ccsds_time_codes, [
		valid/1, format/2
	]).

	cover(ccsds_time_codes).
	cover(ccsds_cuc(_, _, _)).
	cover(ccsds_cds(_, _, _)).
	cover(ccsds_ccs(_, _)).
	cover(ccsds_time_codes_types).

	test(ccsds_time_codes_valid_1_01, deterministic) :-
		valid(cuc_time(256, 128)).

	test(ccsds_time_codes_valid_1_02, deterministic) :-
		valid(cds_time(1, 2000)).

	test(ccsds_time_codes_format_2_01, deterministic(Format == cuc)) :-
		format(cuc_time(256, 128), Format).

	test(ccsds_time_codes_format_2_02, deterministic(Format == cds)) :-
		format(cds_time(1, 2000), Format).

	test(ccsds_time_codes_valid_1_03, deterministic) :-
		valid(ccs_calendar_time(2026, 5, 8, 14, 30, 45, 0)).

	test(ccsds_time_codes_valid_1_04, deterministic) :-
		valid(ccs_ordinal_time(2026, 128, 14, 30, 45, 0)).

	test(ccsds_time_codes_format_2_03, deterministic(Format == ccs)) :-
		format(ccs_calendar_time(2026, 5, 8, 14, 30, 45, 0), Format).

	test(ccsds_cuc_parse_2_01, true(TimeCode == cuc_time(256, 128))) :-
		ccsds_cuc(4, 2, ccsds_epoch)::parse(bytes([0x00, 0x00, 0x01, 0x00, 0x00, 0x80]), TimeCode).

	test(ccsds_cuc_parse_2_02, true(TimeCode == cuc_time(16909060, 1286))) :-
		ccsds_cuc(4, 2, ccsds_epoch)::parse(bytes([0x01, 0x02, 0x03, 0x04, 0x05, 0x06]), TimeCode).

	test(ccsds_cuc_parse_2_03, error(domain_error(ccsds_time_code_byte_sequence, _))) :-
		ccsds_cuc(4, 2, ccsds_epoch)::parse(bytes([0x00, 0x01]), _).

	test(ccsds_cuc_generate_2_01, true(Bytes == [0x00, 0x00, 0x01, 0x00, 0x00, 0x80])) :-
		ccsds_cuc(4, 2, ccsds_epoch)::generate(bytes(Bytes), cuc_time(256, 128)).

	test(ccsds_cuc_generate_3_01, true(Bytes == [0x00, 0x00, 0x01, 0x00, 0x00, 0x80, 0xFF])) :-
		ccsds_cuc(4, 2, ccsds_epoch)::generate(cuc_time(256, 128), Bytes, [0xFF]).

	test(ccsds_cuc_valid_1_01, deterministic) :-
		ccsds_cuc(4, 2, ccsds_epoch)::valid(cuc_time(256, 128)).

	test(ccsds_cuc_valid_1_02, fail) :-
		ccsds_cuc(4, 1, ccsds_epoch)::valid(cuc_time(256, 256)).

	test(ccsds_cuc_format_1_01, deterministic(Format == cuc)) :-
		ccsds_cuc(4, 2, ccsds_epoch)::format(Format).

	test(ccsds_cuc_epoch_1_01, deterministic(Epoch == ccsds_epoch)) :-
		ccsds_cuc(4, 2, ccsds_epoch)::epoch(Epoch).

	test(ccsds_cuc_coarse_2_01, deterministic(Coarse == 256)) :-
		ccsds_cuc(4, 2, ccsds_epoch)::coarse(cuc_time(256, 128), Coarse).

	test(ccsds_cuc_fine_2_01, deterministic(Fine == 128)) :-
		ccsds_cuc(4, 2, ccsds_epoch)::fine(cuc_time(256, 128), Fine).

	test(ccsds_cuc_unix_seconds_2_01, deterministic(Seconds =:= 1.5)) :-
		ccsds_cuc(4, 2, unix_epoch)::unix_seconds(cuc_time(1, 32768), Seconds).

	test(ccsds_cuc_unix_seconds_2_02, deterministic(Seconds =:= -378691200)) :-
		ccsds_cuc(4, 0, ccsds_epoch)::unix_seconds(cuc_time(0, 0), Seconds).

	test(ccsds_cuc_from_unix_seconds_2_01, deterministic(TimeCode == cuc_time(1, 32768))) :-
		ccsds_cuc(4, 2, unix_epoch)::from_unix_seconds(1.5, TimeCode).

	test(ccsds_cuc_from_unix_seconds_2_02, error(domain_error(ccsds_time_code_unix_seconds, _))) :-
		ccsds_cuc(4, 0, unix_epoch)::from_unix_seconds(-1, _).

	test(ccsds_cuc_roundtrip_01, true(Bytes == OriginalBytes)) :-
		OriginalBytes = [0x01, 0x02, 0x03, 0x04, 0x05, 0x06],
		ccsds_cuc(4, 2, ccsds_epoch)::parse(bytes(OriginalBytes), TimeCode),
		ccsds_cuc(4, 2, ccsds_epoch)::generate(bytes(Bytes), TimeCode).

	test(ccsds_cds_parse_2_01, true(TimeCode == cds_time(1, 2000))) :-
		ccsds_cds(2, 0, ccsds_epoch)::parse(bytes([0x00, 0x01, 0x00, 0x00, 0x07, 0xD0]), TimeCode).

	test(ccsds_cds_parse_2_02, true(TimeCode == cds_time(1, 2000, 500))) :-
		ccsds_cds(2, 2, unix_epoch)::parse(bytes([0x00, 0x01, 0x00, 0x00, 0x07, 0xD0, 0x01, 0xF4]), TimeCode).

	test(ccsds_cds_parse_2_03, true(TimeCode == cds_time(1, 2000, 500000000))) :-
		ccsds_cds(3, 4, unix_epoch)::parse(bytes([0x00, 0x00, 0x01, 0x00, 0x00, 0x07, 0xD0, 0x1D, 0xCD, 0x65, 0x00]), TimeCode).

	test(ccsds_cds_parse_2_04, error(domain_error(ccsds_time_code_byte_sequence, _))) :-
		ccsds_cds(2, 0, ccsds_epoch)::parse(bytes([0x00, 0x01]), _).

	test(ccsds_cds_generate_2_01, true(Bytes == [0x00, 0x01, 0x00, 0x00, 0x07, 0xD0])) :-
		ccsds_cds(2, 0, ccsds_epoch)::generate(bytes(Bytes), cds_time(1, 2000)).

	test(ccsds_cds_generate_2_02, true(Bytes == [0x00, 0x01, 0x00, 0x00, 0x07, 0xD0, 0x01, 0xF4])) :-
		ccsds_cds(2, 2, unix_epoch)::generate(bytes(Bytes), cds_time(1, 2000, 500)).

	test(ccsds_cds_generate_2_03, true(Bytes == [0x00, 0x00, 0x01, 0x00, 0x00, 0x07, 0xD0, 0x1D, 0xCD, 0x65, 0x00])) :-
		ccsds_cds(3, 4, unix_epoch)::generate(bytes(Bytes), cds_time(1, 2000, 500000000)).

	test(ccsds_cds_generate_3_01, true(Bytes == [0x00, 0x01, 0x00, 0x00, 0x07, 0xD0, 0xFF])) :-
		ccsds_cds(2, 0, ccsds_epoch)::generate(cds_time(1, 2000), Bytes, [0xFF]).

	test(ccsds_cds_valid_1_01, deterministic) :-
		ccsds_cds(2, 0, ccsds_epoch)::valid(cds_time(1, 2000)).

	test(ccsds_cds_valid_1_02, deterministic) :-
		ccsds_cds(2, 2, ccsds_epoch)::valid(cds_time(1, 2000, 500)).

	test(ccsds_cds_valid_1_03, fail) :-
		ccsds_cds(2, 2, ccsds_epoch)::valid(cds_time(1, 2000, 1000)).

	test(ccsds_cds_valid_1_04, deterministic) :-
		ccsds_cds(3, 4, ccsds_epoch)::valid(cds_time(1, 2000, 500000000)).

	test(ccsds_cds_valid_1_05, fail) :-
		ccsds_cds(3, 4, ccsds_epoch)::valid(cds_time(1, 2000, 1000000000)).

	test(ccsds_cds_format_1_01, deterministic(Format == cds)) :-
		ccsds_cds(2, 0, ccsds_epoch)::format(Format).

	test(ccsds_cds_epoch_1_01, deterministic(Epoch == ccsds_epoch)) :-
		ccsds_cds(2, 0, ccsds_epoch)::epoch(Epoch).

	test(ccsds_cds_days_2_01, deterministic(Days == 1)) :-
		ccsds_cds(2, 0, ccsds_epoch)::days(cds_time(1, 2000), Days).

	test(ccsds_cds_milliseconds_2_01, deterministic(Milliseconds == 2000)) :-
		ccsds_cds(2, 2, ccsds_epoch)::milliseconds(cds_time(1, 2000, 500), Milliseconds).

	test(ccsds_cds_submilliseconds_2_01, deterministic(Submilliseconds == 500)) :-
		ccsds_cds(2, 2, ccsds_epoch)::submilliseconds(cds_time(1, 2000, 500), Submilliseconds).

	test(ccsds_cds_submilliseconds_2_02, deterministic(Submilliseconds == 500000000)) :-
		ccsds_cds(3, 4, ccsds_epoch)::submilliseconds(cds_time(1, 2000, 500000000), Submilliseconds).

	test(ccsds_cds_unix_seconds_2_01, deterministic(Seconds =:= 86402.0)) :-
		ccsds_cds(2, 0, unix_epoch)::unix_seconds(cds_time(1, 2000), Seconds).

	test(ccsds_cds_unix_seconds_2_02, deterministic(Seconds =:= 86402.0005)) :-
		ccsds_cds(2, 2, unix_epoch)::unix_seconds(cds_time(1, 2000, 500), Seconds).

	test(ccsds_cds_unix_seconds_2_03, deterministic(Seconds =:= 86402.0005)) :-
		ccsds_cds(3, 4, unix_epoch)::unix_seconds(cds_time(1, 2000, 500000000), Seconds).

	test(ccsds_cds_unix_seconds_2_04, deterministic(Seconds =:= -378691200)) :-
		ccsds_cds(2, 0, ccsds_epoch)::unix_seconds(cds_time(0, 0), Seconds).

	test(ccsds_cds_from_unix_seconds_2_01, deterministic(TimeCode == cds_time(1, 2000))) :-
		ccsds_cds(2, 0, unix_epoch)::from_unix_seconds(86402.0, TimeCode).

	test(ccsds_cds_from_unix_seconds_2_02, deterministic(TimeCode == cds_time(1, 2000, 500))) :-
		ccsds_cds(2, 2, unix_epoch)::from_unix_seconds(86402.0005, TimeCode).

	test(ccsds_cds_from_unix_seconds_2_03, deterministic(TimeCode == cds_time(1, 2000, 500000000))) :-
		ccsds_cds(3, 4, unix_epoch)::from_unix_seconds(86402.0005, TimeCode).

	test(ccsds_cds_from_unix_seconds_2_04, error(domain_error(ccsds_time_code_unix_seconds, _))) :-
		ccsds_cds(2, 0, unix_epoch)::from_unix_seconds(-1, _).

	test(ccsds_cds_roundtrip_01, true(Bytes == OriginalBytes)) :-
		OriginalBytes = [0x00, 0x01, 0x00, 0x00, 0x07, 0xD0, 0x01, 0xF4],
		ccsds_cds(2, 2, ccsds_epoch)::parse(bytes(OriginalBytes), TimeCode),
		ccsds_cds(2, 2, ccsds_epoch)::generate(bytes(Bytes), TimeCode).

	test(ccsds_cds_roundtrip_02, true(Bytes == OriginalBytes)) :-
		OriginalBytes = [0x00, 0x00, 0x01, 0x00, 0x00, 0x07, 0xD0, 0x1D, 0xCD, 0x65, 0x00],
		ccsds_cds(3, 4, ccsds_epoch)::parse(bytes(OriginalBytes), TimeCode),
		ccsds_cds(3, 4, ccsds_epoch)::generate(bytes(Bytes), TimeCode).

	test(ccsds_ccs_parse_2_01, true(TimeCode == ccs_calendar_time(2026, 5, 8, 14, 30, 45, 0))) :-
		ccsds_ccs(calendar, 0)::parse(bytes([0x20, 0x26, 0x05, 0x08, 0x14, 0x30, 0x45]), TimeCode).

	test(ccsds_ccs_parse_2_02, true(TimeCode == ccs_calendar_time(2026, 5, 8, 14, 30, 45, 67))) :-
		ccsds_ccs(calendar, 1)::parse(bytes([0x20, 0x26, 0x05, 0x08, 0x14, 0x30, 0x45, 0x67]), TimeCode).

	test(ccsds_ccs_parse_2_03, true(TimeCode == ccs_ordinal_time(2026, 128, 14, 30, 45, 0))) :-
		ccsds_ccs(day_of_year, 0)::parse(bytes([0x20, 0x26, 0x01, 0x28, 0x14, 0x30, 0x45]), TimeCode).

	test(ccsds_ccs_parse_2_04, error(domain_error(ccs_time_code_byte_sequence, _))) :-
		ccsds_ccs(calendar, 0)::parse(bytes([0x20, 0x26]), _).

	test(ccsds_ccs_generate_2_01, true(Bytes == [0x20, 0x26, 0x05, 0x08, 0x14, 0x30, 0x45])) :-
		ccsds_ccs(calendar, 0)::generate(bytes(Bytes), ccs_calendar_time(2026, 5, 8, 14, 30, 45, 0)).

	test(ccsds_ccs_generate_2_02, true(Bytes == [0x20, 0x26, 0x01, 0x28, 0x14, 0x30, 0x45, 0x67])) :-
		ccsds_ccs(day_of_year, 1)::generate(bytes(Bytes), ccs_ordinal_time(2026, 128, 14, 30, 45, 67)).

	test(ccsds_ccs_generate_3_01, true(Bytes == [0x20, 0x26, 0x05, 0x08, 0x14, 0x30, 0x45, 0xFF])) :-
		ccsds_ccs(calendar, 0)::generate(ccs_calendar_time(2026, 5, 8, 14, 30, 45, 0), Bytes, [0xFF]).

	test(ccsds_ccs_valid_1_01, deterministic) :-
		ccsds_ccs(calendar, 0)::valid(ccs_calendar_time(2026, 5, 8, 14, 30, 45, 0)).

	test(ccsds_ccs_valid_1_02, deterministic) :-
		ccsds_ccs(day_of_year, 1)::valid(ccs_ordinal_time(2024, 60, 12, 0, 0, 50)).

	test(ccsds_ccs_valid_1_03, fail) :-
		ccsds_ccs(calendar, 0)::valid(ccs_calendar_time(2026, 2, 30, 14, 30, 45, 0)).

	test(ccsds_ccs_valid_1_04, fail) :-
		ccsds_ccs(day_of_year, 0)::valid(ccs_ordinal_time(2025, 366, 0, 0, 0, 0)).

	test(ccsds_ccs_format_1_01, deterministic(Format == ccs)) :-
		ccsds_ccs(calendar, 0)::format(Format).

	test(ccsds_ccs_epoch_1_01, deterministic(Epoch == none)) :-
		ccsds_ccs(calendar, 0)::epoch(Epoch).

	test(ccsds_ccs_year_2_01, deterministic(Year == 2026)) :-
		ccsds_ccs(calendar, 0)::year(ccs_calendar_time(2026, 5, 8, 14, 30, 45, 0), Year).

	test(ccsds_ccs_month_2_01, deterministic(Month == 5)) :-
		ccsds_ccs(calendar, 0)::month(ccs_calendar_time(2026, 5, 8, 14, 30, 45, 0), Month).

	test(ccsds_ccs_day_2_01, deterministic(Day == 8)) :-
		ccsds_ccs(calendar, 0)::day(ccs_calendar_time(2026, 5, 8, 14, 30, 45, 0), Day).

	test(ccsds_ccs_day_of_year_2_01, deterministic(DayOfYear == 128)) :-
		ccsds_ccs(day_of_year, 0)::day_of_year(ccs_ordinal_time(2026, 128, 14, 30, 45, 0), DayOfYear).

	test(ccsds_ccs_time_of_day_4_01, deterministic(Hour-Minute-Seconds == 14-30-45.67)) :-
		ccsds_ccs(calendar, 1)::time_of_day(ccs_calendar_time(2026, 5, 8, 14, 30, 45, 67), Hour, Minute, Seconds).

	test(ccsds_ccs_unix_seconds_2_01, deterministic(Seconds =:= 0)) :-
		ccsds_ccs(calendar, 0)::unix_seconds(ccs_calendar_time(1970, 1, 1, 0, 0, 0, 0), Seconds).

	test(ccsds_ccs_unix_seconds_2_02, deterministic(Seconds =:= 1.5)) :-
		ccsds_ccs(day_of_year, 1)::unix_seconds(ccs_ordinal_time(1970, 1, 0, 0, 1, 50), Seconds).

	test(ccsds_ccs_from_unix_seconds_2_01, deterministic(TimeCode == ccs_calendar_time(1970, 1, 1, 0, 0, 0, 0))) :-
		ccsds_ccs(calendar, 0)::from_unix_seconds(0, TimeCode).

	test(ccsds_ccs_from_unix_seconds_2_02, deterministic(TimeCode == ccs_ordinal_time(1970, 1, 0, 0, 1, 50))) :-
		ccsds_ccs(day_of_year, 1)::from_unix_seconds(1.5, TimeCode).

	test(ccsds_ccs_roundtrip_01, true(Bytes == OriginalBytes)) :-
		OriginalBytes = [0x20, 0x26, 0x05, 0x08, 0x14, 0x30, 0x45, 0x67],
		ccsds_ccs(calendar, 1)::parse(bytes(OriginalBytes), TimeCode),
		ccsds_ccs(calendar, 1)::generate(bytes(Bytes), TimeCode).

	test(ccsds_time_codes_types_check_01, deterministic) :-
		type::check(ccsds_cuc(4, 2, ccsds_epoch), [0x00, 0x00, 0x01, 0x00, 0x00, 0x80]).

	test(ccsds_time_codes_types_check_02, deterministic) :-
		type::check(ccsds_cuc_time(4, 2), cuc_time(256, 128)).

	test(ccsds_time_codes_types_check_03, deterministic) :-
		type::check(ccsds_cds(2, 0, ccsds_epoch), [0x00, 0x01, 0x00, 0x00, 0x07, 0xD0]).

	test(ccsds_time_codes_types_check_04, deterministic) :-
		type::check(ccsds_cds_time(2, 2), cds_time(1, 2000, 500)).

	test(ccsds_time_codes_types_check_05, deterministic) :-
		type::check(ccsds_ccs(calendar, 0), [0x20, 0x26, 0x05, 0x08, 0x14, 0x30, 0x45]).

	test(ccsds_time_codes_types_check_06, deterministic) :-
		type::check(ccsds_ccs_time(day_of_year, 1), ccs_ordinal_time(2026, 128, 14, 30, 45, 67)).

:- end_object.
