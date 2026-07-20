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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-20,
		comment is 'Unit tests for the "ccsds_time_fields" library.'
	]).

	:- uses(ccsds_time_fields, [
		valid_descriptor/1, parse/3, generate/3, format/2, epoch/2
	]).

	cover(ccsds_time_fields).

	test(ccsds_time_fields_valid_descriptor_1_01, deterministic) :-
		valid_descriptor(cuc_descriptor(4, 2, ccsds_epoch)).

	test(ccsds_time_fields_valid_descriptor_1_02, deterministic) :-
		valid_descriptor(cds_descriptor(3, 2, unix_epoch)).

	test(ccsds_time_fields_valid_descriptor_1_03, deterministic) :-
		valid_descriptor(ccs_descriptor(day_of_year, 1)).

	test(ccsds_time_fields_valid_descriptor_1_04, deterministic) :-
		valid_descriptor(cuc_descriptor(5, 0, ccsds_epoch)).

	test(ccsds_time_fields_valid_descriptor_1_05, deterministic) :-
		valid_descriptor(cds_descriptor(3, 4, unix_epoch)).

	test(ccsds_time_fields_valid_descriptor_1_06, fail) :-
		valid_descriptor(cuc_descriptor(8, 0, ccsds_epoch)).

	test(ccsds_time_fields_format_2_01, deterministic(Format == cuc)) :-
		format(cuc_descriptor(4, 2, ccsds_epoch), Format).

	test(ccsds_time_fields_epoch_2_01, deterministic(Epoch == unix_epoch)) :-
		epoch(cds_descriptor(3, 2, unix_epoch), Epoch).

	test(ccsds_time_fields_epoch_2_02, deterministic(Epoch == none)) :-
		epoch(ccs_descriptor(calendar, 1), Epoch).

	test(ccsds_time_fields_parse_3_01, deterministic(Descriptor-TimeCode == cuc_descriptor(4, 2, ccsds_epoch)-cuc_time(256, 128))) :-
		parse(bytes([0x1E, 0x00, 0x00, 0x01, 0x00, 0x00, 0x80]), Descriptor, TimeCode).

	test(ccsds_time_fields_generate_3_01, deterministic(Bytes == [0x1E, 0x00, 0x00, 0x01, 0x00, 0x00, 0x80])) :-
		generate(bytes(Bytes), cuc_descriptor(4, 2, ccsds_epoch), cuc_time(256, 128)).

	test(ccsds_time_fields_parse_3_01b, deterministic(Descriptor-TimeCode == cuc_descriptor(5, 5, ccsds_epoch)-cuc_time(256, 128))) :-
		parse(bytes([0x9F, 0x28, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80]), Descriptor, TimeCode).

	test(ccsds_time_fields_generate_3_01b, deterministic(Bytes == [0x9F, 0x28, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80])) :-
		generate(bytes(Bytes), cuc_descriptor(5, 5, ccsds_epoch), cuc_time(256, 128)).

	test(ccsds_time_fields_parse_3_02, deterministic(Descriptor-TimeCode == cds_descriptor(2, 0, ccsds_epoch)-cds_time(1, 2000))) :-
		parse(bytes([0x40, 0x00, 0x01, 0x00, 0x00, 0x07, 0xD0]), Descriptor, TimeCode).

	test(ccsds_time_fields_generate_3_02, deterministic(Bytes == [0x4D, 0x00, 0x00, 0x01, 0x00, 0x00, 0x07, 0xD0, 0x01, 0xF4])) :-
		generate(bytes(Bytes), cds_descriptor(3, 2, unix_epoch), cds_time(1, 2000, 500)).

	test(ccsds_time_fields_parse_3_02b, deterministic(Descriptor-TimeCode == cds_descriptor(3, 4, unix_epoch)-cds_time(1, 2000, 500000000))) :-
		parse(bytes([0x4E, 0x00, 0x00, 0x01, 0x00, 0x00, 0x07, 0xD0, 0x1D, 0xCD, 0x65, 0x00]), Descriptor, TimeCode).

	test(ccsds_time_fields_generate_3_02b, deterministic(Bytes == [0x4E, 0x00, 0x00, 0x01, 0x00, 0x00, 0x07, 0xD0, 0x1D, 0xCD, 0x65, 0x00])) :-
		generate(bytes(Bytes), cds_descriptor(3, 4, unix_epoch), cds_time(1, 2000, 500000000)).

	test(ccsds_time_fields_parse_3_03, deterministic(Descriptor-TimeCode == ccs_descriptor(calendar, 1)-ccs_calendar_time(2026, 5, 8, 14, 30, 45, 67))) :-
		parse(bytes([0x51, 0x20, 0x26, 0x05, 0x08, 0x14, 0x30, 0x45, 0x67]), Descriptor, TimeCode).

	test(ccsds_time_fields_generate_3_03, deterministic(Bytes == [0x59, 0x20, 0x26, 0x01, 0x28, 0x14, 0x30, 0x45, 0x67])) :-
		generate(bytes(Bytes), ccs_descriptor(day_of_year, 1), ccs_ordinal_time(2026, 128, 14, 30, 45, 67)).

	test(ccsds_time_fields_roundtrip_01, deterministic(Bytes == OriginalBytes)) :-
		OriginalBytes = [0x59, 0x20, 0x26, 0x01, 0x28, 0x14, 0x30, 0x45, 0x67],
		parse(bytes(OriginalBytes), Descriptor, TimeCode),
		generate(bytes(Bytes), Descriptor, TimeCode).

	test(ccsds_time_fields_parse_3_04, error(domain_error(ccsds_time_field_byte_sequence, _))) :-
		parse(bytes([0x42, 0x00, 0x01, 0x00, 0x00, 0x07, 0xD0]), _, _).

	test(ccsds_time_fields_parse_3_05, error(domain_error(ccsds_time_field_byte_sequence, _))) :-
		parse(bytes([0x9F, 0x2A, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x80]), _, _).

	test(ccsds_time_fields_generate_3_06, error(domain_error(ccsds_time_field_descriptor, _))) :-
		generate(bytes(_), ccs_descriptor(calendar, 7), ccs_calendar_time(2026, 5, 8, 14, 30, 45, 67)).

:- end_object.
