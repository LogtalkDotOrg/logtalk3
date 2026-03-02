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


% define a flag to allow the logtalk_tester script to pass the
% option to suppress the test file and directory path prefix
:- initialization(
	create_logtalk_flag(suppress_path_prefix, '', [type(atom), keep(true)])
).


:- object(subunit_v2_output).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-02,
		comment is 'Intercepts unit test execution messages and outputs a Subunit v2 binary stream to the current output stream.',
		remarks is [
			'Usage' - 'Simply load this object before running your tests using the goal ``logtalk_load([basic_types(loader),term_io(loader),lgtunit(subunit_v2_output)])``.'
		]
	]).

	:- uses(list, [
		append/3, length/2
	]).

	:- uses(term_io, [
		write_term_to_atom(Term, Atom, [quoted(true)]) as term_to_atom(Term, Atom),
		write_term_to_codes(Term, Codes, [quoted(true)]) as term_to_codes(Term, Codes)
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	% intercept all messages from the "lgtunit" object while running tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, _, lgtunit, _) :-
		ignore(message_hook(Message)).

	% passed test
	message_hook(passed_test(Object, Test, _, _, Flaky, _, _, _)) :-
		write_status_packet(success, Flaky, Object, Test, '').
	% failed test
	message_hook(failed_test(Object, Test, _, _, Reason, Flaky, _, _, _)) :-
		reason_text(Reason, Text),
		write_status_packet(fail, Flaky, Object, Test, Text).
	% skipped test
	message_hook(skipped_test(Object, Test, _, _, Flaky, Note)) :-
		(   Note == '' -> Text = 'Skipped test'
		;   Text = Note
		),
		write_status_packet(skip, Flaky, Object, Test, Text).
	% tests skipped
	message_hook(tests_skipped(Object, _, Note)) :-
		(   Note == '' -> Text = 'Skipped test set'
		;   Text = Note
		),
		forall(
			Object::test(Test),
			write_status_packet(skip, false, Object, Test, Text)
		).
	% broken steps
	message_hook(broken_step(condition, Object, Error)) :-
		error_text('broken test suite condition step error', Error, Text),
		write_suite_packet(fail, Object, condition, Text).
	message_hook(broken_step(setup, Object, Error)) :-
		error_text('broken test suite setup step error', Error, Text),
		write_suite_packet(fail, Object, setup, Text).
	message_hook(broken_step(cleanup, Object, Error)) :-
		error_text('broken test suite cleanup step error', Error, Text),
		write_suite_packet(fail, Object, cleanup, Text).
	message_hook(failed_step(setup, Object)) :-
		write_suite_packet(fail, Object, setup, 'broken test suite setup step failure').
	message_hook(failed_step(cleanup, Object)) :-
		write_suite_packet(fail, Object, cleanup, 'broken test suite cleanup step failure').
	% ignore remaining messages
	message_hook(_).

	write_suite_packet(Status, Object, Step, Text) :-
		current_timestamp(Seconds, Nanoseconds),
		test_id_codes(Object, Step, TestIdCodes),
		status_packet(Status, false, Seconds, Nanoseconds, TestIdCodes, Text, Packet),
		write_packet(Packet).

	write_status_packet(Status, Flaky, Object, Test, Text) :-
		current_timestamp(Seconds, Nanoseconds),
		test_id_codes(Object, Test, TestIdCodes),
		status_packet(Status, Flaky, Seconds, Nanoseconds, TestIdCodes, Text, Packet),
		write_packet(Packet).

	status_packet(Status, Flaky, Seconds, Nanoseconds, TestIdCodes, Text, Packet) :-
		status_mask(Status, StatusMask),
		base_flags(BaseFlags),
		tags_flags(Flaky, TagsFlags),
		(   Text == '' ->
			Flags is BaseFlags \/ TagsFlags \/ StatusMask,
			build_packet(Flags, Flaky, Seconds, Nanoseconds, TestIdCodes, [], Packet)
		;   Flags is BaseFlags \/ 0x0020 \/ 0x0040 \/ StatusMask,
			atom_codes('text/plain;charset=utf8', MimeTypeCodes),
			atom_codes('reason', FileNameCodes),
			atom_codes(Text, TextCodes),
			Flags2 is Flags \/ TagsFlags,
			build_packet(Flags2, Flaky, Seconds, Nanoseconds, TestIdCodes, [MimeTypeCodes, FileNameCodes, TextCodes], Packet)
		).

	base_flags(0x2000 \/ 0x0800 \/ 0x0200 \/ 0x0100).

	status_mask(success, 0x0003).
	status_mask(skip,    0x0005).
	status_mask(fail,    0x0006).

	build_packet(Flags, Flaky, Seconds, Nanoseconds, TestIdCodes, Extras, Packet) :-
		integer_to_bytes32(Seconds, EncodedSeconds),
		encode_number(Nanoseconds, EncodedNanoseconds),
		append(EncodedSeconds, EncodedNanoseconds, EncodedTimestamp),
		encode_utf8_with_length(TestIdCodes, EncodedTestId),
		encode_tags(Flaky, EncodedTags),
		build_body(Extras, EncodedTimestamp, EncodedTestId, EncodedTags, Body),
		length(Body, BodyLength),
		packet_base_length(BodyLength, BaseLength),
		length_field_length(BaseLength, LengthFieldLength),
		TotalLength is BaseLength + LengthFieldLength,
		encode_number(TotalLength, EncodedLength),
		integer_to_bytes16(Flags, FlagsBytes),
		Prefix0 = [0xB3| FlagsBytes],
		append(Prefix0, EncodedLength, Prefix1),
		append(Prefix1, Body, Content),
		crc32(Content, CRC32),
		integer_to_bytes32(CRC32, CRCBytes),
		append(Content, CRCBytes, Packet).

	build_body([], EncodedTimestamp, EncodedTestId, EncodedTags, Body) :-
		append(EncodedTimestamp, EncodedTestId, Body0),
		append(Body0, EncodedTags, Body).
	build_body([MimeTypeCodes, FileNameCodes, TextCodes], EncodedTimestamp, EncodedTestId, EncodedTags, Body) :-
		encode_utf8_with_length(MimeTypeCodes, EncodedMimeType),
		encode_utf8_with_length(FileNameCodes, EncodedFileName),
		length(TextCodes, TextLength),
		encode_number(TextLength, EncodedTextLength),
		append(EncodedTimestamp, EncodedTestId, Body0),
		append(Body0, EncodedTags, Body1),
		append(Body1, EncodedMimeType, Body2),
		append(Body2, EncodedFileName, Body3),
		append(Body3, EncodedTextLength, Body4),
		append(Body4, TextCodes, Body).

	tags_flags(true,  0x0080).
	tags_flags(false, 0x0000).

	encode_tags(true, EncodedTags) :-
		encode_number(1, EncodedCount),
		atom_codes(flaky, FlakyTagCodes),
		encode_utf8_with_length(FlakyTagCodes, EncodedFlakyTag),
		append(EncodedCount, EncodedFlakyTag, EncodedTags).
	encode_tags(false, []).

	current_timestamp(Seconds, Nanoseconds) :-
		os::date_time(Year, Month, Day, Hours, Minutes, CurrentSeconds, Milliseconds),
		date_time_to_epoch_seconds(Year, Month, Day, Hours, Minutes, CurrentSeconds, Seconds),
		Nanoseconds is Milliseconds * 1000000.

	date_time_to_epoch_seconds(Year, Month, Day, Hours, Minutes, Seconds, EpochSeconds) :-
		julian_day(1970, 1, 1, EpochJulianDay),
		julian_day(Year, Month, Day, JulianDay),
		EpochSeconds is
			(JulianDay - EpochJulianDay) * 86400 +
			Hours * 3600 +
			Minutes * 60 +
			Seconds.

	julian_day(Year, Month, Day, JulianDay) :-
		A is (14 - Month) // 12,
		Y is Year + 4800 - A,
		M is Month + (12 * A) - 3,
		D is Day + ((153 * M + 2) // 5) + (365 * Y) + (Y // 4),
		JulianDay is D - (Y // 100) + (Y // 400) - 32045.

	packet_base_length(BodyLength, BaseLength) :-
		BaseLength is 1 + 2 + BodyLength + 4.

	length_field_length(BaseLength, 1) :-
		BaseLength =< 62,
		!.
	length_field_length(BaseLength, 2) :-
		BaseLength =< 16381,
		!.
	length_field_length(BaseLength, 3) :-
		BaseLength =< 4194300,
		!.
	length_field_length(_, 4).

	encode_utf8_with_length(Codes, Encoded) :-
		length(Codes, Length),
		encode_number(Length, EncodedLength),
		append(EncodedLength, Codes, Encoded).

	encode_number(Value, [Value]) :-
		Value >= 0,
		Value < 64,
		!.
	encode_number(Value, [Byte1, Byte2]) :-
		Value < 16384,
		!,
		Byte1 is ((Value >> 8) /\ 0x3F) \/ 0x40,
		Byte2 is Value /\ 0xFF.
	encode_number(Value, [Byte1, Byte2, Byte3]) :-
		Value < 4194304,
		!,
		Byte1 is ((Value >> 16) /\ 0x3F) \/ 0x80,
		Byte2 is (Value >> 8) /\ 0xFF,
		Byte3 is Value /\ 0xFF.
	encode_number(Value, [Byte1, Byte2, Byte3, Byte4]) :-
		Byte1 is ((Value >> 24) /\ 0x3F) \/ 0xC0,
		Byte2 is (Value >> 16) /\ 0xFF,
		Byte3 is (Value >> 8) /\ 0xFF,
		Byte4 is Value /\ 0xFF.

	integer_to_bytes16(Integer, [Byte1, Byte2]) :-
		Byte1 is (Integer >> 8) /\ 0xFF,
		Byte2 is Integer /\ 0xFF.

	integer_to_bytes32(Integer, [Byte1, Byte2, Byte3, Byte4]) :-
		Byte1 is (Integer >> 24) /\ 0xFF,
		Byte2 is (Integer >> 16) /\ 0xFF,
		Byte3 is (Integer >> 8) /\ 0xFF,
		Byte4 is Integer /\ 0xFF.

	write_packet(Packet) :-
		current_output(Stream),
		write_bytes(Packet, Stream),
		flush_output(Stream).

	write_bytes([], _).
	write_bytes([Byte| Bytes], Stream) :-
		put_byte(Stream, Byte),
		write_bytes(Bytes, Stream).

	test_id_codes(Object, Test, Codes) :-
		numbervars((Object, Test), 0, _),
		term_to_codes(Object, ObjectCodes),
		term_to_codes(Test, TestCodes),
		append(TestCodes, [32, 0'@, 32| ObjectCodes], Codes).

	error_text(Prefix, Error, Text) :-
		term_to_atom(Error, ErrorAtom),
		atomic_list_concat([Prefix, ': ', ErrorAtom], Text).

	reason_text(success_instead_of_failure, 'test goal succeeded but should have failed').
	reason_text(success_instead_of_error(ExpectedError), Text) :-
		term_to_atom(ExpectedError, Atom),
		atomic_list_concat(['test goal succeeded but should have throw an error: ', Atom], Text).
	reason_text(failure_instead_of_success, 'test goal failed but should have succeeded').
	reason_text(failure_instead_of_error(ExpectedError), Text) :-
		term_to_atom(ExpectedError, Atom),
		atomic_list_concat(['test goal failed but should have throw an error: ', Atom], Text).
	reason_text(error_instead_of_failure(Error), Text) :-
		term_to_atom(Error, Atom),
		atomic_list_concat(['test goal throws an error but should have failed: ', Atom], Text).
	reason_text(error_instead_of_success(Error), Text) :-
		term_to_atom(Error, Atom),
		atomic_list_concat(['test goal throws an error but should have succeeded: ', Atom], Text).
	reason_text(wrong_error(ExpectedError, Error), Text) :-
		term_to_atom(ExpectedError, ExpectedAtom),
		term_to_atom(Error, ErrorAtom),
		atomic_list_concat(['test goal throws the wrong error; got: ', ErrorAtom, '; expected: ', ExpectedAtom], Text).

	reason_text(quick_check_broken(Why, Error), Text) :-
		term_to_atom(Why, WhyAtom),
		term_to_atom(Error, ErrorAtom),
		atomic_list_concat(['broken QuickCheck test; why: ', WhyAtom, '; error: ', ErrorAtom], Text).
	reason_text(quick_check_broken(Why), Text) :-
		term_to_atom(Why, WhyAtom),
		atomic_list_concat(['broken QuickCheck test; why: ', WhyAtom], Text).

	reason_text(step_error(Step, Error), Text) :-
		term_to_atom(Step, StepAtom),
		term_to_atom(Error, ErrorAtom),
		atomic_list_concat([StepAtom, ' goal throws an error but should have succeeded: ', ErrorAtom], Text).
	reason_text(step_failure(Step), Text) :-
		term_to_atom(Step, StepAtom),
		atomic_list_concat([StepAtom, ' goal failed but should have succeeded'], Text).
	reason_text(Reason, Text) :-
		term_to_atom(Reason, Text).

	crc32(Bytes, CRC32) :-
		crc32_acc(Bytes, 0xFFFFFFFF, Acc),
		CRC32 is xor(Acc, 0xFFFFFFFF) /\ 0xFFFFFFFF.

	crc32_acc([], Acc, Acc).
	crc32_acc([Byte| Bytes], Acc0, Acc) :-
		Index is xor(Acc0, Byte) /\ 0xFF,
		crc32_table_value(Index, TableValue),
		Acc1 is xor((Acc0 >> 8), TableValue) /\ 0xFFFFFFFF,
		crc32_acc(Bytes, Acc1, Acc).

	crc32_table_value(Index, Value) :-
		crc32_table_value(8, Index, Value).

	crc32_table_value(0, Value, Value) :-
		!.
	crc32_table_value(Count, Value0, Value) :-
		(   Value0 /\ 1 =:= 1 ->
			Value1 is xor((Value0 >> 1), 0xEDB88320)
		;   Value1 is Value0 >> 1
		),
		NextCount is Count - 1,
		crc32_table_value(NextCount, Value1, Value).

:- end_object.
