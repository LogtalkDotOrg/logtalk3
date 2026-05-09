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


:- object(ccsds_time_fields).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-09,
		comment is 'Helpers for parsing and generating self-describing CCSDS time fields using descriptor terms and the existing CCSDS time-code objects.'
	]).

	:- public(valid_descriptor/1).
	:- mode(valid_descriptor(@compound), zero_or_one).
	:- info(valid_descriptor/1, [
		comment is 'True if the argument is a supported CCSDS time-field descriptor term.',
		argnames is ['Descriptor']
	]).

	:- public(parse/3).
	:- mode(parse(+compound, ?compound, -compound), one_or_error).
	:- info(parse/3, [
		comment is 'Parses a self-describing CCSDS time field from a source term into a descriptor term and a time-code term. Supported source terms are ``file(File)``, ``stream(Stream)``, and ``bytes(Bytes)``.',
		argnames is ['Source', 'Descriptor', 'TimeCode'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source' - domain_error(ccsds_time_field_source, 'Source'),
			'``Source`` contents are not a supported CCSDS time field' - domain_error(ccsds_time_field_byte_sequence, 'Bytes')
		]
	]).

	:- public(generate/3).
	:- mode(generate(+compound, +compound, +compound), one_or_error).
	:- info(generate/3, [
		comment is 'Generates a self-describing CCSDS time field to a sink term from a descriptor term and a time-code term. Supported sink terms are ``file(File)``, ``stream(Stream)``, and ``bytes(Bytes)``.',
		argnames is ['Sink', 'Descriptor', 'TimeCode'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Descriptor`` is a variable' - instantiation_error,
			'``TimeCode`` is a variable' - instantiation_error,
			'``Sink`` is neither a variable nor a valid sink' - domain_error(ccsds_time_field_sink, 'Sink'),
			'``Descriptor`` is neither a variable nor a supported descriptor term' - domain_error(ccsds_time_field_descriptor, 'Descriptor'),
			'``TimeCode`` is neither a variable nor a valid time code term for the selected descriptor' - domain_error(ccsds_time_code_term, 'TimeCode')
		]
	]).

	:- public(format/2).
	:- mode(format(+compound, -atom), one_or_error).
	:- info(format/2, [
		comment is 'Returns the CCSDS time-code format selected by a descriptor term.',
		argnames is ['Descriptor', 'Format']
	]).

	:- public(epoch/2).
	:- mode(epoch(+compound, -atom), one_or_error).
	:- info(epoch/2, [
		comment is 'Returns the epoch selected by a descriptor term. Calendar segmented descriptors return ``none``.',
		argnames is ['Descriptor', 'Epoch']
	]).

	:- uses(reader, [
		file_to_bytes/2, stream_to_bytes/2
	]).

	:- uses(type, [
		valid/2
	]).

	valid_descriptor(cuc_descriptor(CoarseOctets, FineOctets, Epoch)) :-
		valid(between(integer, 1, 7), CoarseOctets),
		valid(between(integer, 0, 10), FineOctets),
		valid_epoch(Epoch).
	valid_descriptor(cds_descriptor(DaySegmentOctets, SubmillisecondOctets, Epoch)) :-
		valid_day_segment_octets(DaySegmentOctets),
		valid_submillisecond_octets(SubmillisecondOctets),
		valid_epoch(Epoch).
	valid_descriptor(ccs_descriptor(CalendarVariant, FractionOctets)) :-
		valid_calendar_variant(CalendarVariant),
		valid(between(integer, 0, 6), FractionOctets).

	parse(Source, Descriptor, TimeCode) :-
		(   var(Source) ->
			instantiation_error
		;   valid_source(Source) ->
			read_source_bytes(Source, Bytes),
			parse_bytes(Bytes, Descriptor, TimeCode)
		;   domain_error(ccsds_time_field_source, Source)
		).

	generate(Sink, Descriptor, TimeCode) :-
		(   var(Sink) ->
			instantiation_error
		;   var(Descriptor) ->
			instantiation_error
		;   var(TimeCode) ->
			instantiation_error
		;   valid_sink(Sink) ->
			(   valid_descriptor(Descriptor) ->
				descriptor_object(Descriptor, Object),
				(   Object::valid(TimeCode) ->
					encode_descriptor(Descriptor, PFields),
					Object::generate(TimeCode, Bytes, []),
					prepend_bytes(PFields, Bytes, FieldBytes),
					write_sink(Sink, FieldBytes)
				;   domain_error(ccsds_time_code_term, TimeCode)
				)
			;   domain_error(ccsds_time_field_descriptor, Descriptor)
			)
		;   domain_error(ccsds_time_field_sink, Sink)
		).

	format(Descriptor, Format) :-
		(   var(Descriptor) ->
			instantiation_error
		;   Descriptor = cuc_descriptor(_, _, _) ->
			Format = cuc
		;   Descriptor = cds_descriptor(_, _, _) ->
			Format = cds
		;   Descriptor = ccs_descriptor(_, _) ->
			Format = ccs
		;   domain_error(ccsds_time_field_descriptor, Descriptor)
		).

	epoch(Descriptor, Epoch) :-
		(   var(Descriptor) ->
			instantiation_error
		;   Descriptor = cuc_descriptor(_, _, Epoch0) ->
			Epoch = Epoch0
		;   Descriptor = cds_descriptor(_, _, Epoch0) ->
			Epoch = Epoch0
		;   Descriptor = ccs_descriptor(_, _) ->
			Epoch = none
		;   domain_error(ccsds_time_field_descriptor, Descriptor)
		).

	parse_bytes(Bytes, Descriptor, TimeCode) :-
		(   valid_time_field_bytes(Bytes),
			decode_time_field(Bytes, ParsedDescriptor, PayloadBytes),
			descriptor_object(ParsedDescriptor, Object),
			catch(Object::parse(bytes(PayloadBytes), TimeCode), _, fail) ->
			Descriptor = ParsedDescriptor
		;   domain_error(ccsds_time_field_byte_sequence, Bytes)
		).

	decode_time_field([PField| Rest], Descriptor, PayloadBytes) :-
		Extension is (PField >> 7) /\ 0x01,
		Code is (PField >> 4) /\ 0x07,
		decode_descriptor(Code, Extension, PField, Rest, Descriptor, PayloadBytes).

	decode_descriptor(Code, 0, PField, PayloadBytes, Descriptor, PayloadBytes) :-
		decode_cuc_descriptor(Code, PField, 0, 0, Descriptor).
	decode_descriptor(Code, 1, PField, [PField2| PayloadBytes], Descriptor, PayloadBytes) :-
		(Code =:= 1; Code =:= 2),
		0 =:= (PField2 /\ 0x83),
		AdditionalCoarseOctets is (PField2 >> 5) /\ 0x03,
		AdditionalFineOctets is (PField2 >> 2) /\ 0x07,
		decode_cuc_descriptor(Code, PField, AdditionalCoarseOctets, AdditionalFineOctets, Descriptor).
	decode_descriptor(4, 0, PField, PayloadBytes, cds_descriptor(DaySegmentOctets, SubmillisecondOctets, Epoch), PayloadBytes) :-
		cds_epoch_bit(EpochBit, Epoch),
		EpochBit =:= ((PField >> 3) /\ 0x01),
		DaySegmentOctets is 2 + ((PField >> 2) /\ 0x01),
		SubCode is PField /\ 0x03,
		cds_submillisecond_octets(SubCode, SubmillisecondOctets).
	decode_descriptor(5, 0, PField, PayloadBytes, ccs_descriptor(CalendarVariant, FractionOctets), PayloadBytes) :-
		ccs_variant_bit(CalendarVariantBit, CalendarVariant),
		CalendarVariantBit =:= ((PField >> 3) /\ 0x01),
		FractionOctets is PField /\ 0x07,
		FractionOctets =< 6.

	decode_cuc_descriptor(1, PField, AdditionalCoarseOctets, AdditionalFineOctets, cuc_descriptor(CoarseOctets, FineOctets, ccsds_epoch)) :-
		CoarseOctets is ((PField >> 2) /\ 0x03) + 1 + AdditionalCoarseOctets,
		FineOctets is (PField /\ 0x03) + AdditionalFineOctets.
	decode_cuc_descriptor(2, PField, AdditionalCoarseOctets, AdditionalFineOctets, cuc_descriptor(CoarseOctets, FineOctets, unix_epoch)) :-
		CoarseOctets is ((PField >> 2) /\ 0x03) + 1 + AdditionalCoarseOctets,
		FineOctets is (PField /\ 0x03) + AdditionalFineOctets.

	encode_descriptor(cuc_descriptor(CoarseOctets, FineOctets, Epoch), Bytes) :-
		cuc_code(Epoch, Code),
		BaseCoarseOctets is min(CoarseOctets, 4),
		BaseFineOctets is min(FineOctets, 3),
		(   (CoarseOctets > 4; FineOctets > 3) ->
			FirstByte is 0x80 \/ (Code << 4) \/ ((BaseCoarseOctets - 1) << 2) \/ BaseFineOctets,
			AdditionalCoarseOctets is max(CoarseOctets - 4, 0),
			AdditionalFineOctets is max(FineOctets - 3, 0),
			SecondByte is (AdditionalCoarseOctets << 5) \/ (AdditionalFineOctets << 2),
			Bytes = [FirstByte, SecondByte]
		;   Byte is (Code << 4) \/ ((BaseCoarseOctets - 1) << 2) \/ BaseFineOctets,
			Bytes = [Byte]
		).
	encode_descriptor(cds_descriptor(DaySegmentOctets, SubmillisecondOctets, Epoch), [Byte]) :-
		cds_epoch_bit(EpochBit, Epoch),
		DaySegmentBit is DaySegmentOctets - 2,
		cds_submillisecond_octets(SubCode, SubmillisecondOctets),
		Byte is (4 << 4) \/ (EpochBit << 3) \/ (DaySegmentBit << 2) \/ SubCode.
	encode_descriptor(ccs_descriptor(CalendarVariant, FractionOctets), [Byte]) :-
		ccs_variant_bit(CalendarVariantBit, CalendarVariant),
		Byte is (5 << 4) \/ (CalendarVariantBit << 3) \/ FractionOctets.

	descriptor_object(cuc_descriptor(CoarseOctets, FineOctets, Epoch), ccsds_cuc(CoarseOctets, FineOctets, Epoch)).
	descriptor_object(cds_descriptor(DaySegmentOctets, SubmillisecondOctets, Epoch), ccsds_cds(DaySegmentOctets, SubmillisecondOctets, Epoch)).
	descriptor_object(ccs_descriptor(CalendarVariant, FractionOctets), ccsds_ccs(CalendarVariant, FractionOctets)).

	valid_source(file(_)).
	valid_source(stream(_)).
	valid_source(bytes(Bytes)) :-
		valid_time_field_bytes(Bytes).

	valid_sink(file(_)).
	valid_sink(stream(_)).
	valid_sink(bytes(_)).

	read_source_bytes(file(File), Bytes) :-
		file_to_bytes(File, Bytes).
	read_source_bytes(stream(Stream), Bytes) :-
		stream_to_bytes(Stream, Bytes).
	read_source_bytes(bytes(Bytes), Bytes).

	write_sink(file(File), Bytes) :-
		open(File, write, Stream, [type(binary)]),
		write_bytes(Bytes, Stream),
		close(Stream).
	write_sink(stream(Stream), Bytes) :-
		write_bytes(Bytes, Stream).
	write_sink(bytes(Bytes), Bytes).

	write_bytes([], _).
	write_bytes([Byte| Bytes], Stream) :-
		put_byte(Stream, Byte),
		write_bytes(Bytes, Stream).

	prepend_bytes([], Bytes, Bytes).
	prepend_bytes([Byte| Bytes], PayloadBytes, [Byte| FieldBytes]) :-
		prepend_bytes(Bytes, PayloadBytes, FieldBytes).

	valid_time_field_bytes([Byte| Bytes]) :-
		valid(list(byte), [Byte| Bytes]).

	valid_epoch(ccsds_epoch).
	valid_epoch(unix_epoch).

	valid_day_segment_octets(2).
	valid_day_segment_octets(3).

	valid_submillisecond_octets(0).
	valid_submillisecond_octets(2).
	valid_submillisecond_octets(4).

	valid_calendar_variant(calendar).
	valid_calendar_variant(day_of_year).

	cuc_code(ccsds_epoch, 1).
	cuc_code(unix_epoch, 2).

	cds_epoch_bit(0, ccsds_epoch).
	cds_epoch_bit(1, unix_epoch).

	cds_submillisecond_octets(0, 0).
	cds_submillisecond_octets(1, 2).
	cds_submillisecond_octets(2, 4).

	ccs_variant_bit(0, calendar).
	ccs_variant_bit(1, day_of_year).

:- end_object.
