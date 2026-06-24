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


:- object(ccsds_cds(_DaySegmentOctets_, _SubmillisecondOctets_, _Epoch_),
	implements(ccsds_time_code_protocol)).

	:- info([
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2026-06-24,
		comment is 'CCSDS day segmented time code parser and generator.',
		parameters is [
			'DaySegmentOctets' - 'Number of day segment octets. Supported values are ``2`` and ``3``.',
			'SubmillisecondOctets' - 'Number of submillisecond octets. Supported values are ``0``, ``2``, and ``4``.',
			'Epoch' - 'Epoch used when converting to and from Unix seconds. Supported values are ``ccsds_epoch`` and ``unix_epoch``.'
		]
	]).

	:- public(days/2).
	:- mode(days(+compound, -integer), one).
	:- info(days/2, [
		comment is 'Extracts the day segment value from a CDS time term.',
		argnames is ['TimeCode', 'Days']
	]).

	:- public(milliseconds/2).
	:- mode(milliseconds(+compound, -integer), one).
	:- info(milliseconds/2, [
		comment is 'Extracts the millisecond-of-day value from a CDS time term.',
		argnames is ['TimeCode', 'Milliseconds']
	]).

	:- public(submilliseconds/2).
	:- mode(submilliseconds(+compound, -integer), zero_or_one).
	:- info(submilliseconds/2, [
		comment is 'Extracts the optional submillisecond value from a CDS time term. Two-octet objects use microseconds; four-octet objects use picoseconds within the millisecond.',
		argnames is ['TimeCode', 'Submilliseconds']
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(reader, [
		file_to_bytes/2, stream_to_bytes/2
	]).

	parse(Source, _) :-
		var(Source),
		instantiation_error.
	parse(file(File), TimeCode) :-
		!,
		file_to_bytes(File, Bytes),
		parse_bytes(Bytes, TimeCode).
	parse(stream(Stream), TimeCode) :-
		!,
		stream_to_bytes(Stream, Bytes),
		parse_bytes(Bytes, TimeCode).
	parse(bytes(Bytes), TimeCode) :-
		!,
		parse_bytes(Bytes, TimeCode).
	parse(Source, _) :-
		domain_error(ccsds_time_code_source, Source).

	generate(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate(file(File), TimeCode) :-
		!,
		generate(TimeCode, Bytes, []),
		open(File, write, Stream, [type(binary)]),
		(	catch(
				write_bytes(Bytes, Stream),
				Error,
				(	catch(close(Stream), _, true),
					throw(Error)
				)
			) ->
			close(Stream)
		; 	catch(close(Stream), _, true),
			fail
		).
	generate(stream(Stream), TimeCode) :-
		!,
		generate(TimeCode, Bytes, []),
		write_bytes(Bytes, Stream).
	generate(bytes(Bytes), TimeCode) :-
		!,
		generate(TimeCode, Bytes, []).
	generate(Sink, _) :-
		domain_error(ccsds_time_code_sink, Sink).

	generate(TimeCode, _, _) :-
		var(TimeCode),
		instantiation_error.
	generate(TimeCode, Bytes, Tail) :-
		valid(TimeCode),
		!,
		encode_time(TimeCode, Bytes, Tail).
	generate(TimeCode, _, _) :-
		domain_error(ccsds_time_code_term, TimeCode).

	valid(cds_time(Days, Milliseconds)) :-
		valid_parameters,
		_SubmillisecondOctets_ =:= 0,
		valid_time_fields(Days, Milliseconds),
		!.
	valid(cds_time(Days, Milliseconds, Submilliseconds)) :-
		valid_parameters,
		_SubmillisecondOctets_ =:= 2,
		valid_time_fields(Days, Milliseconds),
		integer(Submilliseconds),
		Submilliseconds >= 0,
		Submilliseconds =< 999,
		!.
	valid(cds_time(Days, Milliseconds, Submilliseconds)) :-
		valid_parameters,
		_SubmillisecondOctets_ =:= 4,
		valid_time_fields(Days, Milliseconds),
		integer(Submilliseconds),
		Submilliseconds >= 0,
		Submilliseconds =< 999999999.

	format(cds).

	epoch(_Epoch_).

	unix_seconds(TimeCode, Seconds) :-
		(	var(TimeCode) ->
			instantiation_error
		;	valid(TimeCode) ->
			epoch_offset(_Epoch_, Offset),
			TimeCode =.. [_Functor, Days, Milliseconds| Rest],
			Seconds0 is Days * 86400 + Milliseconds / 1000,
			(	Rest = [Submilliseconds] ->
				submillisecond_scale(_SubmillisecondOctets_, Scale),
				Seconds is Offset + Seconds0 + Submilliseconds / Scale
			;	Seconds is Offset + Seconds0
			)
		;	domain_error(ccsds_time_code_term, TimeCode)
		).

	from_unix_seconds(Seconds, TimeCode) :-
		(	var(Seconds) ->
			instantiation_error
		;	number(Seconds) ->
			epoch_offset(_Epoch_, Offset),
			Adjusted is Seconds - Offset,
			(	Adjusted < 0 ->
				domain_error(ccsds_time_code_unix_seconds, Seconds)
			;	convert_unix_seconds(Adjusted, TimeCode),
				(	valid(TimeCode) ->
					true
				;	domain_error(ccsds_time_code_unix_seconds, Seconds)
				)
			)
		;	domain_error(ccsds_time_code_unix_seconds, Seconds)
		).

	days(cds_time(Days, _), Days).
	days(cds_time(Days, _, _), Days).

	milliseconds(cds_time(_, Milliseconds), Milliseconds).
	milliseconds(cds_time(_, Milliseconds, _), Milliseconds).

	submilliseconds(cds_time(_, _, Submilliseconds), Submilliseconds).

	parse_bytes(Bytes, TimeCode) :-
		expected_length(ExpectedLength),
		(	valid(list(byte, ExpectedLength), Bytes) ->
			decode_time(Bytes, TimeCode)
		;	domain_error(ccsds_time_code_byte_sequence, Bytes)
		).

	expected_length(ExpectedLength) :-
		ExpectedLength is _DaySegmentOctets_ + 4 + _SubmillisecondOctets_.

	decode_time(Bytes, TimeCode) :-
		decode_integer(_DaySegmentOctets_, Bytes, Rest, Days),
		decode_integer(4, Rest, Rest2, Milliseconds),
		decode_submilliseconds(Rest2, Submilliseconds),
		make_time_code(Days, Milliseconds, Submilliseconds, TimeCode).

	encode_time(TimeCode, Bytes, Tail) :-
		split_time_code(TimeCode, Days, Milliseconds, Submilliseconds),
		encode_integer(_DaySegmentOctets_, Days, Bytes, Rest),
		encode_integer(4, Milliseconds, Rest, Rest2),
		encode_submilliseconds(Submilliseconds, Rest2, Tail).

	decode_submilliseconds([], none) :-
		_SubmillisecondOctets_ =:= 0,
		!.
	decode_submilliseconds(Bytes, Submilliseconds) :-
		_SubmillisecondOctets_ > 0,
		decode_integer(_SubmillisecondOctets_, Bytes, [], Submilliseconds).

	encode_submilliseconds(none, Tail, Tail) :-
		_SubmillisecondOctets_ =:= 0,
		!.
	encode_submilliseconds(Submilliseconds, Bytes, Tail) :-
		_SubmillisecondOctets_ > 0,
		encode_integer(_SubmillisecondOctets_, Submilliseconds, Bytes, Tail).

	make_time_code(Days, Milliseconds, none, cds_time(Days, Milliseconds)) :-
		_SubmillisecondOctets_ =:= 0,
		!.
	make_time_code(Days, Milliseconds, Submilliseconds, cds_time(Days, Milliseconds, Submilliseconds)).

	split_time_code(cds_time(Days, Milliseconds), Days, Milliseconds, none) :-
		_SubmillisecondOctets_ =:= 0,
		!.
	split_time_code(cds_time(Days, Milliseconds, Submilliseconds), Days, Milliseconds, Submilliseconds).

	convert_unix_seconds(Adjusted, cds_time(Days, Milliseconds)) :-
		_SubmillisecondOctets_ =:= 0,
		!,
		TotalMilliseconds is round(Adjusted * 1000),
		DaySpan is 86400000,
		Days is TotalMilliseconds // DaySpan,
		Milliseconds is TotalMilliseconds mod DaySpan.
	convert_unix_seconds(Adjusted, cds_time(Days, Milliseconds, Submilliseconds)) :-
		_SubmillisecondOctets_ =:= 2,
		!,
		TotalMicroseconds is round(Adjusted * 1000000),
		DaySpan is 86400000000,
		Days is TotalMicroseconds // DaySpan,
		RemainingMicroseconds is TotalMicroseconds mod DaySpan,
		Milliseconds is RemainingMicroseconds // 1000,
		Submilliseconds is RemainingMicroseconds mod 1000.
	convert_unix_seconds(Adjusted, cds_time(Days, Milliseconds, Submilliseconds)) :-
		TotalPicoseconds is round(Adjusted * 1000000000000),
		DaySpan is 86400000000000000,
		Days is TotalPicoseconds // DaySpan,
		RemainingPicoseconds is TotalPicoseconds mod DaySpan,
		Milliseconds is RemainingPicoseconds // 1000000000,
		Submilliseconds is RemainingPicoseconds mod 1000000000.

	valid_time_fields(Days, Milliseconds) :-
		max_integer(_DaySegmentOctets_, MaxDays),
		integer(Days),
		Days >= 0,
		Days =< MaxDays,
		integer(Milliseconds),
		Milliseconds >= 0,
		Milliseconds =< 86399999.

	decode_integer(0, Bytes, Bytes, 0) :-
		!.
	decode_integer(Octets, [Byte| Bytes], Rest, Integer) :-
		Octets > 0,
		Octets1 is Octets - 1,
		decode_integer(Octets1, Bytes, Rest, Partial),
		Shift is 8 * Octets1,
		Integer is (Byte << Shift) + Partial.

	encode_integer(0, 0, Tail, Tail) :-
		!.
	encode_integer(Octets, Integer, [Byte| Bytes], Tail) :-
		Octets > 0,
		Shift is 8 * (Octets - 1),
		Byte is (Integer >> Shift) /\ 255,
		Rest is Integer /\ ((1 << Shift) - 1),
		Octets1 is Octets - 1,
		encode_integer(Octets1, Rest, Bytes, Tail).

	write_bytes([], _).
	write_bytes([Byte| Bytes], Stream) :-
		put_byte(Stream, Byte),
		write_bytes(Bytes, Stream).

	valid_parameters :-
		valid_day_segment_octets(_DaySegmentOctets_),
		valid_submillisecond_octets(_SubmillisecondOctets_),
		valid_epoch(_Epoch_).

	valid_day_segment_octets(2).
	valid_day_segment_octets(3).

	valid_submillisecond_octets(0).
	valid_submillisecond_octets(2).
	valid_submillisecond_octets(4).

	valid_epoch(ccsds_epoch).
	valid_epoch(unix_epoch).

	submillisecond_scale(2, 1000000).
	submillisecond_scale(4, 1000000000000).

	epoch_offset(ccsds_epoch, -378691200).
	epoch_offset(unix_epoch, 0).

	max_integer(Octets, Max) :-
		Max is (1 << (8 * Octets)) - 1.

:- end_object.
