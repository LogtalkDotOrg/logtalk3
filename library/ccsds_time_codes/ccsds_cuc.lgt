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


:- object(ccsds_cuc(_CoarseOctets_, _FineOctets_, _Epoch_),
	implements(ccsds_time_code_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-08,
		comment is 'CCSDS unsegmented time code parser and generator.',
		parameters is [
			'CoarseOctets' - 'Number of coarse time octets.',
			'FineOctets' - 'Number of fine time octets.',
			'Epoch' - 'Epoch used when converting to and from Unix seconds. Supported values are ``ccsds_epoch`` and ``unix_epoch``.'
		]
	]).

	:- public(coarse/2).
	:- mode(coarse(+compound, -integer), one).
	:- info(coarse/2, [
		comment is 'Extracts the coarse field from a CUC time term.',
		argnames is ['TimeCode', 'Coarse']
	]).

	:- public(fine/2).
	:- mode(fine(+compound, -integer), one).
	:- info(fine/2, [
		comment is 'Extracts the fine field from a CUC time term.',
		argnames is ['TimeCode', 'Fine']
	]).

	:- uses(list, [
		length/2
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

	valid(cuc_time(Coarse, Fine)) :-
		valid_octet_count(_CoarseOctets_),
		valid_octet_count(_FineOctets_),
		valid_epoch(_Epoch_),
		max_integer(_CoarseOctets_, MaxCoarse),
		max_integer(_FineOctets_, MaxFine),
		integer(Coarse), Coarse >= 0, Coarse =< MaxCoarse,
		integer(Fine), Fine >= 0, Fine =< MaxFine.

	format(cuc).

	epoch(_Epoch_).

	unix_seconds(TimeCode, Seconds) :-
		(	var(TimeCode) ->
			instantiation_error
		;	valid(TimeCode) ->
			TimeCode = cuc_time(Coarse, Fine),
			fine_scale(_FineOctets_, Scale),
			epoch_offset(_Epoch_, Offset),
			Seconds is Offset + Coarse + Fine / Scale
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
			;	true
			),
			fine_scale(_FineOctets_, Scale),
			Coarse0 is floor(Adjusted),
			Fraction is Adjusted - Coarse0,
			Fine0 is round(Fraction * Scale),
			(	Fine0 =:= Scale ->
				Coarse is Coarse0 + 1,
				Fine = 0
			;	Coarse = Coarse0,
				Fine = Fine0
			),
			TimeCode = cuc_time(Coarse, Fine),
			(	valid(TimeCode) ->
				true
			;	domain_error(ccsds_time_code_unix_seconds, Seconds)
			)
		;	domain_error(ccsds_time_code_unix_seconds, Seconds)
		).

	coarse(cuc_time(Coarse, _), Coarse).

	fine(cuc_time(_, Fine), Fine).

	parse_bytes(Bytes, TimeCode) :-
		(	valid_bytes(Bytes),
			expected_length(ExpectedLength),
			length(Bytes, ExpectedLength) ->
			decode_time(Bytes, TimeCode)
		;	domain_error(ccsds_time_code_byte_sequence, Bytes)
		).

	expected_length(ExpectedLength) :-
		ExpectedLength is _CoarseOctets_ + _FineOctets_.

	decode_time(Bytes, cuc_time(Coarse, Fine)) :-
		decode_integer(_CoarseOctets_, Bytes, Rest, Coarse),
		decode_integer(_FineOctets_, Rest, [], Fine).

	encode_time(cuc_time(Coarse, Fine), Bytes, Tail) :-
		encode_integer(_CoarseOctets_, Coarse, Bytes, Rest),
		encode_integer(_FineOctets_, Fine, Rest, Tail).

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

	valid_bytes([]).
	valid_bytes([Byte| Bytes]) :-
		integer(Byte), Byte >= 0, Byte =< 255,
		valid_bytes(Bytes).

	valid_octet_count(Octets) :-
		integer(Octets),
		Octets >= 0.

	valid_epoch(ccsds_epoch).
	valid_epoch(unix_epoch).

	epoch_offset(ccsds_epoch, -378691200).
	epoch_offset(unix_epoch, 0).

	fine_scale(FineOctets, Scale) :-
		Scale is 1 << (8 * FineOctets).

	max_integer(0, 0) :-
		!.
	max_integer(Octets, Max) :-
		Max is (1 << (8 * Octets)) - 1.

:- end_object.
