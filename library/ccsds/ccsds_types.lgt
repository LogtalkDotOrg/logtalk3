%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2025 Paulo Moura <pmoura@logtalk.org>
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


:- category(ccsds_types).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-12-06,
		comment is 'Type definitions and arbitrary generators for CCSDS packets.'
	]).

	% Type definitions for ccsds_packet(SecondaryHeaderLength)
	% Allows type-checking a list of bytes as a valid CCSDS packet

	:- multifile(type::type/1).
	type::type(ccsds_packet(_)).
	type::type(ccsds_packet).

	:- multifile(type::check/2).
	type::check(ccsds_packet(SecondaryHeaderLength), Term) :-
		(	check_ccsds_bytes(Term, SecondaryHeaderLength) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(ccsds_packet(SecondaryHeaderLength), Term))
		).
	% ccsds_packet is an alias for ccsds_packet(0)
	type::check(ccsds_packet, Term) :-
		type::check(ccsds_packet(0), Term).

	% Check if a term is a valid list of bytes that parses as a CCSDS packet
	check_ccsds_bytes(Bytes, SecondaryHeaderLength) :-
		type::valid(list(byte), Bytes),
		ccsds(SecondaryHeaderLength)::parse(Bytes, _).

	% Arbitrary generators for ccsds_packet(SecondaryHeaderLength)
	% Generates random valid CCSDS packets as byte lists

	:- multifile(arbitrary::arbitrary/1).
	arbitrary::arbitrary(ccsds_packet(_)).
	arbitrary::arbitrary(ccsds_packet).

	:- multifile(arbitrary::arbitrary/2).
	arbitrary::arbitrary(ccsds_packet(SecondaryHeaderLength), Bytes) :-
		generate_ccsds_packet(SecondaryHeaderLength, Bytes).
	% ccsds_packet is an alias for ccsds_packet(0)
	arbitrary::arbitrary(ccsds_packet, Bytes) :-
		generate_ccsds_packet(0, Bytes).

	generate_ccsds_packet(SecondaryHeaderLength, Bytes) :-
		% Generate random valid packet fields
		Version = 0,                                                % Version is always 0
		type::arbitrary(between(integer, 0, 1), Type),              % 0=telemetry, 1=telecommand
		type::arbitrary(between(integer, 0, 2047), APID),           % 11 bits
		type::arbitrary(between(integer, 0, 3), SeqFlags),          % 2 bits
		type::arbitrary(between(integer, 0, 16383), SeqCount),      % 14 bits
		type::arbitrary(between(integer, 1, 100), UserDataLength),  % Generate 1-100 bytes of user data
		type::arbitrary(list(byte, UserDataLength), UserData),
		% Handle secondary header
		(	SecondaryHeaderLength > 0 ->
			SecHeaderFlag = 1,
			type::arbitrary(list(byte, SecondaryHeaderLength), SecHeaderBytes),
			SecHeader = secondary_header(SecHeaderBytes)
		;	SecHeaderFlag = 0,
			SecHeader = none
		),
		% Calculate data length field (total data field length - 1)
		DataLength is UserDataLength + SecondaryHeaderLength - 1,
		% Build the packet and generate bytes
		Packet = ccsds_packet(Version, Type, SecHeaderFlag, APID, SeqFlags, SeqCount, DataLength, SecHeader, UserData),
		ccsds(SecondaryHeaderLength)::generate(Packet, Bytes).

:- end_category.

