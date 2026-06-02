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


:- object(http_websocket_handshake).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'Shared helpers for WebSocket opening-handshake key generation and accept-value computation.'
	]).

	:- public(websocket_opening_key/1).
	:- mode(websocket_opening_key(--atom), one).
	:- info(websocket_opening_key/1, [
		comment is 'Generates a canonical base64-encoded 16-byte WebSocket opening key.',
		argnames is ['Key']
	]).

	:- public(websocket_accept/2).
	:- mode(websocket_accept(++term, --atom), one_or_error).
	:- info(websocket_accept/2, [
		comment is 'Computes the canonical `Sec-WebSocket-Accept` value for a valid WebSocket opening key.',
		argnames is ['Key', 'Accept']
	]).

	:- uses(crypto, [
		random_bytes/2
	]).

	:- uses(base64, [
		parse/2, generate/2
	]).

	:- uses(list, [
		append/2, append/3, length/2, nth0/3, reverse/2, take/4
	]).

	:- uses(hash_common_32, [
		add32/3, add32/5, big_endian_word32/2, integer_to_big_endian_bytes32/3, pad_md/4, rol32/3
	]).

	websocket_opening_key(Key) :-
		random_bytes(16, Bytes),
		generate(atom(Key), Bytes).

	websocket_accept(Key, Accept) :-
		(   var(Key) ->
			throw(error(instantiation_error, context(http_websocket_handshake::websocket_accept/2, _)))
		;   normalize_websocket_key(Key, NormalizedKey) ->
			once(websocket_accept_from_key(NormalizedKey, Accept))
		;   throw(error(domain_error(http_websocket_key, Key), context(http_websocket_handshake::websocket_accept/2, _)))
		).

	normalize_websocket_key(Value, Key) :-
		normalize_websocket_base64_value(Value, 16, Key).

	normalize_websocket_base64_value(Value, Size, NormalizedValue) :-
		text_to_codes(Value, Codes0),
		trim_ows_codes(Codes0, Codes),
		Codes \== [],
		catch(once(parse(codes(Codes), Bytes)), _, fail),
		length(Bytes, Size),
		generate(atom(NormalizedValue), Bytes).

	text_to_codes(Value, Codes) :-
		atom(Value),
		!,
		atom_codes(Value, Codes).
	text_to_codes(Value, Codes) :-
		chars_to_codes(Value, Codes),
		!.
	text_to_codes(Codes, Codes) :-
		codes_text(Codes).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	codes_text([]).
	codes_text([Code| Codes]) :-
		integer(Code),
		Code >= 0,
		codes_text(Codes).

	trim_ows_codes(Codes, TrimmedCodes) :-
		trim_leading_ows_codes(Codes, LeadingTrimmedCodes),
		trim_trailing_ows_codes(LeadingTrimmedCodes, TrimmedCodes).

	trim_leading_ows_codes([Code| Codes], TrimmedCodes) :-
		ows_code(Code),
		!,
		trim_leading_ows_codes(Codes, TrimmedCodes).
	trim_leading_ows_codes(Codes, Codes).

	trim_trailing_ows_codes(Codes, TrimmedCodes) :-
		reverse(Codes, ReversedCodes),
		trim_leading_ows_codes(ReversedCodes, ReversedTrimmedCodes),
		reverse(ReversedTrimmedCodes, TrimmedCodes).

	ows_code(0' ).
	ows_code(0'\t).

	websocket_accept_from_key(Key, Accept) :-
		atom_codes(Key, KeyCodes),
		atom_codes('258EAFA5-E914-47DA-95CA-C5AB0DC85B11', GuidCodes),
		append([KeyCodes, GuidCodes], Bytes),
		websocket_sha1_digest(Bytes, DigestBytes),
		generate(atom(Accept), DigestBytes).

	websocket_sha1_digest(Bytes, DigestBytes) :-
		pad_md(big, Bytes, 8, PaddedBytes),
		websocket_sha1_blocks(PaddedBytes, 0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0, H0, H1, H2, H3, H4),
		integer_to_big_endian_bytes32(H0, DigestBytes, B1),
		integer_to_big_endian_bytes32(H1, B1, B2),
		integer_to_big_endian_bytes32(H2, B2, B3),
		integer_to_big_endian_bytes32(H3, B3, B4),
		integer_to_big_endian_bytes32(H4, B4, []).

	websocket_sha1_blocks([], H0, H1, H2, H3, H4, H0, H1, H2, H3, H4).
	websocket_sha1_blocks(Bytes0, H0_0, H1_0, H2_0, H3_0, H4_0, H0, H1, H2, H3, H4) :-
		take(64, Bytes0, Block, Rest),
		websocket_sha1_block_words(Block, W0),
		extend_websocket_sha1_words(16, W0, W),
		websocket_sha1_rounds(0, W, H0_0, H1_0, H2_0, H3_0, H4_0, A, B, C, D, E),
		add32(H0_0, A, H0_1),
		add32(H1_0, B, H1_1),
		add32(H2_0, C, H2_1),
		add32(H3_0, D, H3_1),
		add32(H4_0, E, H4_1),
		websocket_sha1_blocks(Rest, H0_1, H1_1, H2_1, H3_1, H4_1, H0, H1, H2, H3, H4).

	websocket_sha1_block_words([], []).
	websocket_sha1_block_words([B0, B1, B2, B3| Bytes], [Word| Words]) :-
		big_endian_word32([B0, B1, B2, B3], Word),
		websocket_sha1_block_words(Bytes, Words).

	extend_websocket_sha1_words(80, Words, Words) :-
		!.
	extend_websocket_sha1_words(Index, Words0, Words) :-
		I3 is Index - 3,
		I8 is Index - 8,
		I14 is Index - 14,
		I16 is Index - 16,
		nth0(I3, Words0, W3),
		nth0(I8, Words0, W8),
		nth0(I14, Words0, W14),
		nth0(I16, Words0, W16),
		Temp is xor(W3, xor(W8, xor(W14, W16))),
		rol32(Temp, 1, Word),
		append(Words0, [Word], Words1),
		NextIndex is Index + 1,
		extend_websocket_sha1_words(NextIndex, Words1, Words).

	websocket_sha1_rounds(80, _Words, A, B, C, D, E, A, B, C, D, E) :-
		!.
	websocket_sha1_rounds(Index, Words, A0, B0, C0, D0, E0, A, B, C, D, E) :-
		nth0(Index, Words, WI),
		websocket_sha1_f_k(Index, B0, C0, D0, F, K),
		rol32(A0, 5, RA),
		add32(RA, F, E0, K, T0),
		add32(T0, WI, T),
		rol32(B0, 30, C1),
		NextIndex is Index + 1,
		websocket_sha1_rounds(NextIndex, Words, T, A0, C1, C0, D0, A, B, C, D, E).

	websocket_sha1_f_k(Index, B, C, D, F, K) :-
		(   Index < 20 ->
			F is ((B /\ C) \/ ((\ B) /\ D)) /\ 0xFFFFFFFF,
			K = 0x5A827999
		;   Index < 40 ->
			F is xor(B, xor(C, D)) /\ 0xFFFFFFFF,
			K = 0x6ED9EBA1
		;   Index < 60 ->
			F is ((B /\ C) \/ (B /\ D) \/ (C /\ D)) /\ 0xFFFFFFFF,
			K = 0x8F1BBCDC
		;   F is xor(B, xor(C, D)) /\ 0xFFFFFFFF,
			K = 0xCA62C1D6
		).

:- end_object.
