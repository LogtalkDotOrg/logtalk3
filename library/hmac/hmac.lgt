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


:- object(hmac,
	implements(hmac_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-16,
		comment is 'HMAC (Keyed-Hash Message Authentication Code) implementation as specified in RFC 2104.',
		see_also is [md5, sha1, sha256, sha512, sha512_256, sha3_224, sha3_256, sha3_384, sha3_512]
	]).

	:- uses(hash_common_32, [
		bytes_hex/2
	]).

	:- uses(list, [
		append/3, length/2, take/3
	]).

	digest(Hash, KeyBytes, MessageBytes, DigestBytes) :-
		check_hash(Hash),
		Hash::digest_size(DigestSize),
		digest_checked(Hash, KeyBytes, MessageBytes, DigestSize, DigestBytes).

	hex_digest(Hash, KeyBytes, MessageBytes, HexDigest) :-
		digest(Hash, KeyBytes, MessageBytes, DigestBytes),
		bytes_hex(DigestBytes, HexDigest).

	digest(Hash, KeyBytes, MessageBytes, Length, DigestBytes) :-
		check_hash(Hash),
		Hash::digest_size(DigestSize),
		check_length(Length, DigestSize),
		digest_checked(Hash, KeyBytes, MessageBytes, Length, DigestBytes).

	hex_digest(Hash, KeyBytes, MessageBytes, Length, HexDigest) :-
		digest(Hash, KeyBytes, MessageBytes, Length, DigestBytes),
		bytes_hex(DigestBytes, HexDigest).

	check_hash(Hash) :-
		(	var(Hash) ->
			instantiation_error
		;	conforms_to_protocol(Hash, hash_digest_protocol) ->
			true
		;	domain_error(hmac_hash, Hash)
		).

	check_length(Length, DigestSize) :-
		(	var(Length) ->
			instantiation_error
		;	integer(Length) ->
			(	0 =< Length,
				Length =< DigestSize ->
				true
			;	domain_error(hmac_output_length(0, DigestSize), Length)
			)
		;	type_error(integer, Length)
		).

	digest_checked(Hash, KeyBytes, MessageBytes, Length, DigestBytes) :-
		normalized_key(Hash, KeyBytes, NormalizedKey),
		xor_with_constant(NormalizedKey, 0x36, InnerKey),
		xor_with_constant(NormalizedKey, 0x5c, OuterKey),
		append(InnerKey, MessageBytes, InnerInput),
		Hash::digest(InnerInput, InnerDigest),
		append(OuterKey, InnerDigest, OuterInput),
		Hash::digest(OuterInput, FullDigest),
		take(Length, FullDigest, DigestBytes).

	normalized_key(Hash, KeyBytes, NormalizedKey) :-
		Hash::block_size(BlockSize),
		length(KeyBytes, KeyLength),
		(	KeyLength > BlockSize ->
			Hash::digest(KeyBytes, ShortKey)
		;	ShortKey = KeyBytes
		),
		length(ShortKey, ShortKeyLength),
		ZeroCount is BlockSize - ShortKeyLength,
		zero_bytes(ZeroCount, Zeros),
		append(ShortKey, Zeros, NormalizedKey).

	xor_with_constant([], _, []).
	xor_with_constant([Byte| Bytes], Constant, [XorByte| XorBytes]) :-
		XorByte is xor(Byte, Constant),
		xor_with_constant(Bytes, Constant, XorBytes).

	zero_bytes(0, []) :-
		!.
	zero_bytes(Count, [0| Bytes]) :-
		NextCount is Count - 1,
		zero_bytes(NextCount, Bytes).

:- end_object.
