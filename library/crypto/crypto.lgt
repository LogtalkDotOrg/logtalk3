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


:- object(crypto).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-20,
		comment is 'Transport-neutral cryptographic helper predicates.'
	]).

	:- public(random_bytes/2).
	:- mode(random_bytes(+non_negative_integer, -list(byte)), one_or_error).
	:- info(random_bytes/2, [
		comment is 'Returns a list with the requested number of random bytes. It tries ``/dev/urandom`` first and falls back to a pseudo-random generator when necessary.',
		argnames is ['Count', 'Bytes'],
		exceptions is [
			'``Count`` is a variable' - instantiation_error,
			'``Count`` is neither a variable nor an integer' - type_error(integer, 'Count'),
			'``Count`` is an integer but is less than zero' - domain_error(non_negative_integer, 'Count')
		]
	]).

	:- public(token_hex/2).
	:- mode(token_hex(+non_negative_integer, -atom), one_or_error).
	:- info(token_hex/2, [
		comment is 'Returns a lowercase hexadecimal token generated from the requested number of random bytes.',
		argnames is ['Count', 'Token'],
		exceptions is [
			'``Count`` is a variable' - instantiation_error,
			'``Count`` is neither a variable nor an integer' - type_error(integer, 'Count'),
			'``Count`` is an integer but is less than zero' - domain_error(non_negative_integer, 'Count')
		]
	]).

	:- public(token_urlsafe/2).
	:- mode(token_urlsafe(+non_negative_integer, -atom), one_or_error).
	:- info(token_urlsafe/2, [
		comment is 'Returns an unpadded Base64URL token generated from the requested number of random bytes.',
		argnames is ['Count', 'Token'],
		exceptions is [
			'``Count`` is a variable' - instantiation_error,
			'``Count`` is neither a variable nor an integer' - type_error(integer, 'Count'),
			'``Count`` is an integer but is less than zero' - domain_error(non_negative_integer, 'Count')
		]
	]).

	:- public(random_below/2).
	:- mode(random_below(+positive_integer, -non_negative_integer), one_or_error).
	:- info(random_below/2, [
		comment is 'Returns a uniformly distributed random integer greater than or equal to zero and less than the given exclusive upper bound.',
		argnames is ['UpperBound', 'Integer'],
		exceptions is [
			'``UpperBound`` is a variable' - instantiation_error,
			'``UpperBound`` is neither a variable nor an integer' - type_error(integer, 'UpperBound'),
			'``UpperBound`` is an integer but is not positive' - domain_error(positive_integer, 'UpperBound')
		]
	]).

	:- public(hex_bytes/2).
	:- mode(hex_bytes(+atom, -list(byte)), zero_or_one_or_error).
	:- mode(hex_bytes(-atom, +list(byte)), zero_or_one_or_error).
	:- info(hex_bytes/2, [
		comment is 'Relates a hexadecimal atom with the corresponding list of bytes, generating lowercase hexadecimal atoms when converting from bytes.',
		argnames is ['Hex', 'Bytes'],
		exceptions is [
			'``Hex`` and ``Bytes`` are both variables' - instantiation_error,
			'``Hex`` is neither a variable nor an atom' - type_error(atom, 'Hex'),
			'``Hex`` is an atom but not a valid hexadecimal atom' - domain_error(hexadecimal_atom, 'Hex'),
			'``Bytes`` is neither a variable nor a list of bytes' - type_error(list(byte), 'Bytes'),
			'``Bytes`` contains a variable byte' - instantiation_error,
			'``Bytes`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Bytes`` contains an integer outside the byte range' - domain_error(byte, 'Byte')
		]
	]).

	:- public(secure_compare/2).
	:- mode(secure_compare(+list(byte), +list(byte)), zero_or_one_or_error).
	:- mode(secure_compare(+atom, +atom), zero_or_one_or_error).
	:- info(secure_compare/2, [
		comment is 'Succeeds when both inputs are equal using a constant-time comparison strategy for byte sequences of the same representation.',
		argnames is ['Expected', 'Candidate'],
		exceptions is [
			'``Expected`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Expected`` is neither an atom nor a list of bytes' - type_error(list(byte), 'Expected'),
			'``Expected`` is an atom and ``Candidate`` is not an atom' - type_error(atom, 'Candidate'),
			'``Candidate`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Candidate`` is neither an atom nor a list of bytes when ``Expected`` is a byte list' - type_error(list(byte), 'Candidate'),
			'``Expected`` or ``Candidate`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Expected`` or ``Candidate`` contains an integer outside the byte range' - domain_error(byte, 'Byte')
		]
	]).

	:- public(hkdf/5).
	:- mode(hkdf(+object_identifier, +list(byte), +non_negative_integer, -list(byte), +list(compound)), one_or_error).
	:- info(hkdf/5, [
		comment is 'Derives a byte sequence of the requested length from input keying material using HKDF with a hash object implementing the ``hash_digest_protocol`` protocol.',
		argnames is ['Hash', 'KeyMaterial', 'Length', 'Bytes', 'Options'],
		remarks is [
			'Repeated options' - 'When the same HKDF option is given multiple times, the last occurrence is used.',
			'Option ``salt(Bytes)``' - 'Uses the given byte list as the HKDF salt. When this option is absent, the salt defaults to a zero-filled byte list with the selected hash digest size.',
			'Option ``info(Bytes)``' - 'Uses the given byte list as the HKDF context information. When this option is absent, the context information defaults to the empty list.'
		],
		exceptions is [
			'``Hash`` is a variable' - instantiation_error,
			'``Hash`` is not an object implementing the ``hash_digest_protocol`` protocol' - domain_error(crypto_hash, 'Hash'),
			'``KeyMaterial`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``KeyMaterial`` is a list but not a list of bytes' - type_error(list(byte), 'KeyMaterial'),
			'``KeyMaterial`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``KeyMaterial`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Length`` is a variable' - instantiation_error,
			'``Length`` is neither a variable nor an integer' - type_error(integer, 'Length'),
			'``Length`` is less than zero' - domain_error(non_negative_integer, 'Length'),
			'``Length`` exceeds the maximum HKDF output length for the selected hash' - domain_error(hkdf_output_length(0, 'MaxLength'), 'Length'),
			'``Options`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list of compound terms' - type_error(list(compound), 'Options'),
			'``Options`` contains an invalid option term' - domain_error(hkdf_option, 'Option'),
			'``Options`` contains a ``salt/1`` or ``info/1`` value that is not a list of bytes' - type_error(list(byte), 'Bytes'),
			'``Options`` contains a ``salt/1`` or ``info/1`` value with a variable byte' - instantiation_error,
			'``Options`` contains a ``salt/1`` or ``info/1`` value with a non-integer byte' - type_error(integer, 'Byte'),
			'``Options`` contains a ``salt/1`` or ``info/1`` value with an integer outside the byte range' - domain_error(byte, 'Byte')
		]
	]).

	:- public(pbkdf2/6).
	:- mode(pbkdf2(+object_identifier, +list(byte), +list(byte), +integer, +non_negative_integer, -list(byte)), one_or_error).
	:- info(pbkdf2/6, [
		comment is 'Derives a key from a password byte sequence and a salt using PBKDF2 with a hash object implementing the ``hash_digest_protocol`` protocol.',
		argnames is ['Hash', 'Password', 'Salt', 'Iterations', 'Length', 'DerivedKey'],
		exceptions is [
			'``Hash`` is a variable' - instantiation_error,
			'``Hash`` is not an object implementing the ``hash_digest_protocol`` protocol' - domain_error(crypto_hash, 'Hash'),
			'``Password`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Password`` is a list but not a list of bytes' - type_error(list(byte), 'Password'),
			'``Password`` contains a non-integer element' - type_error(integer, 'Byte'),
			'``Password`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Salt`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Salt`` is a liust but not a list of bytes' - type_error(list(byte), 'Salt'),
			'``Salt`` contains a non-integer element' - type_error(integer, 'Byte'),
			'``Salt`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Iterations`` is a variable' - instantiation_error,
			'``Iterations`` is neither a variable nor an integer' - type_error(integer, 'Iterations'),
			'``Iterations`` is an integer but not a positive integer' - domain_error(positive_integer, 'Iterations'),
			'``Length`` is a variable' - instantiation_error,
			'``Length`` is neither a variable nor an integer' - type_error(integer, 'Length'),
			'``Length`` is an integer but is less than zero' - domain_error(non_negative_integer, 'Length'),
			'``Length`` exceeds the maximum PBKDF2 output length' - domain_error(pbkdf2_output_length, 'Length')
		]
	]).

	:- public(apr1/3).
	:- mode(apr1(+list(byte), +list(byte), -list(byte)), one_or_error).
	:- info(apr1/3, [
		comment is 'Computes the Apache APR1 encoded checksum for a password byte sequence and salt byte sequence.',
		argnames is ['Password', 'Salt', 'Checksum'],
		exceptions is [
			'``Password`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Password`` is a list but not a list of bytes' - type_error(list(byte), 'Password'),
			'``Password`` contains a non-integer element' - type_error(integer, 'Byte'),
			'``Password`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Salt`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Salt`` is a list but not a list of bytes' - type_error(list(byte), 'Salt'),
			'``Salt`` contains a non-integer element' - type_error(integer, 'Byte'),
			'``Salt`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Salt`` is not a valid APR1 salt' - domain_error(apr1_salt, 'Salt')
		]
	]).

	:- public(password_hash/4).
	:- mode(password_hash(+object_identifier, +list(byte), -compound, +list(compound)), one_or_error).
	:- info(password_hash/4, [
		comment is 'Computes a structured password-hash term using PBKDF2, a hash object implementing the ``hash_digest_protocol`` protocol, and the given derivation options.',
		argnames is ['Hash', 'Password', 'PasswordHash', 'Options'],
		remarks is [
			'Repeated options' - 'When the same password-hash option is given multiple times, the last occurrence is used.',
			'Option ``iterations(Count)``' - 'Uses the given positive integer PBKDF2 iteration count. When this option is absent, the iteration count defaults to ``131072``.',
			'Option ``salt(Bytes)``' - 'Uses the given byte list as the PBKDF2 salt. When this option is present, no random salt is generated and any ``salt_length/1`` option only affects validation, not the selected salt value.',
			'Option ``salt_length(Count)``' - 'Generates a random salt with the given non-negative number of bytes when ``salt/1`` is absent. When this option is absent, the generated salt length defaults to ``16`` bytes.',
			'Option ``length(Count)``' - 'Uses the given non-negative derived-key length. When this option is absent, the derived-key length defaults to the selected hash digest size.'
		],
		exceptions is [
			'``Hash`` is a variable' - instantiation_error,
			'``Hash`` is not an object implementing the ``hash_digest_protocol`` protocol' - domain_error(crypto_hash, 'Hash'),
			'``Password`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Password`` is a list but not a list of bytes' - type_error(list(byte), 'Password'),
			'``Password`` contains a non-integer element' - type_error(integer, 'Byte'),
			'``Password`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Options`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Options`` is a list but not a list of compound terms' - type_error(list(compound), 'Options'),
			'``Options`` contains an invalid option term' - domain_error(password_hash_option, 'Option'),
			'``Options`` contains an ``iterations/1`` value that is not an integer' - type_error(integer, 'Iterations'),
			'``Options`` contains an ``iterations/1`` value that is not a positive integer' - domain_error(positive_integer, 'Iterations'),
			'``Options`` contains a ``salt/1`` value that is not a list of bytes' - type_error(list(byte), 'Salt'),
			'``Options`` contains a ``salt/1`` value with a variable byte' - instantiation_error,
			'``Options`` contains a ``salt/1`` value with a non-integer byte' - type_error(integer, 'Byte'),
			'``Options`` contains a ``salt/1`` value with an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Options`` contains a ``salt_length/1`` or ``length/1`` value that is not an integer' - type_error(integer, 'Length'),
			'``Options`` contains a ``salt_length/1`` or ``length/1`` value that is less than zero' - domain_error(non_negative_integer, 'Length')
		]
	]).

	:- public(password_hash_needs_rehash/3).
	:- mode(password_hash_needs_rehash(+compound, +object_identifier, +list(compound)), zero_or_one_or_error).
	:- info(password_hash_needs_rehash/3, [
		comment is 'Succeeds when the given password-hash term does not match the selected PBKDF2 password-hash policy.',
		argnames is ['PasswordHash', 'Hash', 'Options'],
		remarks is [
			'Policy' - 'The ``Hash`` and ``Options`` arguments use the same policy options as ``password_hash/4``.',
			'Legacy hashes' - 'Supported non-PBKDF2 password-hash terms need rehashing when valid.'
		],
		exceptions is [
			'``PasswordHash`` is not a supported password-hash term' - domain_error(password_hash, 'PasswordHash'),
			'``PasswordHash`` contains a variable hash object' - instantiation_error,
			'``PasswordHash`` contains a hash object that does not implement the ``hash_digest_protocol`` protocol' - domain_error(crypto_hash, 'Hash'),
			'``PasswordHash`` contains an ``Iterations`` value that is not an integer' - type_error(integer, 'Iterations'),
			'``PasswordHash`` contains an ``Iterations`` value that is not a positive integer' - domain_error(positive_integer, 'Iterations'),
			'``PasswordHash`` contains a ``Salt``, ``StoredKey``, ``StoredDigest``, or ``Checksum`` value that is not a list of bytes' - type_error(list(byte), 'Bytes'),
			'``PasswordHash`` contains a ``Salt``, ``StoredKey``, ``StoredDigest``, or ``Checksum`` value with a variable byte' - instantiation_error,
			'``PasswordHash`` contains a ``Salt``, ``StoredKey``, ``StoredDigest``, or ``Checksum`` value with a non-integer byte' - type_error(integer, 'Byte'),
			'``PasswordHash`` contains a ``Salt``, ``StoredKey``, ``StoredDigest``, or ``Checksum`` value with an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``PasswordHash`` contains an invalid APR1 salt' - domain_error(apr1_salt, 'Salt'),
			'``PasswordHash`` contains an invalid APR1 checksum' - domain_error(apr1_checksum, 'Checksum'),
			'``Hash`` is a variable' - instantiation_error,
			'``Hash`` is not an object implementing the ``hash_digest_protocol`` protocol' - domain_error(crypto_hash, 'Hash'),
			'``Options`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Options`` is a list but not a list of compound terms' - type_error(list(compound), 'Options'),
			'``Options`` contains an invalid option term' - domain_error(password_hash_option, 'Option'),
			'``Options`` contains an ``iterations/1`` value that is not an integer' - type_error(integer, 'Iterations'),
			'``Options`` contains an ``iterations/1`` value that is not a positive integer' - domain_error(positive_integer, 'Iterations'),
			'``Options`` contains a ``salt/1`` value that is not a list of bytes' - type_error(list(byte), 'Salt'),
			'``Options`` contains a ``salt/1`` value with a variable byte' - instantiation_error,
			'``Options`` contains a ``salt/1`` value with a non-integer byte' - type_error(integer, 'Byte'),
			'``Options`` contains a ``salt/1`` value with an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Options`` contains a ``salt_length/1`` or ``length/1`` value that is not an integer' - type_error(integer, 'Length'),
			'``Options`` contains a ``salt_length/1`` or ``length/1`` value that is less than zero' - domain_error(non_negative_integer, 'Length'),
			'``Options`` selects a ``length/1`` value that exceeds the maximum PBKDF2 output length' - domain_error(pbkdf2_output_length, 'Length')
		]
	]).

	:- public(verify_password_hash/2).
	:- mode(verify_password_hash(+compound, +list(byte)), zero_or_one_or_error).
	:- info(verify_password_hash/2, [
		comment is 'Succeeds when the password byte sequence matches the given structured password-hash term or stored digest term.',
		argnames is ['PasswordHash', 'Password'],
		exceptions is [
			'``PasswordHash`` is not a supported password-hash term' - domain_error(password_hash, 'PasswordHash'),
			'``PasswordHash`` contains a variable hash object' - instantiation_error,
			'``PasswordHash`` contains a hash object that does not implement the ``hash_digest_protocol`` protocol' - domain_error(crypto_hash, 'Hash'),
			'``PasswordHash`` contains an ``Iterations`` value that is not an integer' - type_error(integer, 'Iterations'),
			'``PasswordHash`` contains an ``Iterations`` value that is not a positive integer' - domain_error(positive_integer, 'Iterations'),
			'``PasswordHash`` contains a ``Salt``, ``StoredKey``, ``StoredDigest``, or ``Checksum`` value that is not a list of bytes' - type_error(list(byte), 'Bytes'),
			'``PasswordHash`` contains a ``Salt``, ``StoredKey``, ``StoredDigest``, or ``Checksum`` value with a variable byte' - instantiation_error,
			'``PasswordHash`` contains a ``Salt``, ``StoredKey``, ``StoredDigest``, or ``Checksum`` value with a non-integer byte' - type_error(integer, 'Byte'),
			'``PasswordHash`` contains a ``Salt``, ``StoredKey``, ``StoredDigest``, or ``Checksum`` value with an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``PasswordHash`` contains an invalid APR1 salt' - domain_error(apr1_salt, 'Salt'),
			'``PasswordHash`` contains an invalid APR1 checksum' - domain_error(apr1_checksum, 'Checksum'),
			'``Password`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Password`` is a list but not a list of bytes' - type_error(list(byte), 'Password'),
			'``Password`` contains a non-integer element' - type_error(integer, 'Byte'),
			'``Password`` contains an integer outside the byte range' - domain_error(byte, 'Byte')
		]
	]).

	:- uses(list, [
		append/2, append/3, length/2
	]).

	:- uses(fast_random(xoshiro128pp), [
		randomize/1, sequence/4
	]).

	:- uses(os, [
		cpu_time/1, pid/1, wall_time/1
	]).

	:- uses(type, [
		check/3
	]).

	:- uses(hmac, [
		digest/4
	]).

	:- uses(base64url_no_padding, [
		generate/2
	]).

	random_bytes(Count, Bytes) :-
		context(Context),
		check(non_negative_integer, Count, Context),
		catch(open('/dev/urandom', read, Stream, [type(binary)]), _, fail),
		length(Bytes, Count),
		read_random_bytes(Bytes, Stream),
		close(Stream),
		!.
	random_bytes(Count, Bytes) :-
		fallback_seed(Seed),
		randomize(Seed),
		sequence(Count, 0, 255, Bytes).

	token_hex(Count, Token) :-
		random_bytes(Count, Bytes),
		hex_bytes(Token, Bytes).

	token_urlsafe(Count, Token) :-
		random_bytes(Count, Bytes),
		generate(codes(Codes), Bytes),
		atom_codes(Token, Codes).

	random_below(UpperBound, Integer) :-
		context(Context),
		check(positive_integer, UpperBound, Context),
		integer_bit_length(UpperBound, BitLength),
		integer_bits(UpperBound, BitLength, UpperBits),
		random_below_bits(BitLength, UpperBits, Integer).

	hex_bytes(Hex, Bytes) :-
		context(Context),
		(	var(Hex) ->
			check(list(byte), Bytes, Context),
			bytes_hex(Bytes, Hex)
		;	atom(Hex) ->
			parse_hex_atom(Hex, DecodedBytes, Context),
			(	var(Bytes) ->
				Bytes = DecodedBytes
			;	check(list(byte), Bytes, Context),
				Bytes = DecodedBytes
			)
		;	throw(error(type_error(atom, Hex), Context))
		).

	secure_compare(Expected, Candidate) :-
		context(Context),
		(	var(Expected) ->
			throw(error(instantiation_error, Context))
		;	var(Candidate) ->
			throw(error(instantiation_error, Context))
		;	atom(Expected) ->
			(	atom(Candidate) ->
				atom_codes(Expected, ExpectedCodes),
				atom_codes(Candidate, CandidateCodes),
				constant_time_equal(ExpectedCodes, CandidateCodes)
			;	throw(error(type_error(atom, Candidate), Context))
			)
		;	check(list(byte), Expected, Context),
			check(list(byte), Candidate, Context),
			constant_time_equal(Expected, Candidate)
		).

	hkdf(Hash, KeyMaterial, Length, Bytes, Options) :-
		context(Context),
		check_hash(Hash, Context),
		check(list(byte), KeyMaterial, Context),
		check(non_negative_integer, Length, Context),
		Hash::digest_size(DigestSize),
		check_hkdf_output_length(Length, DigestSize, Context),
		parse_hkdf_options(Options, DigestSize, Salt, Info, Context),
		digest(Hash, Salt, KeyMaterial, PseudorandomKey),
		hkdf_expand(Hash, PseudorandomKey, Info, Length, Bytes).

	pbkdf2(Hash, Password, Salt, Iterations, Length, DerivedKey) :-
		context(Context),
		check_hash(Hash, Context),
		check(list(byte), Password, Context),
		check(list(byte), Salt, Context),
		check(positive_integer, Iterations, Context),
		check(non_negative_integer, Length, Context),
		Hash::digest_size(DigestSize),
		check_pbkdf2_output_length(Length, DigestSize, Context),
		(	Length =:= 0 ->
			DerivedKey = []
		;	pbkdf2_blocks(Hash, Password, Salt, Iterations, 1, Length, DerivedKey, [])
		).

	apr1(Password, Salt, Checksum) :-
		context(Context),
		check(list(byte), Password, Context),
		check(list(byte), Salt, Context),
		check_apr1_salt(Salt, Context),
		apr1_digest(Password, Salt, Digest),
		apr1_encode_digest(Digest, ComputedChecksum),
		Checksum = ComputedChecksum.

	password_hash(Hash, Password, PasswordHash, Options) :-
		context(Context),
		check_hash(Hash, Context),
		check(list(byte), Password, Context),
		parse_password_hash_options(Options, Hash, Iterations, Salt, Length, Context),
		pbkdf2(Hash, Password, Salt, Iterations, Length, DerivedKey),
		PasswordHash = pbkdf2(Hash, Iterations, Salt, DerivedKey).

	password_hash_needs_rehash(PasswordHash, Hash, Options) :-
		context(Context),
		check_hash(Hash, Context),
		parse_password_hash_policy_options(Options, Hash, PolicyIterations, PolicySaltLength, PolicyLength, Context),
		( 	PasswordHash = pbkdf2(_, _, _, _) ->
			check_password_hash(PasswordHash, StoredHash, Iterations, Salt, StoredKey, Context),
			( 	StoredHash \== Hash ->
				true
			; 	Iterations =\= PolicyIterations ->
				true
			; 	length(Salt, SaltLength),
				SaltLength =\= PolicySaltLength ->
				true
			; 	length(StoredKey, StoredKeyLength),
				StoredKeyLength =\= PolicyLength
			)
		; 	PasswordHash = digest(_, _) ->
			check_digest_password_hash(PasswordHash, _StoredHash, _StoredDigest, Context)
		; 	PasswordHash = apr1(_, _) ->
			check_apr1_password_hash(PasswordHash, _Salt, _Checksum, Context)
		; 	check_password_hash(PasswordHash, _StoredHash, _Iterations, _Salt, _StoredKey, Context)
		).

	verify_password_hash(PasswordHash, Password) :-
		context(Context),
		check(list(byte), Password, Context),
		(	PasswordHash = pbkdf2(_, _, _, _) ->
			check_password_hash(PasswordHash, Hash, Iterations, Salt, StoredKey, Context),
			length(StoredKey, Length),
			pbkdf2(Hash, Password, Salt, Iterations, Length, DerivedKey),
			secure_compare(StoredKey, DerivedKey)
		;	PasswordHash = digest(_, _) ->
			check_digest_password_hash(PasswordHash, Hash, StoredDigest, Context),
			Hash::digest(Password, ComputedDigest),
			secure_compare(StoredDigest, ComputedDigest)
		;	PasswordHash = apr1(_, _) ->
			check_apr1_password_hash(PasswordHash, Salt, StoredChecksum, Context),
			apr1(Password, Salt, ComputedChecksum),
			secure_compare(StoredChecksum, ComputedChecksum)
		;	check_password_hash(PasswordHash, _Hash, _Iterations, _Salt, _StoredKey, Context)
		).

	fallback_seed(Seed) :-
		pid(PID),
		cpu_time(CPU0),
		wall_time(Wall0),
		CPU is round(CPU0),
		Wall is round(Wall0),
		W is 0xFFFFFFFF,
		S0 is xor(xor(PID, Wall << 8), CPU << 16) /\ W,
		S1 is xor(S0, S0 >> 11),
		S2 is xor(S1, S1 << 7) /\ W,
		S3 is xor(S2, S2 >> 17),
		Seed is xor(S3, (S3 << 5)) /\ W.

	integer_bit_length(Integer, Length) :-
		integer_bit_length(Integer, 0, Length).

	integer_bit_length(0, Length, Length) :-
		!.
	integer_bit_length(Integer, Length0, Length) :-
		NextInteger is Integer >> 1,
		NextLength is Length0 + 1,
		integer_bit_length(NextInteger, NextLength, Length).

	integer_bits(_Integer, 0, []) :-
		!.
	integer_bits(Integer, Length, [Bit| Bits]) :-
		Shift is Length - 1,
		Bit is (Integer >> Shift) /\ 1,
		integer_bits(Integer, Shift, Bits).

	random_below_bits(BitLength, UpperBits, Integer) :-
		random_bytes(BitLength, Bytes),
		bytes_bits(Bytes, Bits),
		(	\+ bits_less_than(Bits, UpperBits) ->
			random_below_bits(BitLength, UpperBits, Integer)
		;	bits_integer(Bits, 0, Integer)
		).

	bytes_bits([], []).
	bytes_bits([Byte| Bytes], [Bit| Bits]) :-
		Bit is Byte /\ 1,
		bytes_bits(Bytes, Bits).

	bits_less_than([0| _], [1| _]) :-
		!.
	bits_less_than([Bit| Bits], [Bit| UpperBits]) :-
		bits_less_than(Bits, UpperBits).

	bits_integer([], Integer, Integer).
	bits_integer([Bit| Bits], Integer0, Integer) :-
		Integer1 is (Integer0 << 1) \/ Bit,
		bits_integer(Bits, Integer1, Integer).

	check_hash(Hash, Context) :-
		(	var(Hash) ->
			throw(error(instantiation_error, Context))
		;	conforms_to_protocol(Hash, hash_digest_protocol) ->
			true
		;	throw(error(domain_error(crypto_hash, Hash), Context))
		).

	check_hkdf_output_length(0, _, _) :-
		!.
	check_hkdf_output_length(Length, DigestSize, Context) :-
		BlockCount is ((Length - 1) // DigestSize) + 1,
		MaxLength is 255 * DigestSize,
		(	BlockCount =< 255 ->
			true
		;	throw(error(domain_error(hkdf_output_length(0, MaxLength), Length), Context))
		).

	check_pbkdf2_output_length(0, _, _) :-
		!.
	check_pbkdf2_output_length(Length, DigestSize, Context) :-
		BlockCount is ((Length - 1) // DigestSize) + 1,
		(	BlockCount =< 0xFFFFFFFF ->
			true
		;	throw(error(domain_error(pbkdf2_output_length, Length), Context))
		).

	parse_hex_atom(Hex, Bytes, Context) :-
		atom_codes(Hex, Codes),
		(	Codes == [] ->
			Bytes = []
		;	parse_hex_codes(Codes, Hex, Bytes, Context)
		).

	parse_hex_codes([], _, [], _) :-
		!.
	parse_hex_codes([HighCode, LowCode| Codes], Hex, [Byte| Bytes], Context) :-
		!,
		hex_digit_value(HighCode, High, Hex, Context),
		hex_digit_value(LowCode, Low, Hex, Context),
		Byte is (High << 4) + Low,
		parse_hex_codes(Codes, Hex, Bytes, Context).
	parse_hex_codes(_, Hex, _, Context) :-
		throw(error(domain_error(hexadecimal_atom, Hex), Context)).

	hex_digit_value(Code, Value, _Hex, _Context) :-
		0'0 =< Code,
		Code =< 0'9,
		!,
		Value is Code - 0'0.
	hex_digit_value(Code, Value, _Hex, _Context) :-
		0'a =< Code,
		Code =< 0'f,
		!,
		Value is Code - 0'a + 10.
	hex_digit_value(Code, Value, _Hex, _Context) :-
		0'A =< Code,
		Code =< 0'F,
		!,
		Value is Code - 0'A + 10.
	hex_digit_value(_Code, _Value, Hex, Context) :-
		throw(error(domain_error(hexadecimal_atom, Hex), Context)).

	bytes_hex(Bytes, Hex) :-
		bytes_hex_codes(Bytes, Codes),
		atom_codes(Hex, Codes).

	bytes_hex_codes([], []).
	bytes_hex_codes([Byte| Bytes], [HighCode, LowCode| Codes]) :-
		High is (Byte >> 4) /\ 0x0F,
		Low is Byte /\ 0x0F,
		hex_digit_code(High, HighCode),
		hex_digit_code(Low, LowCode),
		bytes_hex_codes(Bytes, Codes).

	hex_digit_code( 0, 0'0).
	hex_digit_code( 1, 0'1).
	hex_digit_code( 2, 0'2).
	hex_digit_code( 3, 0'3).
	hex_digit_code( 4, 0'4).
	hex_digit_code( 5, 0'5).
	hex_digit_code( 6, 0'6).
	hex_digit_code( 7, 0'7).
	hex_digit_code( 8, 0'8).
	hex_digit_code( 9, 0'9).
	hex_digit_code(10, 0'a).
	hex_digit_code(11, 0'b).
	hex_digit_code(12, 0'c).
	hex_digit_code(13, 0'd).
	hex_digit_code(14, 0'e).
	hex_digit_code(15, 0'f).

	parse_hkdf_options(Options, DigestSize, Salt, Info, Context) :-
		check(list(compound), Options, Context),
		parse_hkdf_options(Options, none, none, DigestSize, Salt, Info, Context).

	parse_hkdf_options([], SaltOption, InfoOption, DigestSize, Salt, Info, _) :-
		(	SaltOption == none ->
			zero_bytes(DigestSize, Salt)
		;	Salt = SaltOption
		),
		(	InfoOption == none ->
			Info = []
		;	Info = InfoOption
		).
	parse_hkdf_options([Option| Options], Salt0, Info0, DigestSize, Salt, Info, Context) :-
		parse_hkdf_option(Option, Salt0, Info0, Salt1, Info1, Context),
		parse_hkdf_options(Options, Salt1, Info1, DigestSize, Salt, Info, Context).

	parse_hkdf_option(Option, _Salt0, Info0, Salt, Info, Context) :-
		(	Option = salt(SaltBytes) ->
			check(list(byte), SaltBytes, Context),
			Salt = SaltBytes,
			Info = Info0
		;	Option = info(InfoBytes) ->
			check(list(byte), InfoBytes, Context),
			Salt = _Salt0,
			Info = InfoBytes
		;	throw(error(domain_error(hkdf_option, Option), Context))
		).

	parse_password_hash_options(Options, Hash, Iterations, Salt, Length, Context) :-
		check(list(compound), Options, Context),
		Hash::digest_size(DefaultLength),
		(	var(Options) ->
			throw(error(instantiation_error, Context))
		;	parse_password_hash_options(Options, 131072, none, 16, DefaultLength, Iterations, SaltOption, SaltLength, Length, Context),
			(	SaltOption == none ->
				random_bytes(SaltLength, Salt)
			;	Salt = SaltOption
			)
		).

	parse_password_hash_options([], Iterations, Salt, SaltLength, Length, Iterations, Salt, SaltLength, Length, _) :-
		!.
	parse_password_hash_options([Option| Options], Iterations0, Salt0, SaltLength0, Length0, Iterations, Salt, SaltLength, Length, Context) :-
		!,
		parse_password_hash_option(Option, Iterations0, Salt0, SaltLength0, Length0, Iterations1, Salt1, SaltLength1, Length1, Context),
		parse_password_hash_options(Options, Iterations1, Salt1, SaltLength1, Length1, Iterations, Salt, SaltLength, Length, Context).
	parse_password_hash_options(Options, _, _, _, _, _, _, _, _, Context) :-
		throw(error(type_error(list, Options), Context)).

	parse_password_hash_option(Option, Iterations0, Salt0, SaltLength0, Length0, Iterations, Salt, SaltLength, Length, Context) :-
		(	var(Option) ->
			throw(error(instantiation_error, Context))
		;	Option = iterations(Value) ->
			check(positive_integer, Value, Context),
			Iterations = Value,
			Salt = Salt0,
			SaltLength = SaltLength0,
			Length = Length0
		;	Option = salt(SaltBytes) ->
			check(list(byte), SaltBytes, Context),
			Iterations = Iterations0,
			Salt = SaltBytes,
			SaltLength = SaltLength0,
			Length = Length0
		;	Option = salt_length(Value) ->
			check(non_negative_integer, Value, Context),
			Iterations = Iterations0,
			Salt = Salt0,
			SaltLength = Value,
			Length = Length0
		;	Option = length(Value) ->
			check(non_negative_integer, Value, Context),
			Iterations = Iterations0,
			Salt = Salt0,
			SaltLength = SaltLength0,
			Length = Value
		;	throw(error(domain_error(password_hash_option, Option), Context))
		).

	parse_password_hash_policy_options(Options, Hash, Iterations, SaltLength, Length, Context) :-
		check(list(compound), Options, Context),
		Hash::digest_size(DefaultLength),
		( 	var(Options) ->
			throw(error(instantiation_error, Context))
		; 	parse_password_hash_options(Options, 131072, none, 16, DefaultLength, Iterations, SaltOption, SaltLength0, Length, Context),
			check_pbkdf2_output_length(Length, DefaultLength, Context),
			( 	SaltOption == none ->
				SaltLength = SaltLength0
			; 	length(SaltOption, SaltLength)
			)
		).

	check_password_hash(PasswordHash, Hash, Iterations, Salt, StoredKey, Context) :-
		(	var(PasswordHash) ->
			throw(error(instantiation_error, Context))
		;	PasswordHash = pbkdf2(Hash0, Iterations0, Salt0, StoredKey0) ->
			check_hash(Hash0, Context),
			check(positive_integer, Iterations0, Context),
			check(list(byte), Salt0, Context),
			check(list(byte), StoredKey0, Context),
			Hash = Hash0,
			Iterations = Iterations0,
			Salt = Salt0,
			StoredKey = StoredKey0
		;	throw(error(domain_error(password_hash, PasswordHash), Context))
		).

	check_digest_password_hash(PasswordHash, Hash, StoredDigest, Context) :-
		(	var(PasswordHash) ->
			throw(error(instantiation_error, Context))
		;	PasswordHash = digest(Hash0, StoredDigest0) ->
			check_hash(Hash0, Context),
			check(list(byte), StoredDigest0, Context),
			Hash = Hash0,
			StoredDigest = StoredDigest0
		;	throw(error(domain_error(password_hash, PasswordHash), Context))
		).

	check_apr1_password_hash(PasswordHash, Salt, Checksum, Context) :-
		(	var(PasswordHash) ->
			throw(error(instantiation_error, Context))
		;	PasswordHash = apr1(Salt0, Checksum0) ->
			check(list(byte), Salt0, Context),
			check(list(byte), Checksum0, Context),
			check_apr1_salt(Salt0, Context),
			check_apr1_checksum(Checksum0, Context),
			Salt = Salt0,
			Checksum = Checksum0
		;	throw(error(domain_error(password_hash, PasswordHash), Context))
		).

	check_apr1_salt(Salt, Context) :-
		length(Salt, Length),
		(	Length > 0,
			Length =< 8,
			apr1_base64_codes(Salt) ->
			true
		;	throw(error(domain_error(apr1_salt, Salt), Context))
		).

	check_apr1_checksum(Checksum, Context) :-
		length(Checksum, Length),
		(	Length =:= 22,
			apr1_base64_codes(Checksum) ->
			true
		;	throw(error(domain_error(apr1_checksum, Checksum), Context))
		).

	apr1_digest(Password, Salt, Digest) :-
		length(Password, PasswordLength),
		append([Password, Salt, Password], AlternateInput),
		md5::digest(AlternateInput, AlternateDigest),
		copy_repeated_bytes(AlternateDigest, PasswordLength, AlternatePrefix, []),
		apr1_length_mixing(PasswordLength, Password, LengthMixing),
		append([Password, [0'$, 0'a, 0'p, 0'r, 0'1, 0'$], Salt, AlternatePrefix, LengthMixing], InitialInput),
		md5::digest(InitialInput, InitialDigest),
		apr1_rounds(0, Password, Salt, InitialDigest, Digest).

	apr1_length_mixing(0, _Password, []) :-
		!.
	apr1_length_mixing(Length, [PasswordByte| _], Mixing) :-
		apr1_length_mixing(Length, PasswordByte, Mixing, []).

	apr1_length_mixing(0, _PasswordByte, Mixing, Mixing) :-
		!.
	apr1_length_mixing(Length, PasswordByte, Mixing0, Mixing) :-
		(	Length /\ 1 =:= 1 ->
			Mixing0 = [0| Mixing1]
		;	Mixing0 = [PasswordByte| Mixing1]
		),
		NextLength is Length >> 1,
		apr1_length_mixing(NextLength, PasswordByte, Mixing1, Mixing).

	apr1_rounds(1000, _Password, _Salt, Digest, Digest) :-
		!.
	apr1_rounds(Index, Password, Salt, Digest0, Digest) :-
		apr1_round_input(Index, Password, Salt, Digest0, Input),
		md5::digest(Input, Digest1),
		NextIndex is Index + 1,
		apr1_rounds(NextIndex, Password, Salt, Digest1, Digest).

	apr1_round_input(Index, Password, Salt, Digest, Input) :-
		(	Index /\ 1 =:= 1 ->
			Prefix = Password,
			Suffix = Digest
		;	Prefix = Digest,
			Suffix = Password
		),
		(	Index mod 3 =:= 0 ->
			SaltPart = []
		;	SaltPart = Salt
		),
		(	Index mod 7 =:= 0 ->
			PasswordPart = []
		;	PasswordPart = Password
		),
		append([Prefix, SaltPart, PasswordPart, Suffix], Input).

	apr1_encode_digest([
			Byte00, Byte01, Byte02, Byte03, Byte04, Byte05, Byte06, Byte07,
			Byte08, Byte09, Byte10, Byte11, Byte12, Byte13, Byte14, Byte15
		], Checksum) :-
		apr1_to64((Byte00 << 16) \/ (Byte06 << 8) \/ Byte12, 4, Checksum, Checksum1),
		apr1_to64((Byte01 << 16) \/ (Byte07 << 8) \/ Byte13, 4, Checksum1, Checksum2),
		apr1_to64((Byte02 << 16) \/ (Byte08 << 8) \/ Byte14, 4, Checksum2, Checksum3),
		apr1_to64((Byte03 << 16) \/ (Byte09 << 8) \/ Byte15, 4, Checksum3, Checksum4),
		apr1_to64((Byte04 << 16) \/ (Byte10 << 8) \/ Byte05, 4, Checksum4, Checksum5),
		apr1_to64(Byte11, 2, Checksum5, []).

	apr1_to64(_Value, 0, Checksum, Checksum) :-
		!.
	apr1_to64(Value, Count, [Code| Codes0], Codes) :-
		Index is Value /\ 0x3f,
		apr1_base64_code(Index, Code),
		NextValue is Value >> 6,
		NextCount is Count - 1,
		apr1_to64(NextValue, NextCount, Codes0, Codes).

	apr1_base64_code(Index, Code) :-
		(	Index =:= 0 ->
			Code is 0'.
		;	Index =:= 1 ->
			Code is 0'/
		;	Index =< 11 ->
			Code is 0'0 + Index - 2
		;	Index =< 37 ->
			Code is 0'A + Index - 12
		;	Code is 0'a + Index - 38
		),
		!.

	apr1_base64_codes([]).
	apr1_base64_codes([Code| Codes]) :-
		apr1_base64_code(Code),
		apr1_base64_codes(Codes).

	apr1_base64_code(Code) :-
		(	Code =:= 0'.
		;	Code =:= 0'/
		;	0'0 =< Code, Code =< 0'9
		;	0'A =< Code, Code =< 0'Z
		;	0'a =< Code, Code =< 0'z
		),
		!.

	copy_repeated_bytes(_Bytes, 0, Output, Output) :-
		!.
	copy_repeated_bytes(Bytes, Count0, Output0, Output) :-
		copy_output_bytes(Bytes, Count0, Count, Output0, Output1),
		copy_repeated_bytes(Bytes, Count, Output1, Output).

	zero_bytes(0, []) :-
		!.
	zero_bytes(Count, [0| Bytes]) :-
		NextCount is Count - 1,
		zero_bytes(NextCount, Bytes).

	hkdf_expand(_Hash, _PseudorandomKey, _Info, 0, []) :-
		!.
	hkdf_expand(Hash, PseudorandomKey, Info, Length, Bytes) :-
		Hash::digest_size(DigestSize),
		BlockCount is ((Length - 1) // DigestSize) + 1,
		hkdf_expand_blocks(Hash, PseudorandomKey, Info, 1, BlockCount, [], Length, Bytes, []).

	hkdf_expand_blocks(_Hash, _PseudorandomKey, _Info, _Index, _BlockCount, _Previous, 0, Bytes, Bytes) :-
		!.
	hkdf_expand_blocks(_Hash, _PseudorandomKey, _Info, Index, BlockCount, _Previous, _Remaining, Bytes, Bytes) :-
		Index > BlockCount,
		!.
	hkdf_expand_blocks(Hash, PseudorandomKey, Info, Index, BlockCount, Previous, Remaining0, Bytes0, Bytes) :-
		build_hkdf_message(Previous, Info, Index, Message),
		digest(Hash, PseudorandomKey, Message, Block),
		copy_output_bytes(Block, Remaining0, Remaining, Bytes0, Bytes1),
		NextIndex is Index + 1,
		hkdf_expand_blocks(Hash, PseudorandomKey, Info, NextIndex, BlockCount, Block, Remaining, Bytes1, Bytes).

	copy_output_bytes(_Block, 0, 0, Bytes, Bytes) :-
		!.
	copy_output_bytes([], Remaining, Remaining, Bytes, Bytes).
	copy_output_bytes([Byte| Block], Remaining0, Remaining, [Byte| Bytes], Tail) :-
		Remaining1 is Remaining0 - 1,
		copy_output_bytes(Block, Remaining1, Remaining, Bytes, Tail).

	build_hkdf_message(Previous, Info, Index, Message) :-
		copy_hkdf_prefix(Previous, Info, Index, Message).

	copy_hkdf_prefix([], Info, Index, Message) :-
		copy_hkdf_suffix(Info, Index, Message).
	copy_hkdf_prefix([Byte| Bytes], Info, Index, [Byte| Message]) :-
		copy_hkdf_prefix(Bytes, Info, Index, Message).

	copy_hkdf_suffix([], Index, [Index]).
	copy_hkdf_suffix([Byte| Bytes], Index, [Byte| Message]) :-
		copy_hkdf_suffix(Bytes, Index, Message).

	pbkdf2_blocks(_Hash, _Password, _Salt, _Iterations, _Index, 0, DerivedKey, DerivedKey) :-
		!.
	pbkdf2_blocks(Hash, Password, Salt, Iterations, Index, Remaining0, DerivedKey0, DerivedKey) :-
		pbkdf2_block(Hash, Password, Salt, Iterations, Index, Block),
		copy_output_bytes(Block, Remaining0, Remaining, DerivedKey0, DerivedKey1),
		NextIndex is Index + 1,
		pbkdf2_blocks(Hash, Password, Salt, Iterations, NextIndex, Remaining, DerivedKey1, DerivedKey).

	pbkdf2_block(Hash, Password, Salt, Iterations, Index, Block) :-
		integer_to_big_endian_bytes32(Index, CounterBytes),
		append(Salt, CounterBytes, Message),
		digest(Hash, Password, Message, U1),
		pbkdf2_iterate(Hash, Password, Iterations, U1, U1, Block).

	pbkdf2_iterate(_Hash, _Password, 1, _Current, Accumulator, Accumulator) :-
		!.
	pbkdf2_iterate(Hash, Password, Iterations, Current, Accumulator0, Accumulator) :-
		digest(Hash, Password, Current, Next),
		xor_bytes(Accumulator0, Next, Accumulator1),
		NextIterations is Iterations - 1,
		pbkdf2_iterate(Hash, Password, NextIterations, Next, Accumulator1, Accumulator).

	xor_bytes([], [], []).
	xor_bytes([Byte1| Bytes1], [Byte2| Bytes2], [XorByte| XorBytes]) :-
		XorByte is xor(Byte1, Byte2),
		xor_bytes(Bytes1, Bytes2, XorBytes).

	integer_to_big_endian_bytes32(Integer, [B0, B1, B2, B3]) :-
		B0 is (Integer >> 24) /\ 0xFF,
		B1 is (Integer >> 16) /\ 0xFF,
		B2 is (Integer >> 8) /\ 0xFF,
		B3 is Integer /\ 0xFF.

	constant_time_equal(Expected, Candidate) :-
		constant_time_difference(Expected, Candidate, 0, Difference),
		Difference =:= 0.

	constant_time_difference([], [], Difference, Difference) :-
		!.
	constant_time_difference([ExpectedByte| Expected], [], Difference0, Difference) :-
		!,
		Difference1 is Difference0 \/ ExpectedByte \/ 1,
		constant_time_difference(Expected, [], Difference1, Difference).
	constant_time_difference([], [CandidateByte| Candidate], Difference0, Difference) :-
		!,
		Difference1 is Difference0 \/ CandidateByte \/ 1,
		constant_time_difference([], Candidate, Difference1, Difference).
	constant_time_difference([ExpectedByte| Expected], [CandidateByte| Candidate], Difference0, Difference) :-
		!,
		Difference1 is Difference0 \/ xor(ExpectedByte, CandidateByte),
		constant_time_difference(Expected, Candidate, Difference1, Difference).

	read_random_bytes([], _).
	read_random_bytes([Byte| Bytes], Stream) :-
		get_byte(Stream, Byte),
		read_random_bytes(Bytes, Stream).

:- end_object.
