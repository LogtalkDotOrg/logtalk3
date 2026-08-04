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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-08-04,
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
	:- mode(pbkdf2(+object_identifier, +list(byte), +list(byte), +integer, +positive_integer, -list(byte)), one_or_error).
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
			'``Length`` is an integer but is not positive' - domain_error(positive_integer, 'Length'),
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

	:- public(bcrypt/4).
	:- mode(bcrypt(+list(byte), +integer, +list(byte), -list(byte)), one_or_error).
	:- info(bcrypt/4, [
		comment is 'Computes a bcrypt version 2b encoded checksum for a password, cost, and raw 16-byte salt.',
		argnames is ['Password', 'Cost', 'Salt', 'Checksum'],
		exceptions is [
			'``Password`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Password`` is a list but not a list of bytes' - type_error(list(byte), 'Password'),
			'``Password`` contains a non-integer element' - type_error(integer, 'Byte'),
			'``Password`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Password`` contains more than 72 bytes' - domain_error(bcrypt_password_length, 'Password'),
			'``Cost`` is a variable' - instantiation_error,
			'``Cost`` is not an integer' - type_error(integer, 'Cost'),
			'``Cost`` is outside the range 4 through 31' - domain_error(bcrypt_cost, 'Cost'),
			'``Salt`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Salt`` is a list but not a list of bytes' - type_error(list(byte), 'Salt'),
			'``Salt`` is not exactly 16 bytes long' - domain_error(bcrypt_salt, 'Salt')
		]
	]).

	:- public(password_hash/4).
	:- mode(password_hash(+object_identifier, +list(byte), -compound, +list(compound)), one_or_error).
	:- mode(password_hash(+compound, +list(byte), -compound, +list(compound)), one_or_error).
	:- info(password_hash/4, [
		comment is 'Computes a structured password-hash term using the selected password-hashing method and derivation options.',
		argnames is ['Method', 'Password', 'PasswordHash', 'Options'],
		remarks is [
			'Method' - 'Supported methods are ``pbkdf2(Hash)``, ``bcrypt``, and ``apr1``. A hash object is interpreted as shorthand for ``pbkdf2(Hash)``.',
			'Repeated options' - 'When the same password-hash option is given multiple times, the last occurrence is used.',
			'Option ``iterations(Count)``' - 'Uses the given positive integer PBKDF2 iteration count. When this option is absent, the iteration count defaults to ``131072``.',
			'Option ``cost(Count)``' - 'Uses the given bcrypt cost from 4 through 31. When this option is absent, the cost defaults to ``12``.',
			'Option ``salt(Bytes)``' - 'Uses the given method-specific salt. Bcrypt requires exactly 16 raw bytes; APR1 requires one through eight characters from its base64 alphabet.',
			'Option ``salt_length(Count)``' - 'Generates a random PBKDF2 or APR1 salt when ``salt/1`` is absent. PBKDF2 defaults to 16 bytes and APR1 defaults to 8 characters.',
			'Option ``length(Count)``' - 'Uses the given positive PBKDF2 derived-key length. When this option is absent, the derived-key length defaults to the selected hash digest size.'
		],
		exceptions is [
			'``Method`` is not a ground term' - instantiation_error,
			'``Method`` is a ground ``Hash`` or ``pbkdf2(Hash)`` term but not an object implementing the ``hash_digest_protocol`` protocol' - domain_error(crypto_hash, 'Hash'),
			'``Password`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Password`` is a list but not a list of bytes' - type_error(list(byte), 'Password'),
			'``Password`` contains a non-integer element' - type_error(integer, 'Byte'),
			'``Password`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Password`` contains more than 72 bytes when using bcrypt' - domain_error(bcrypt_password_length, 'Password'),
			'``Options`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Options`` is a list but not a list of compound terms' - type_error(list(compound), 'Options'),
			'``Options`` contains an invalid option term' - domain_error(password_hash_option, 'Option'),
			'``Options`` contains an ``iterations/1`` value that is not an integer' - type_error(integer, 'Iterations'),
			'``Options`` contains an ``iterations/1`` value that is not a positive integer' - domain_error(positive_integer, 'Iterations'),
			'``Options`` contains a ``cost/1`` value that is not an integer' - type_error(integer, 'Cost'),
			'``Options`` contains a ``cost/1`` value outside the range 4 through 31' - domain_error(bcrypt_cost, 'Cost'),
			'``Options`` contains a ``salt/1`` value that is not a list of bytes' - type_error(list(byte), 'Salt'),
			'``Options`` contains a ``salt/1`` value with a variable byte' - instantiation_error,
			'``Options`` contains a ``salt/1`` value with a non-integer byte' - type_error(integer, 'Byte'),
			'``Options`` contains a ``salt/1`` value with an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Options`` contains a bcrypt ``salt/1`` value that is not exactly 16 bytes long' - domain_error(bcrypt_salt, 'Salt'),
			'``Options`` contains an invalid APR1 ``salt/1`` value' - domain_error(apr1_salt, 'Salt'),
			'``Options`` contains a ``salt_length/1`` or ``length/1`` value that is not an integer' - type_error(integer, 'Length'),
			'``Options`` contains a PBKDF2 ``salt_length/1`` value that is less than zero' - domain_error(non_negative_integer, 'Length'),
			'``Options`` contains an APR1 ``salt_length/1`` value outside the range 1 through 8' - domain_error(apr1_salt_length, 'Length'),
			'``Options`` contains a ``length/1`` value that is not positive' - domain_error(positive_integer, 'Length'),
			'``Options`` selects a PBKDF2 ``length/1`` value that exceeds the maximum output length' - domain_error(pbkdf2_output_length, 'Length')
		]
	]).

	:- public(password_hash_needs_rehash/3).
	:- mode(password_hash_needs_rehash(+compound, +object_identifier, +list(compound)), zero_or_one_or_error).
	:- mode(password_hash_needs_rehash(+compound, +compound, +list(compound)), zero_or_one_or_error).
	:- info(password_hash_needs_rehash/3, [
		comment is 'Succeeds when the given password-hash term does not match the selected password-hashing method and policy.',
		argnames is ['PasswordHash', 'Method', 'Options'],
		remarks is [
			'Method' - 'Supported target methods are ``pbkdf2(Hash)``, ``bcrypt``, and ``apr1``. A hash object is interpreted as shorthand for ``pbkdf2(Hash)``.',
			'Policy' - 'The ``Method`` and ``Options`` arguments use the same method and policy options as ``password_hash/4``.',
			'Method changes' - 'A valid stored hash using a different method always needs rehashing.'
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
			'``PasswordHash`` contains a bcrypt cost that is not an integer' - type_error(integer, 'Cost'),
			'``PasswordHash`` contains a bcrypt cost outside the range 4 through 31' - domain_error(bcrypt_cost, 'Cost'),
			'``PasswordHash`` contains a bcrypt salt that is not exactly 16 bytes long' - domain_error(bcrypt_salt, 'Salt'),
			'``PasswordHash`` contains an invalid bcrypt checksum' - domain_error(bcrypt_checksum, 'Checksum'),
			'``Method`` is not a ground term' - instantiation_error,
			'``Method`` is a ground ``Hash`` or ``pbkdf2(Hash)`` term but not an object implementing the ``hash_digest_protocol`` protocol' - domain_error(crypto_hash, 'Hash'),
			'``Options`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Options`` is a list but not a list of compound terms' - type_error(list(compound), 'Options'),
			'``Options`` contains an invalid option term' - domain_error(password_hash_option, 'Option'),
			'``Options`` contains an ``iterations/1`` value that is not an integer' - type_error(integer, 'Iterations'),
			'``Options`` contains an ``iterations/1`` value that is not a positive integer' - domain_error(positive_integer, 'Iterations'),
			'``Options`` contains a ``cost/1`` value that is not an integer' - type_error(integer, 'Cost'),
			'``Options`` contains a ``cost/1`` value outside the range 4 through 31' - domain_error(bcrypt_cost, 'Cost'),
			'``Options`` contains a ``salt/1`` value that is not a list of bytes' - type_error(list(byte), 'Salt'),
			'``Options`` contains a ``salt/1`` value with a variable byte' - instantiation_error,
			'``Options`` contains a ``salt/1`` value with a non-integer byte' - type_error(integer, 'Byte'),
			'``Options`` contains a ``salt/1`` value with an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Options`` contains a bcrypt ``salt/1`` value that is not exactly 16 bytes long' - domain_error(bcrypt_salt, 'Salt'),
			'``Options`` contains an invalid APR1 ``salt/1`` value' - domain_error(apr1_salt, 'Salt'),
			'``Options`` contains a ``salt_length/1`` or ``length/1`` value that is not an integer' - type_error(integer, 'Length'),
			'``Options`` contains a PBKDF2 ``salt_length/1`` value that is less than zero' - domain_error(non_negative_integer, 'Length'),
			'``Options`` contains an APR1 ``salt_length/1`` value outside the range 1 through 8' - domain_error(apr1_salt_length, 'Length'),
			'``Options`` contains a ``length/1`` value that is not positive' - domain_error(positive_integer, 'Length'),
			'``Options`` selects a PBKDF2 ``length/1`` value that exceeds the maximum output length' - domain_error(pbkdf2_output_length, 'Length')
		]
	]).

	:- public(verify_password_hash/2).
	:- mode(verify_password_hash(+compound, +list(byte)), zero_or_one_or_error).
	:- info(verify_password_hash/2, [
		comment is 'Succeeds when the password byte sequence matches the given structured password-hash term or stored digest term.',
		argnames is ['PasswordHash', 'Password'],
		exceptions is [
			'``PasswordHash`` is not a ground term' - instantiation_error,
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

	:- public(password_hash_atom/2).
	:- mode(password_hash_atom(+compound, ?atom), zero_or_one_or_error).
	:- mode(password_hash_atom(-compound, +atom), one_or_error).
	:- info(password_hash_atom/2, [
		comment is 'Converts between a structured bcrypt 2b or APR1 password-hash term and its canonical modular-crypt atom.',
		argnames is ['PasswordHash', 'Atom'],
		exceptions is [
			'Both arguments are variables' - instantiation_error,
			'``Atom`` is neither a variable nor an atom' - type_error(atom, 'Atom'),
			'The instantiated argument does not represent a supported canonical password hash' - domain_error(password_hash, 'Value')
		]
	]).

	:- private(xor_bytes/3).
	:- mode(xor_bytes(+list(byte), +list(byte), -list(byte)), one).
	:- info(xor_bytes/3, [
		comment is 'Computes the byte-wise exclusive disjunction of two equal-length byte lists.',
		argnames is ['Bytes1', 'Bytes2', 'XorBytes']
	]).

	:- private(constant_time_equal/2).
	:- mode(constant_time_equal(+list(byte), +list(byte)), zero_or_one).
	:- info(constant_time_equal/2, [
		comment is 'Compares two byte lists using a constant-time strategy.',
		argnames is ['Expected', 'Candidate']
	]).

	:- uses(list, [
		append/2, append/3, length/2, nth0/3
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
		check(positive_integer, Length, Context),
		Hash::digest_size(DigestSize),
		check_pbkdf2_output_length(Length, DigestSize, Context),
		pbkdf2_blocks(Length, Hash, Password, Salt, Iterations, 1, DerivedKey, []).

	apr1(Password, Salt, Checksum) :-
		context(Context),
		check(list(byte), Password, Context),
		check(list(byte), Salt, Context),
		check_apr1_salt(Salt, Context),
		apr1_digest(Password, Salt, Digest),
		apr1_encode_digest(Digest, ComputedChecksum),
		Checksum = ComputedChecksum.

	bcrypt(Password, Cost, Salt, Checksum) :-
		context(Context),
		check(list(byte), Password, Context),
		check_bcrypt_password(Password, Context),
		check_bcrypt_cost(Cost, Context),
		check(list(byte), Salt, Context),
		check_bcrypt_salt(Salt, Context),
		append(Password, [0], Key),
		bcrypt_initial_state(State0),
		bcrypt_expand_key(Key, Salt, State0, State1),
		Rounds is 1 << Cost,
		bcrypt_cost_rounds(Rounds, Key, Salt, State1, State),
		bcrypt_ciphertext(Plaintext),
		bcrypt_encrypt_ciphertext(Plaintext, State, Ciphertext),
		bcrypt_first_bytes(23, Ciphertext, EncodedBytes),
		bcrypt_base64_encode(EncodedBytes, Checksum).

	password_hash(Method, _Password, _PasswordHash, _Options) :-
		\+ ground(Method),
		instantiation_error.
	password_hash(pbkdf2(Hash), Password, PasswordHash, Options) :-
		!,
		password_hash(Hash, Password, PasswordHash, Options).
	password_hash(bcrypt, Password, PasswordHash, Options) :-
		!,
		context(Context),
		check(list(byte), Password, Context),
		check_bcrypt_password(Password, Context),
		parse_bcrypt_options(Options, Cost, Salt, Context),
		bcrypt(Password, Cost, Salt, Checksum),
		PasswordHash = bcrypt(Cost, Salt, Checksum).
	password_hash(apr1, Password, PasswordHash, Options) :-
		!,
		context(Context),
		check(list(byte), Password, Context),
		parse_apr1_options(Options, Salt, Context),
		apr1(Password, Salt, Checksum),
		PasswordHash = apr1(Salt, Checksum).
	password_hash(Hash, Password, PasswordHash, Options) :-
		context(Context),
		check_hash(Hash, Context),
		check(list(byte), Password, Context),
		parse_password_hash_options(Options, Hash, Iterations, Salt, Length, Context),
		pbkdf2(Hash, Password, Salt, Iterations, Length, DerivedKey),
		PasswordHash = pbkdf2(Hash, Iterations, Salt, DerivedKey).

	password_hash_needs_rehash(_PasswordHash, Method, _Options) :-
		\+ ground(Method),
		instantiation_error.
	password_hash_needs_rehash(PasswordHash, pbkdf2(Hash), Options) :-
		!,
		password_hash_needs_rehash(PasswordHash, Hash, Options).
	password_hash_needs_rehash(PasswordHash, bcrypt, Options) :-
		!,
		context(Context),
		parse_bcrypt_policy_options(Options, PolicyCost, Context),
		(	PasswordHash = bcrypt(_, _, _) ->
			check_bcrypt_password_hash(PasswordHash, Cost, _Salt, _Checksum, Context),
			Cost =\= PolicyCost
		;	check_supported_password_hash(PasswordHash, Context)
		).
	password_hash_needs_rehash(PasswordHash, apr1, Options) :-
		!,
		context(Context),
		parse_apr1_policy_options(Options, PolicySaltLength, Context),
		(	PasswordHash = apr1(_, _) ->
			check_apr1_password_hash(PasswordHash, Salt, _Checksum, Context),
			length(Salt, SaltLength),
			SaltLength =\= PolicySaltLength
		;	check_supported_password_hash(PasswordHash, Context)
		).
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
		;	PasswordHash = bcrypt(_, _, _) ->
			check_bcrypt_password_hash(PasswordHash, _Cost, _Salt, _Checksum, Context)
		; 	check_password_hash(PasswordHash, _StoredHash, _Iterations, _Salt, _StoredKey, Context)
		).

	verify_password_hash(PasswordHash, Password) :-
		context(Context),
		check(list(byte), Password, Context),
		(	\+ ground(PasswordHash)	->
			instantiation_error
		;	PasswordHash = pbkdf2(_, _, _, _) ->
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
		;	PasswordHash = bcrypt(_, _, _) ->
			check_bcrypt_password_hash(PasswordHash, Cost, Salt, StoredChecksum, Context),
			bcrypt(Password, Cost, Salt, ComputedChecksum),
			secure_compare(StoredChecksum, ComputedChecksum)
		;	check_password_hash(PasswordHash, _Hash, _Iterations, _Salt, _StoredKey, Context)
		).

	password_hash_atom(PasswordHash, Atom) :-
		context(Context),
		(	var(PasswordHash) ->
			(	var(Atom) ->
				throw(error(instantiation_error, Context))
			;	check(atom, Atom, Context),
				atom_codes(Atom, Codes),
				(	parse_password_hash_codes(Codes, ParsedPasswordHash) ->
					PasswordHash = ParsedPasswordHash
				;	throw(error(domain_error(password_hash, Atom), Context))
				)
			)
		;	password_hash_codes(PasswordHash, Codes, Context),
			atom_codes(CanonicalAtom, Codes),
			(	var(Atom) ->
				Atom = CanonicalAtom
			;	check(atom, Atom, Context),
				Atom == CanonicalAtom
			)
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
			check(positive_integer, Value, Context),
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

	parse_bcrypt_options(Options, Cost, Salt, Context) :-
		check(list(compound), Options, Context),
		parse_bcrypt_options(Options, 12, none, Cost, SaltOption, Context),
		(	SaltOption == none ->
			random_bytes(16, Salt)
		;	Salt = SaltOption
		).

	parse_bcrypt_options([], Cost, Salt, Cost, Salt, _) :-
		!.
	parse_bcrypt_options([Option| Options], Cost0, Salt0, Cost, Salt, Context) :-
		!,
		(	var(Option) ->
			throw(error(instantiation_error, Context))
		;	Option = cost(Value) ->
			check_bcrypt_cost(Value, Context),
			Cost1 = Value,
			Salt1 = Salt0
		;	Option = salt(SaltBytes) ->
			check(list(byte), SaltBytes, Context),
			check_bcrypt_salt(SaltBytes, Context),
			Cost1 = Cost0,
			Salt1 = SaltBytes
		;	throw(error(domain_error(password_hash_option, Option), Context))
		),
		parse_bcrypt_options(Options, Cost1, Salt1, Cost, Salt, Context).
	parse_bcrypt_options(Options, _, _, _, _, Context) :-
		throw(error(type_error(list, Options), Context)).

	parse_bcrypt_policy_options(Options, Cost, Context) :-
		check(list(compound), Options, Context),
		parse_bcrypt_policy_options(Options, 12, Cost, Context).

	parse_bcrypt_policy_options([], Cost, Cost, _) :-
		!.
	parse_bcrypt_policy_options([Option| Options], Cost0, Cost, Context) :-
		!,
		(	var(Option) ->
			throw(error(instantiation_error, Context))
		;	Option = cost(Value) ->
			check_bcrypt_cost(Value, Context),
			Cost1 = Value
		;	Option = salt(SaltBytes) ->
			check(list(byte), SaltBytes, Context),
			check_bcrypt_salt(SaltBytes, Context),
			Cost1 = Cost0
		;	throw(error(domain_error(password_hash_option, Option), Context))
		),
		parse_bcrypt_policy_options(Options, Cost1, Cost, Context).
	parse_bcrypt_policy_options(Options, _, _, Context) :-
		throw(error(type_error(list, Options), Context)).

	parse_apr1_options(Options, Salt, Context) :-
		check(list(compound), Options, Context),
		parse_apr1_options(Options, none, 8, SaltOption, SaltLength, Context),
		(	SaltOption == none ->
			random_apr1_salt(SaltLength, Salt)
		;	Salt = SaltOption
		).

	parse_apr1_options([], Salt, SaltLength, Salt, SaltLength, _) :-
		!.
	parse_apr1_options([Option| Options], Salt0, SaltLength0, Salt, SaltLength, Context) :-
		!,
		(	var(Option) ->
			throw(error(instantiation_error, Context))
		;	Option = salt(SaltBytes) ->
			check(list(byte), SaltBytes, Context),
			check_apr1_salt(SaltBytes, Context),
			Salt1 = SaltBytes,
			SaltLength1 = SaltLength0
		;	Option = salt_length(Value) ->
			check_apr1_salt_length(Value, Context),
			Salt1 = Salt0,
			SaltLength1 = Value
		;	throw(error(domain_error(password_hash_option, Option), Context))
		),
		parse_apr1_options(Options, Salt1, SaltLength1, Salt, SaltLength, Context).
	parse_apr1_options(Options, _, _, _, _, Context) :-
		throw(error(type_error(list, Options), Context)).

	parse_apr1_policy_options(Options, SaltLength, Context) :-
		check(list(compound), Options, Context),
		parse_apr1_policy_options(Options, 8, SaltLength, Context).

	parse_apr1_policy_options([], SaltLength, SaltLength, _) :-
		!.
	parse_apr1_policy_options([Option| Options], _SaltLength0, SaltLength, Context) :-
		!,
		(	var(Option) ->
			throw(error(instantiation_error, Context))
		;	Option = salt_length(Value) ->
			check_apr1_salt_length(Value, Context),
			SaltLength1 = Value
		;	Option = salt(SaltBytes) ->
			check(list(byte), SaltBytes, Context),
			check_apr1_salt(SaltBytes, Context),
			length(SaltBytes, SaltLength1)
		;	throw(error(domain_error(password_hash_option, Option), Context))
		),
		parse_apr1_policy_options(Options, SaltLength1, SaltLength, Context).
	parse_apr1_policy_options(Options, _, _, Context) :-
		throw(error(type_error(list, Options), Context)).

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

	check_bcrypt_password_hash(PasswordHash, Cost, Salt, Checksum, Context) :-
		(	var(PasswordHash) ->
			throw(error(instantiation_error, Context))
		;	PasswordHash = bcrypt(Cost0, Salt0, Checksum0) ->
			check_bcrypt_cost(Cost0, Context),
			check(list(byte), Salt0, Context),
			check_bcrypt_salt(Salt0, Context),
			check(list(byte), Checksum0, Context),
			check_bcrypt_checksum(Checksum0, Context),
			Cost = Cost0,
			Salt = Salt0,
			Checksum = Checksum0
		;	throw(error(domain_error(password_hash, PasswordHash), Context))
		).

	check_supported_password_hash(PasswordHash, Context) :-
		(	PasswordHash = pbkdf2(_, _, _, _) ->
			check_password_hash(PasswordHash, _Hash, _Iterations, _Salt, _StoredKey, Context)
		;	PasswordHash = digest(_, _) ->
			check_digest_password_hash(PasswordHash, _Hash, _Digest, Context)
		;	PasswordHash = apr1(_, _) ->
			check_apr1_password_hash(PasswordHash, _Salt, _Checksum, Context)
		;	PasswordHash = bcrypt(_, _, _) ->
			check_bcrypt_password_hash(PasswordHash, _Cost, _Salt, _Checksum, Context)
		;	throw(error(domain_error(password_hash, PasswordHash), Context))
		).

	password_hash_codes(PasswordHash, Codes, Context) :-
		(	PasswordHash = bcrypt(_, _, _) ->
			check_bcrypt_password_hash(PasswordHash, Cost, Salt, Checksum, Context),
			bcrypt_base64_encode(Salt, EncodedSalt),
			Tens is Cost // 10,
			Ones is Cost mod 10,
			TensCode is 0'0 + Tens,
			OnesCode is 0'0 + Ones,
			append([[0'$,0'2,0'b,0'$,TensCode,OnesCode,0'$], EncodedSalt, Checksum], Codes)
		;	PasswordHash = apr1(_, _) ->
			check_apr1_password_hash(PasswordHash, Salt, Checksum, Context),
			append([[0'$,0'a,0'p,0'r,0'1,0'$], Salt, [0'$], Checksum], Codes)
		;	throw(error(domain_error(password_hash, PasswordHash), Context))
		).

	parse_password_hash_codes([0'$,0'2,0'b,0'$,TensCode,OnesCode,0'$| Codes], bcrypt(Cost, Salt, Checksum)) :-
		!,
		TensCode >= 0'0,
		TensCode =< 0'9,
		OnesCode >= 0'0,
		OnesCode =< 0'9,
		Cost is (TensCode - 0'0) * 10 + OnesCode - 0'0,
		Cost >= 4,
		Cost =< 31,
		bcrypt_split_hash_codes(Codes, EncodedSalt, Checksum),
		bcrypt_base64_codes(EncodedSalt),
		bcrypt_base64_codes(Checksum),
		bcrypt_base64_decode(EncodedSalt, Salt),
		length(Salt, 16),
		bcrypt_base64_encode(Salt, EncodedSalt).
	parse_password_hash_codes([0'$,0'a,0'p,0'r,0'1,0'$| Codes], apr1(Salt, Checksum)) :-
		split_apr1_hash_codes(Codes, Salt, Checksum),
		length(Salt, SaltLength),
		SaltLength >= 1,
		SaltLength =< 8,
		apr1_base64_codes(Salt),
		length(Checksum, 22),
		apr1_base64_codes(Checksum).

	bcrypt_split_hash_codes(Codes, Salt, Checksum) :-
		bcrypt_split_hash_codes(22, Codes, Salt, Checksum),
		length(Checksum, 31).

	bcrypt_split_hash_codes(0, Checksum, [], Checksum) :-
		!.
	bcrypt_split_hash_codes(Count, [Code| Codes], [Code| Salt], Checksum) :-
		NextCount is Count - 1,
		bcrypt_split_hash_codes(NextCount, Codes, Salt, Checksum).

	split_apr1_hash_codes([0'$| Checksum], [], Checksum) :-
		!.
	split_apr1_hash_codes([Code| Codes], [Code| Salt], Checksum) :-
		split_apr1_hash_codes(Codes, Salt, Checksum).

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

	check_apr1_salt_length(Length, Context) :-
		check(integer, Length, Context),
		(	Length >= 1, Length =< 8 ->
			true
		;	throw(error(domain_error(apr1_salt_length, Length), Context))
		).

	random_apr1_salt(0, []) :-
		!.
	random_apr1_salt(Length, [Code| Codes]) :-
		random_below(64, Index),
		apr1_base64_code(Index, Code),
		NextLength is Length - 1,
		random_apr1_salt(NextLength, Codes).

	check_bcrypt_password(Password, Context) :-
		length(Password, Length),
		(	Length =< 72 ->
			true
		;	throw(error(domain_error(bcrypt_password_length, Password), Context))
		).

	check_bcrypt_cost(Cost, Context) :-
		check(integer, Cost, Context),
		(	Cost >= 4, Cost =< 31 ->
			true
		;	throw(error(domain_error(bcrypt_cost, Cost), Context))
		).

	check_bcrypt_salt(Salt, Context) :-
		length(Salt, Length),
		(	Length =:= 16 ->
			true
		;	throw(error(domain_error(bcrypt_salt, Salt), Context))
		).

	check_bcrypt_checksum(Checksum, Context) :-
		length(Checksum, Length),
		(	Length =:= 31, bcrypt_base64_codes(Checksum) ->
			true
		;	throw(error(domain_error(bcrypt_checksum, Checksum), Context))
		).

	bcrypt_base64_codes([]).
	bcrypt_base64_codes([Code| Codes]) :-
		bcrypt_base64_code(Code),
		bcrypt_base64_codes(Codes).

	bcrypt_base64_code(Code) :-
		(	Code =:= 0'.
		;	Code =:= 0'/
		;	0'A =< Code, Code =< 0'Z
		;	0'a =< Code, Code =< 0'z
		;	0'0 =< Code, Code =< 0'9
		),
		!.

	bcrypt_base64_decode(Codes, Bytes) :-
		bcrypt_base64_decode(Codes, 0, 0, Bytes).

	bcrypt_base64_decode([], _Buffer, _Bits, []).
	bcrypt_base64_decode([Code| Codes], Buffer0, Bits0, Bytes) :-
		bcrypt_base64_value(Code, Value),
		Buffer1 is (Buffer0 << 6) \/ Value,
		Bits1 is Bits0 + 6,
		bcrypt_base64_decode_bytes(Bits1, Buffer1, Codes, Bytes).

	bcrypt_base64_decode_bytes(Bits, Buffer, Codes, [Byte| Bytes]) :-
		Bits >= 8,
		!,
		RemainingBits is Bits - 8,
		Byte is (Buffer >> RemainingBits) /\ 0xFF,
		Mask is (1 << RemainingBits) - 1,
		RemainingBuffer is Buffer /\ Mask,
		bcrypt_base64_decode_bytes(RemainingBits, RemainingBuffer, Codes, Bytes).
	bcrypt_base64_decode_bytes(Bits, Buffer, Codes, Bytes) :-
		bcrypt_base64_decode(Codes, Buffer, Bits, Bytes).

	bcrypt_base64_value(Code, Value) :-
		(	Code =:= 0'. -> Value = 0
		;	Code =:= 0'/ -> Value = 1
		;	Code >= 0'A, Code =< 0'Z -> Value is Code - 0'A + 2
		;	Code >= 0'a, Code =< 0'z -> Value is Code - 0'a + 28
		;	Value is Code - 0'0 + 54
		),
		!.

	bcrypt_initial_state(state(P, S0, S1, S2, S3)) :-
		bcrypt_initial_p(P),
		bcrypt_initial_s0(S0),
		bcrypt_initial_s1(S1),
		bcrypt_initial_s2(S2),
		bcrypt_initial_s3(S3).

	bcrypt_expand_key(Key, Salt, state(P0, S00, S10, S20, S30), State) :-
		bcrypt_xor_key(P0, Key, 0, P1, _),
		State0 = state(P1, S00, S10, S20, S30),
		bcrypt_replace_pairs(p, 0, 18, Salt, 0, 0, 0, State0, State1, Position1, Left1, Right1),
		bcrypt_replace_pairs(s0, 0, 256, Salt, Position1, Left1, Right1, State1, State2, Position2, Left2, Right2),
		bcrypt_replace_pairs(s1, 0, 256, Salt, Position2, Left2, Right2, State2, State3, Position3, Left3, Right3),
		bcrypt_replace_pairs(s2, 0, 256, Salt, Position3, Left3, Right3, State3, State4, Position4, Left4, Right4),
		bcrypt_replace_pairs(s3, 0, 256, Salt, Position4, Left4, Right4, State4, State, _, _, _).

	bcrypt_expand_key(Key, State0, State) :-
		State0 = state(P0, _, _, _, _),
		bcrypt_xor_key(P0, Key, 0, P, _),
		bcrypt_set_p(State0, P, State1),
		bcrypt_replace_pairs(p, 0, 18, [], 0, 0, 0, State1, State2, _, Left1, Right1),
		bcrypt_replace_pairs(s0, 0, 256, [], 0, Left1, Right1, State2, State3, _, Left2, Right2),
		bcrypt_replace_pairs(s1, 0, 256, [], 0, Left2, Right2, State3, State4, _, Left3, Right3),
		bcrypt_replace_pairs(s2, 0, 256, [], 0, Left3, Right3, State4, State5, _, Left4, Right4),
		bcrypt_replace_pairs(s3, 0, 256, [], 0, Left4, Right4, State5, State, _, _, _).

	bcrypt_cost_rounds(0, _Key, _Salt, State, State) :-
		!.
	bcrypt_cost_rounds(Rounds, Key, Salt, State0, State) :-
		bcrypt_expand_key(Key, State0, State1),
		bcrypt_expand_key(Salt, State1, State2),
		NextRounds is Rounds - 1,
		bcrypt_cost_rounds(NextRounds, Key, Salt, State2, State).

	bcrypt_xor_key([], _Key, Position, [], Position).
	bcrypt_xor_key([Word0| Words0], Key, Position0, [Word| Words], Position) :-
		bcrypt_next_word(Key, Position0, KeyWord, Position1),
		Word is xor(Word0, KeyWord) /\ 0xFFFFFFFF,
		bcrypt_xor_key(Words0, Key, Position1, Words, Position).

	bcrypt_next_word(Bytes, Position0, Word, Position) :-
		length(Bytes, Length),
		bcrypt_next_word(4, Bytes, Length, Position0, 0, Word, Position).

	bcrypt_next_word(0, _Bytes, _Length, Position, Word, Word, Position) :-
		!.
	bcrypt_next_word(Count, Bytes, Length, Position0, Word0, Word, Position) :-
		nth0(Position0, Bytes, Byte),
		Word1 is ((Word0 << 8) \/ Byte) /\ 0xFFFFFFFF,
		Position1 is (Position0 + 1) mod Length,
		NextCount is Count - 1,
		bcrypt_next_word(NextCount, Bytes, Length, Position1, Word1, Word, Position).

	bcrypt_replace_pairs(_Table, Index, Length, _Salt, Position, Left, Right, State, State, Position, Left, Right) :-
		Index >= Length,
		!.
	bcrypt_replace_pairs(Table, Index, Length, Salt, Position0, Left0, Right0, State0, State, Position, FinalLeft, FinalRight) :-
		bcrypt_salt_words(Salt, Position0, Left0, Right0, Left1, Right1, Position1),
		bcrypt_encrypt_block(State0, Left1, Right1, Left, Right),
		bcrypt_set_pair(Table, Index, Left, Right, State0, State1),
		NextIndex is Index + 2,
		bcrypt_replace_pairs(Table, NextIndex, Length, Salt, Position1, Left, Right, State1, State, Position, FinalLeft, FinalRight).

	bcrypt_salt_words([], Position, Left, Right, Left, Right, Position) :-
		!.
	bcrypt_salt_words(Salt, Position0, Left0, Right0, Left, Right, Position) :-
		bcrypt_next_word(Salt, Position0, SaltLeft, Position1),
		bcrypt_next_word(Salt, Position1, SaltRight, Position),
		Left is xor(Left0, SaltLeft) /\ 0xFFFFFFFF,
		Right is xor(Right0, SaltRight) /\ 0xFFFFFFFF.

	bcrypt_encrypt_block(state(P, S0, S1, S2, S3), Left0, Right0, Left, Right) :-
		P = [P0| Ps],
		Left1 is xor(Left0, P0) /\ 0xFFFFFFFF,
		bcrypt_encrypt_rounds(Ps, S0, S1, S2, S3, Left1, Right0, Left, Right).

	bcrypt_encrypt_rounds([P17], _S0, _S1, _S2, _S3, Left, Right0, Right, Left) :-
		!,
		Right is xor(Right0, P17) /\ 0xFFFFFFFF.
	bcrypt_encrypt_rounds([P| Ps], S0, S1, S2, S3, Left0, Right0, Left, Right) :-
		bcrypt_f(Left0, S0, S1, S2, S3, F),
		Right1 is xor(xor(Right0, F), P) /\ 0xFFFFFFFF,
		bcrypt_encrypt_rounds(Ps, S0, S1, S2, S3, Right1, Left0, Left, Right).

	bcrypt_f(Word, S0, S1, S2, S3, F) :-
		Index0 is (Word >> 24) /\ 0xFF,
		Index1 is (Word >> 16) /\ 0xFF,
		Index2 is (Word >> 8) /\ 0xFF,
		Index3 is Word /\ 0xFF,
		bcrypt_sbox_word(S0, Index0, A),
		bcrypt_sbox_word(S1, Index1, B),
		bcrypt_sbox_word(S2, Index2, C),
		bcrypt_sbox_word(S3, Index3, D),
		Sum1 is (A + B) /\ 0xFFFFFFFF,
		Sum2 is (xor(Sum1, C) + D) /\ 0xFFFFFFFF,
		F = Sum2.

	bcrypt_sbox_word(Rows, Index, Word) :-
		RowIndex is Index >> 4,
		ColumnIndex is Index /\ 15,
		nth0(RowIndex, Rows, Row),
		nth0(ColumnIndex, Row, Word).

	bcrypt_set_pair(p, Index, Left, Right, state(P0, S0, S1, S2, S3), state(P, S0, S1, S2, S3)) :-
		bcrypt_replace_pair(Index, Left, Right, P0, P).
	bcrypt_set_pair(s0, Index, Left, Right, state(P, S00, S1, S2, S3), state(P, S0, S1, S2, S3)) :-
		bcrypt_replace_sbox_pair(Index, Left, Right, S00, S0).
	bcrypt_set_pair(s1, Index, Left, Right, state(P, S0, S10, S2, S3), state(P, S0, S1, S2, S3)) :-
		bcrypt_replace_sbox_pair(Index, Left, Right, S10, S1).
	bcrypt_set_pair(s2, Index, Left, Right, state(P, S0, S1, S20, S3), state(P, S0, S1, S2, S3)) :-
		bcrypt_replace_sbox_pair(Index, Left, Right, S20, S2).
	bcrypt_set_pair(s3, Index, Left, Right, state(P, S0, S1, S2, S30), state(P, S0, S1, S2, S3)) :-
		bcrypt_replace_sbox_pair(Index, Left, Right, S30, S3).

	bcrypt_set_p(state(_, S0, S1, S2, S3), P, state(P, S0, S1, S2, S3)).

	bcrypt_replace_sbox_pair(Index, Left, Right, Rows0, Rows) :-
		RowIndex is Index >> 4,
		ColumnIndex is Index /\ 15,
		nth0(RowIndex, Rows0, Row0),
		bcrypt_replace_pair(ColumnIndex, Left, Right, Row0, Row),
		bcrypt_replace_at(RowIndex, Row, Rows0, Rows).

	bcrypt_replace_pair(0, Left, Right, [_OldLeft, _OldRight| Values], [Left, Right| Values]) :-
		!.
	bcrypt_replace_pair(Index, Left, Right, [Value| Values0], [Value| Values]) :-
		NextIndex is Index - 1,
		bcrypt_replace_pair(NextIndex, Left, Right, Values0, Values).

	bcrypt_replace_at(0, Value, [_| Values], [Value| Values]) :-
		!.
	bcrypt_replace_at(Index, Value, [Head| Values0], [Head| Values]) :-
		NextIndex is Index - 1,
		bcrypt_replace_at(NextIndex, Value, Values0, Values).

	bcrypt_ciphertext([
		0x4f,0x72,0x70,0x68,0x65,0x61,0x6e,0x42,
		0x65,0x68,0x6f,0x6c,0x64,0x65,0x72,0x53,
		0x63,0x72,0x79,0x44,0x6f,0x75,0x62,0x74
	]).

	bcrypt_encrypt_ciphertext([], _State, []).
	bcrypt_encrypt_ciphertext([B0,B1,B2,B3,B4,B5,B6,B7| Bytes], State, Ciphertext) :-
		Left is (B0 << 24) \/ (B1 << 16) \/ (B2 << 8) \/ B3,
		Right is (B4 << 24) \/ (B5 << 16) \/ (B6 << 8) \/ B7,
		bcrypt_encrypt_block_rounds(64, State, Left, Right, EncryptedLeft, EncryptedRight),
		Ciphertext = [C0,C1,C2,C3,C4,C5,C6,C7| EncryptedBytes],
		C0 is (EncryptedLeft >> 24) /\ 0xFF,
		C1 is (EncryptedLeft >> 16) /\ 0xFF,
		C2 is (EncryptedLeft >> 8) /\ 0xFF,
		C3 is EncryptedLeft /\ 0xFF,
		C4 is (EncryptedRight >> 24) /\ 0xFF,
		C5 is (EncryptedRight >> 16) /\ 0xFF,
		C6 is (EncryptedRight >> 8) /\ 0xFF,
		C7 is EncryptedRight /\ 0xFF,
		bcrypt_encrypt_ciphertext(Bytes, State, EncryptedBytes).

	bcrypt_encrypt_block_rounds(0, _State, Left, Right, Left, Right) :-
		!.
	bcrypt_encrypt_block_rounds(Rounds, State, Left0, Right0, Left, Right) :-
		bcrypt_encrypt_block(State, Left0, Right0, Left1, Right1),
		NextRounds is Rounds - 1,
		bcrypt_encrypt_block_rounds(NextRounds, State, Left1, Right1, Left, Right).

	bcrypt_first_bytes(0, _Bytes, []) :-
		!.
	bcrypt_first_bytes(Count, [Byte| Bytes], [Byte| Prefix]) :-
		NextCount is Count - 1,
		bcrypt_first_bytes(NextCount, Bytes, Prefix).

	bcrypt_base64_encode([], []).
	bcrypt_base64_encode([Byte0], [Code0, Code1]) :-
		!,
		Index0 is Byte0 >> 2,
		Index1 is (Byte0 /\ 3) << 4,
		bcrypt_base64_code(Index0, Code0),
		bcrypt_base64_code(Index1, Code1).
	bcrypt_base64_encode([Byte0, Byte1], [Code0, Code1, Code2]) :-
		!,
		Index0 is Byte0 >> 2,
		Index1 is ((Byte0 /\ 3) << 4) \/ (Byte1 >> 4),
		Index2 is (Byte1 /\ 15) << 2,
		bcrypt_base64_code(Index0, Code0),
		bcrypt_base64_code(Index1, Code1),
		bcrypt_base64_code(Index2, Code2).
	bcrypt_base64_encode([Byte0, Byte1, Byte2| Bytes], [Code0, Code1, Code2, Code3| Codes]) :-
		Index0 is Byte0 >> 2,
		Index1 is ((Byte0 /\ 3) << 4) \/ (Byte1 >> 4),
		Index2 is ((Byte1 /\ 15) << 2) \/ (Byte2 >> 6),
		Index3 is Byte2 /\ 63,
		bcrypt_base64_code(Index0, Code0),
		bcrypt_base64_code(Index1, Code1),
		bcrypt_base64_code(Index2, Code2),
		bcrypt_base64_code(Index3, Code3),
		bcrypt_base64_encode(Bytes, Codes).

	bcrypt_base64_code(Index, Code) :-
		(	Index =:= 0 -> Code is 0'.
		;	Index =:= 1 -> Code is 0'/
		;	Index =< 27 -> Code is 0'A + Index - 2
		;	Index =< 53 -> Code is 0'a + Index - 28
		;	Code is 0'0 + Index - 54
		),
		!.

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

	pbkdf2_blocks(0, _Hash, _Password, _Salt, _Iterations, _Index, DerivedKey, DerivedKey) :-
		!.
	pbkdf2_blocks(Remaining0, Hash, Password, Salt, Iterations, Index, DerivedKey0, DerivedKey) :-
		pbkdf2_block(Hash, Password, Salt, Iterations, Index, Block),
		copy_output_bytes(Block, Remaining0, Remaining, DerivedKey0, DerivedKey1),
		NextIndex is Index + 1,
		pbkdf2_blocks(Remaining, Hash, Password, Salt, Iterations, NextIndex, DerivedKey1, DerivedKey).

	pbkdf2_block(Hash, Password, Salt, Iterations, Index, Block) :-
		integer_to_big_endian_bytes32(Index, CounterBytes),
		append(Salt, CounterBytes, Message),
		digest(Hash, Password, Message, U1),
		pbkdf2_iterate(Iterations, Hash, Password, U1, U1, Block).

	pbkdf2_iterate(1, _Hash, _Password, _Current, Accumulator, Accumulator) :-
		!.
	pbkdf2_iterate(Iterations, Hash, Password, Current, Accumulator0, Accumulator) :-
		digest(Hash, Password, Current, Next),
		xor_bytes(Accumulator0, Next, Accumulator1),
		NextIterations is Iterations - 1,
		pbkdf2_iterate(NextIterations, Hash, Password, Next, Accumulator1, Accumulator).

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

	bcrypt_initial_s0([
		[0xd1310ba6,0x98dfb5ac,0x2ffd72db,0xd01adfb7,0xb8e1afed,0x6a267e96,0xba7c9045,0xf12c7f99,0x24a19947,0xb3916cf7,0x0801f2e2,0x858efc16,0x636920d8,0x71574e69,0xa458fea3,0xf4933d7e],
		[0x0d95748f,0x728eb658,0x718bcd58,0x82154aee,0x7b54a41d,0xc25a59b5,0x9c30d539,0x2af26013,0xc5d1b023,0x286085f0,0xca417918,0xb8db38ef,0x8e79dcb0,0x603a180e,0x6c9e0e8b,0xb01e8a3e],
		[0xd71577c1,0xbd314b27,0x78af2fda,0x55605c60,0xe65525f3,0xaa55ab94,0x57489862,0x63e81440,0x55ca396a,0x2aab10b6,0xb4cc5c34,0x1141e8ce,0xa15486af,0x7c72e993,0xb3ee1411,0x636fbc2a],
		[0x2ba9c55d,0x741831f6,0xce5c3e16,0x9b87931e,0xafd6ba33,0x6c24cf5c,0x7a325381,0x28958677,0x3b8f4898,0x6b4bb9af,0xc4bfe81b,0x66282193,0x61d809cc,0xfb21a991,0x487cac60,0x5dec8032],
		[0xef845d5d,0xe98575b1,0xdc262302,0xeb651b88,0x23893e81,0xd396acc5,0x0f6d6ff3,0x83f44239,0x2e0b4482,0xa4842004,0x69c8f04a,0x9e1f9b5e,0x21c66842,0xf6e96c9a,0x670c9c61,0xabd388f0],
		[0x6a51a0d2,0xd8542f68,0x960fa728,0xab5133a3,0x6eef0b6c,0x137a3be4,0xba3bf050,0x7efb2a98,0xa1f1651d,0x39af0176,0x66ca593e,0x82430e88,0x8cee8619,0x456f9fb4,0x7d84a5c3,0x3b8b5ebe],
		[0xe06f75d8,0x85c12073,0x401a449f,0x56c16aa6,0x4ed3aa62,0x363f7706,0x1bfedf72,0x429b023d,0x37d0d724,0xd00a1248,0xdb0fead3,0x49f1c09b,0x075372c9,0x80991b7b,0x25d479d8,0xf6e8def7],
		[0xe3fe501a,0xb6794c3b,0x976ce0bd,0x04c006ba,0xc1a94fb6,0x409f60c4,0x5e5c9ec2,0x196a2463,0x68fb6faf,0x3e6c53b5,0x1339b2eb,0x3b52ec6f,0x6dfc511f,0x9b30952c,0xcc814544,0xaf5ebd09],
		[0xbee3d004,0xde334afd,0x660f2807,0x192e4bb3,0xc0cba857,0x45c8740f,0xd20b5f39,0xb9d3fbdb,0x5579c0bd,0x1a60320a,0xd6a100c6,0x402c7279,0x679f25fe,0xfb1fa3cc,0x8ea5e9f8,0xdb3222f8],
		[0x3c7516df,0xfd616b15,0x2f501ec8,0xad0552ab,0x323db5fa,0xfd238760,0x53317b48,0x3e00df82,0x9e5c57bb,0xca6f8ca0,0x1a87562e,0xdf1769db,0xd542a8f6,0x287effc3,0xac6732c6,0x8c4f5573],
		[0x695b27b0,0xbbca58c8,0xe1ffa35d,0xb8f011a0,0x10fa3d98,0xfd2183b8,0x4afcb56c,0x2dd1d35b,0x9a53e479,0xb6f84565,0xd28e49bc,0x4bfb9790,0xe1ddf2da,0xa4cb7e33,0x62fb1341,0xcee4c6e8],
		[0xef20cada,0x36774c01,0xd07e9efe,0x2bf11fb4,0x95dbda4d,0xae909198,0xeaad8e71,0x6b93d5a0,0xd08ed1d0,0xafc725e0,0x8e3c5b2f,0x8e7594b7,0x8ff6e2fb,0xf2122b64,0x8888b812,0x900df01c],
		[0x4fad5ea0,0x688fc31c,0xd1cff191,0xb3a8c1ad,0x2f2f2218,0xbe0e1777,0xea752dfe,0x8b021fa1,0xe5a0cc0f,0xb56f74e8,0x18acf3d6,0xce89e299,0xb4a84fe0,0xfd13e0b7,0x7cc43b81,0xd2ada8d9],
		[0x165fa266,0x80957705,0x93cc7314,0x211a1477,0xe6ad2065,0x77b5fa86,0xc75442f5,0xfb9d35cf,0xebcdaf0c,0x7b3e89a0,0xd6411bd3,0xae1e7e49,0x00250e2d,0x2071b35e,0x226800bb,0x57b8e0af],
		[0x2464369b,0xf009b91e,0x5563911d,0x59dfa6aa,0x78c14389,0xd95a537f,0x207d5ba2,0x02e5b9c5,0x83260376,0x6295cfa9,0x11c81968,0x4e734a41,0xb3472dca,0x7b14a94a,0x1b510052,0x9a532915],
		[0xd60f573f,0xbc9bc6e4,0x2b60a476,0x81e67400,0x08ba6fb5,0x571be91f,0xf296ec6b,0x2a0dd915,0xb6636521,0xe7b9f9b6,0xff34052e,0xc5855664,0x53b02d5d,0xa99f8fa1,0x08ba4799,0x6e85076a]
	]).

	bcrypt_initial_s1([
		[0x4b7a70e9,0xb5b32944,0xdb75092e,0xc4192623,0xad6ea6b0,0x49a7df7d,0x9cee60b8,0x8fedb266,0xecaa8c71,0x699a17ff,0x5664526c,0xc2b19ee1,0x193602a5,0x75094c29,0xa0591340,0xe4183a3e],
		[0x3f54989a,0x5b429d65,0x6b8fe4d6,0x99f73fd6,0xa1d29c07,0xefe830f5,0x4d2d38e6,0xf0255dc1,0x4cdd2086,0x8470eb26,0x6382e9c6,0x021ecc5e,0x09686b3f,0x3ebaefc9,0x3c971814,0x6b6a70a1],
		[0x687f3584,0x52a0e286,0xb79c5305,0xaa500737,0x3e07841c,0x7fdeae5c,0x8e7d44ec,0x5716f2b8,0xb03ada37,0xf0500c0d,0xf01c1f04,0x0200b3ff,0xae0cf51a,0x3cb574b2,0x25837a58,0xdc0921bd],
		[0xd19113f9,0x7ca92ff6,0x94324773,0x22f54701,0x3ae5e581,0x37c2dadc,0xc8b57634,0x9af3dda7,0xa9446146,0x0fd0030e,0xecc8c73e,0xa4751e41,0xe238cd99,0x3bea0e2f,0x3280bba1,0x183eb331],
		[0x4e548b38,0x4f6db908,0x6f420d03,0xf60a04bf,0x2cb81290,0x24977c79,0x5679b072,0xbcaf89af,0xde9a771f,0xd9930810,0xb38bae12,0xdccf3f2e,0x5512721f,0x2e6b7124,0x501adde6,0x9f84cd87],
		[0x7a584718,0x7408da17,0xbc9f9abc,0xe94b7d8c,0xec7aec3a,0xdb851dfa,0x63094366,0xc464c3d2,0xef1c1847,0x3215d908,0xdd433b37,0x24c2ba16,0x12a14d43,0x2a65c451,0x50940002,0x133ae4dd],
		[0x71dff89e,0x10314e55,0x81ac77d6,0x5f11199b,0x043556f1,0xd7a3c76b,0x3c11183b,0x5924a509,0xf28fe6ed,0x97f1fbfa,0x9ebabf2c,0x1e153c6e,0x86e34570,0xeae96fb1,0x860e5e0a,0x5a3e2ab3],
		[0x771fe71c,0x4e3d06fa,0x2965dcb9,0x99e71d0f,0x803e89d6,0x5266c825,0x2e4cc978,0x9c10b36a,0xc6150eba,0x94e2ea78,0xa5fc3c53,0x1e0a2df4,0xf2f74ea7,0x361d2b3d,0x1939260f,0x19c27960],
		[0x5223a708,0xf71312b6,0xebadfe6e,0xeac31f66,0xe3bc4595,0xa67bc883,0xb17f37d1,0x018cff28,0xc332ddef,0xbe6c5aa5,0x65582185,0x68ab9802,0xeecea50f,0xdb2f953b,0x2aef7dad,0x5b6e2f84],
		[0x1521b628,0x29076170,0xecdd4775,0x619f1510,0x13cca830,0xeb61bd96,0x0334fe1e,0xaa0363cf,0xb5735c90,0x4c70a239,0xd59e9e0b,0xcbaade14,0xeecc86bc,0x60622ca7,0x9cab5cab,0xb2f3846e],
		[0x648b1eaf,0x19bdf0ca,0xa02369b9,0x655abb50,0x40685a32,0x3c2ab4b3,0x319ee9d5,0xc021b8f7,0x9b540b19,0x875fa099,0x95f7997e,0x623d7da8,0xf837889a,0x97e32d77,0x11ed935f,0x16681281],
		[0x0e358829,0xc7e61fd6,0x96dedfa1,0x7858ba99,0x57f584a5,0x1b227263,0x9b83c3ff,0x1ac24696,0xcdb30aeb,0x532e3054,0x8fd948e4,0x6dbc3128,0x58ebf2ef,0x34c6ffea,0xfe28ed61,0xee7c3c73],
		[0x5d4a14d9,0xe864b7e3,0x42105d14,0x203e13e0,0x45eee2b6,0xa3aaabea,0xdb6c4f15,0xfacb4fd0,0xc742f442,0xef6abbb5,0x654f3b1d,0x41cd2105,0xd81e799e,0x86854dc7,0xe44b476a,0x3d816250],
		[0xcf62a1f2,0x5b8d2646,0xfc8883a0,0xc1c7b6a3,0x7f1524c3,0x69cb7492,0x47848a0b,0x5692b285,0x095bbf00,0xad19489d,0x1462b174,0x23820e00,0x58428d2a,0x0c55f5ea,0x1dadf43e,0x233f7061],
		[0x3372f092,0x8d937e41,0xd65fecf1,0x6c223bdb,0x7cde3759,0xcbee7460,0x4085f2a7,0xce77326e,0xa6078084,0x19f8509e,0xe8efd855,0x61d99735,0xa969a7aa,0xc50c06c2,0x5a04abfc,0x800bcadc],
		[0x9e447a2e,0xc3453484,0xfdd56705,0x0e1e9ec9,0xdb73dbd3,0x105588cd,0x675fda79,0xe3674340,0xc5c43465,0x713e38d8,0x3d28f89e,0xf16dff20,0x153e21e7,0x8fb03d4a,0xe6e39f2b,0xdb83adf7]
	]).

	bcrypt_initial_s2([
		[0xe93d5a68,0x948140f7,0xf64c261c,0x94692934,0x411520f7,0x7602d4f7,0xbcf46b2e,0xd4a20068,0xd4082471,0x3320f46a,0x43b7d4b7,0x500061af,0x1e39f62e,0x97244546,0x14214f74,0xbf8b8840],
		[0x4d95fc1d,0x96b591af,0x70f4ddd3,0x66a02f45,0xbfbc09ec,0x03bd9785,0x7fac6dd0,0x31cb8504,0x96eb27b3,0x55fd3941,0xda2547e6,0xabca0a9a,0x28507825,0x530429f4,0x0a2c86da,0xe9b66dfb],
		[0x68dc1462,0xd7486900,0x680ec0a4,0x27a18dee,0x4f3ffea2,0xe887ad8c,0xb58ce006,0x7af4d6b6,0xaace1e7c,0xd3375fec,0xce78a399,0x406b2a42,0x20fe9e35,0xd9f385b9,0xee39d7ab,0x3b124e8b],
		[0x1dc9faf7,0x4b6d1856,0x26a36631,0xeae397b2,0x3a6efa74,0xdd5b4332,0x6841e7f7,0xca7820fb,0xfb0af54e,0xd8feb397,0x454056ac,0xba489527,0x55533a3a,0x20838d87,0xfe6ba9b7,0xd096954b],
		[0x55a867bc,0xa1159a58,0xcca92963,0x99e1db33,0xa62a4a56,0x3f3125f9,0x5ef47e1c,0x9029317c,0xfdf8e802,0x04272f70,0x80bb155c,0x05282ce3,0x95c11548,0xe4c66d22,0x48c1133f,0xc70f86dc],
		[0x07f9c9ee,0x41041f0f,0x404779a4,0x5d886e17,0x325f51eb,0xd59bc0d1,0xf2bcc18f,0x41113564,0x257b7834,0x602a9c60,0xdff8e8a3,0x1f636c1b,0x0e12b4c2,0x02e1329e,0xaf664fd1,0xcad18115],
		[0x6b2395e0,0x333e92e1,0x3b240b62,0xeebeb922,0x85b2a20e,0xe6ba0d99,0xde720c8c,0x2da2f728,0xd0127845,0x95b794fd,0x647d0862,0xe7ccf5f0,0x5449a36f,0x877d48fa,0xc39dfd27,0xf33e8d1e],
		[0x0a476341,0x992eff74,0x3a6f6eab,0xf4f8fd37,0xa812dc60,0xa1ebddf8,0x991be14c,0xdb6e6b0d,0xc67b5510,0x6d672c37,0x2765d43b,0xdcd0e804,0xf1290dc7,0xcc00ffa3,0xb5390f92,0x690fed0b],
		[0x667b9ffb,0xcedb7d9c,0xa091cf0b,0xd9155ea3,0xbb132f88,0x515bad24,0x7b9479bf,0x763bd6eb,0x37392eb3,0xcc115979,0x8026e297,0xf42e312d,0x6842ada7,0xc66a2b3b,0x12754ccc,0x782ef11c],
		[0x6a124237,0xb79251e7,0x06a1bbe6,0x4bfb6350,0x1a6b1018,0x11caedfa,0x3d25bdd8,0xe2e1c3c9,0x44421659,0x0a121386,0xd90cec6e,0xd5abea2a,0x64af674e,0xda86a85f,0xbebfe988,0x64e4c3fe],
		[0x9dbc8057,0xf0f7c086,0x60787bf8,0x6003604d,0xd1fd8346,0xf6381fb0,0x7745ae04,0xd736fccc,0x83426b33,0xf01eab71,0xb0804187,0x3c005e5f,0x77a057be,0xbde8ae24,0x55464299,0xbf582e61],
		[0x4e58f48f,0xf2ddfda2,0xf474ef38,0x8789bdc2,0x5366f9c3,0xc8b38e74,0xb475f255,0x46fcd9b9,0x7aeb2661,0x8b1ddf84,0x846a0e79,0x915f95e2,0x466e598e,0x20b45770,0x8cd55591,0xc902de4c],
		[0xb90bace1,0xbb8205d0,0x11a86248,0x7574a99e,0xb77f19b6,0xe0a9dc09,0x662d09a1,0xc4324633,0xe85a1f02,0x09f0be8c,0x4a99a025,0x1d6efe10,0x1ab93d1d,0x0ba5a4df,0xa186f20f,0x2868f169],
		[0xdcb7da83,0x573906fe,0xa1e2ce9b,0x4fcd7f52,0x50115e01,0xa70683fa,0xa002b5c4,0x0de6d027,0x9af88c27,0x773f8641,0xc3604c06,0x61a806b5,0xf0177a28,0xc0f586e0,0x006058aa,0x30dc7d62],
		[0x11e69ed7,0x2338ea63,0x53c2dd94,0xc2c21634,0xbbcbee56,0x90bcb6de,0xebfc7da1,0xce591d76,0x6f05e409,0x4b7c0188,0x39720a3d,0x7c927c24,0x86e3725f,0x724d9db9,0x1ac15bb4,0xd39eb8fc],
		[0xed545578,0x08fca5b5,0xd83d7cd3,0x4dad0fc4,0x1e50ef5e,0xb161e6f8,0xa28514d9,0x6c51133c,0x6fd5c7e7,0x56e14ec4,0x362abfce,0xddc6c837,0xd79a3234,0x92638212,0x670efa8e,0x406000e0]
	]).

	bcrypt_initial_s3([
		[0x3a39ce37,0xd3faf5cf,0xabc27737,0x5ac52d1b,0x5cb0679e,0x4fa33742,0xd3822740,0x99bc9bbe,0xd5118e9d,0xbf0f7315,0xd62d1c7e,0xc700c47b,0xb78c1b6b,0x21a19045,0xb26eb1be,0x6a366eb4],
		[0x5748ab2f,0xbc946e79,0xc6a376d2,0x6549c2c8,0x530ff8ee,0x468dde7d,0xd5730a1d,0x4cd04dc6,0x2939bbdb,0xa9ba4650,0xac9526e8,0xbe5ee304,0xa1fad5f0,0x6a2d519a,0x63ef8ce2,0x9a86ee22],
		[0xc089c2b8,0x43242ef6,0xa51e03aa,0x9cf2d0a4,0x83c061ba,0x9be96a4d,0x8fe51550,0xba645bd6,0x2826a2f9,0xa73a3ae1,0x4ba99586,0xef5562e9,0xc72fefd3,0xf752f7da,0x3f046f69,0x77fa0a59],
		[0x80e4a915,0x87b08601,0x9b09e6ad,0x3b3ee593,0xe990fd5a,0x9e34d797,0x2cf0b7d9,0x022b8b51,0x96d5ac3a,0x017da67d,0xd1cf3ed6,0x7c7d2d28,0x1f9f25cf,0xadf2b89b,0x5ad6b472,0x5a88f54c],
		[0xe029ac71,0xe019a5e6,0x47b0acfd,0xed93fa9b,0xe8d3c48d,0x283b57cc,0xf8d56629,0x79132e28,0x785f0191,0xed756055,0xf7960e44,0xe3d35e8c,0x15056dd4,0x88f46dba,0x03a16125,0x0564f0bd],
		[0xc3eb9e15,0x3c9057a2,0x97271aec,0xa93a072a,0x1b3f6d9b,0x1e6321f5,0xf59c66fb,0x26dcf319,0x7533d928,0xb155fdf5,0x03563482,0x8aba3cbb,0x28517711,0xc20ad9f8,0xabcc5167,0xccad925f],
		[0x4de81751,0x3830dc8e,0x379d5862,0x9320f991,0xea7a90c2,0xfb3e7bce,0x5121ce64,0x774fbe32,0xa8b6e37e,0xc3293d46,0x48de5369,0x6413e680,0xa2ae0810,0xdd6db224,0x69852dfd,0x09072166],
		[0xb39a460a,0x6445c0dd,0x586cdecf,0x1c20c8ae,0x5bbef7dd,0x1b588d40,0xccd2017f,0x6bb4e3bb,0xdda26a7e,0x3a59ff45,0x3e350a44,0xbcb4cdd5,0x72eacea8,0xfa6484bb,0x8d6612ae,0xbf3c6f47],
		[0xd29be463,0x542f5d9e,0xaec2771b,0xf64e6370,0x740e0d8d,0xe75b1357,0xf8721671,0xaf537d5d,0x4040cb08,0x4eb4e2cc,0x34d2466a,0x0115af84,0xe1b00428,0x95983a1d,0x06b89fb4,0xce6ea048],
		[0x6f3f3b82,0x3520ab82,0x011a1d4b,0x277227f8,0x611560b1,0xe7933fdc,0xbb3a792b,0x344525bd,0xa08839e1,0x51ce794b,0x2f32c9b7,0xa01fbac9,0xe01cc87e,0xbcc7d1f6,0xcf0111c3,0xa1e8aac7],
		[0x1a908749,0xd44fbd9a,0xd0dadecb,0xd50ada38,0x0339c32a,0xc6913667,0x8df9317c,0xe0b12b4f,0xf79e59b7,0x43f5bb3a,0xf2d519ff,0x27d9459c,0xbf97222c,0x15e6fc2a,0x0f91fc71,0x9b941525],
		[0xfae59361,0xceb69ceb,0xc2a86459,0x12baa8d1,0xb6c1075e,0xe3056a0c,0x10d25065,0xcb03a442,0xe0ec6e0e,0x1698db3b,0x4c98a0be,0x3278e964,0x9f1f9532,0xe0d392df,0xd3a0342b,0x8971f21e],
		[0x1b0a7441,0x4ba3348c,0xc5be7120,0xc37632d8,0xdf359f8d,0x9b992f2e,0xe60b6f47,0x0fe3f11d,0xe54cda54,0x1edad891,0xce6279cf,0xcd3e7e6f,0x1618b166,0xfd2c1d05,0x848fd2c5,0xf6fb2299],
		[0xf523f357,0xa6327623,0x93a83531,0x56cccd02,0xacf08162,0x5a75ebb5,0x6e163697,0x88d273cc,0xde966292,0x81b949d0,0x4c50901b,0x71c65614,0xe6c6c7bd,0x327a140a,0x45e1d006,0xc3f27b9a],
		[0xc9aa53fd,0x62a80f00,0xbb25bfe2,0x35bdd2f6,0x71126905,0xb2040222,0xb6cbcf7c,0xcd769c2b,0x53113ec0,0x1640e3d3,0x38abbd60,0x2547adf0,0xba38209c,0xf746ce76,0x77afa1c5,0x20756060],
		[0x85cbfe4e,0x8ae88dd8,0x7aaaf9b0,0x4cf9aa7e,0x1948c25c,0x02fb8a8c,0x01c36ae4,0xd6ebe1f9,0x90d4f869,0xa65cdea0,0x3f09252d,0xc208e69f,0xb74e6132,0xce77e25b,0x578fdfe3,0x3ac372e6]
	]).

	bcrypt_initial_p([
		0x243f6a88,0x85a308d3,0x13198a2e,0x03707344,0xa4093822,0x299f31d0,0x082efa98,0xec4e6c89,0x452821e6,0x38d01377,0xbe5466cf,0x34e90c6c,0xc0ac29b7,0xc97c50dd,0x3f84d5b5,0xb5470917,0x9216d5d9,0x8979fb1b
	]).

:- end_object.
