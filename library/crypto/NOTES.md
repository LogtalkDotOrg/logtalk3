________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


`crypto`
========

This library provides transport-neutral cryptographic predicates.
It currently exports the `crypto` object with the predicates:

- `random_bytes/2`
- `hex_bytes/2`
- `secure_compare/2`
- `hkdf/5`
- `pbkdf2/6`
- `password_hash/4`
- `verify_password_hash/2`

The `random_bytes/2` predicate returns a list with the requested number of
random bytes. It tries to read bytes from `/dev/urandom` first and falls back
to a pseudo-random generator when that source is unavailable.

The `hex_bytes/2` predicate relates hexadecimal atoms with lists of bytes.

The `secure_compare/2` predicate provides constant-time comparison for byte
sequences represented either as byte lists or atoms.

The `hkdf/5` and `pbkdf2/6` predicates provide portable key derivation
implemented on top of the existing `hashes` and `hmac` libraries.

The `password_hash/4` predicate builds on top of `pbkdf2/6` to generate
structured password-hash terms. The `verify_password_hash/2` predicate
verifies both `pbkdf2(Hash, Iterations, Salt, DerivedKey)` terms and
`digest(Hash, StoredDigest)` terms.


API documentation
-----------------

Open the [../../apis/library_index.html#crypto](../../apis/library_index.html#crypto)
link in a web browser.


Loading
-------

To load the library, load the `loader.lgt` file:

	| ?- logtalk_load(crypto(loader)).


Testing
-------

To test this library, load the `tester.lgt` file:

	| ?- logtalk_load(crypto(tester)).


Examples
--------

Generate sixteen random bytes:

	| ?- crypto::random_bytes(16, Bytes).
	Bytes = [42,17,203,91,16,88,121,4,238,75,63,142,7,210,119,55]
	yes

Convert bytes to a hexadecimal atom:

	| ?- crypto::hex_bytes(Hex, [80,26,206]).
	Hex = '501ace'
	yes

Derive 32 bytes using HKDF-SHA-256:

	| ?- crypto::hkdf(sha256, [1,2,3,4], 32, Bytes, [salt([5,6,7,8]),info([9,10])]).
	Bytes = [...]
	yes
