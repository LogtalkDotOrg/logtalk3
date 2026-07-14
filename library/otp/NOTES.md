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


`otp`
=====

The `otp` library provides predicates for generating and verifying HOTP and
TOTP one-time passwords following:

https://www.rfc-editor.org/rfc/rfc4226

https://www.rfc-editor.org/rfc/rfc6238

The implementation is built on top of the `hmac` and `base32` libraries.


API documentation
-----------------

Open the [../../apis/library_index.html#otp](../../apis/library_index.html#otp)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` utility file:

	| ?- logtalk_load(otp(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(otp(tester)).


Public predicates
-----------------

The library currently provides the following public predicates:

- `otp::hotp/5`
- `otp::totp/5`
- `otp::hotp_verify/7`
- `otp::totp_verify/7`

The arguments are:

- `hotp(Hash, Secret, Counter, Digits, OTP)`
- `totp(Hash, Secret, UnixTime, Digits, OTP)`
- `hotp_verify(Hash, Secret, Counter, Window, Digits, OTP, MatchedCounter)`
- `totp_verify(Hash, Secret, UnixTime, Window, Digits, OTP, MatchedTimeStep)`

The `totp/5` and `totp_verify/7` predicates take an explicit Unix time in
seconds. The current implementation uses the standard 30-second time step and
the Unix epoch `T0 = 0`.


Secrets
-------

The `Secret` argument can be given either as a raw byte list or using one of
the following Base32 wrapper terms:

- `base32(atom(Atom))`
- `base32(chars(Chars))`
- `base32(codes(Codes))`

For example:

	| ?- otp::hotp(sha1, base32(atom('GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ')), 0, 6, OTP).
	OTP = '755224'
	yes

	| ?- atom_codes('12345678901234567890', Secret),
	     otp::hotp(sha1, Secret, 1, 6, OTP).
	OTP = '287082'
	Secret = [49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,48]
	yes


Generation
----------

HOTP generation uses the supplied moving counter directly:

	| ?- atom_codes('12345678901234567890', Secret),
	     otp::hotp(sha1, Secret, 9, 6, OTP).
	OTP = '520489'
	yes

TOTP generation first converts the explicit Unix time to the corresponding
30-second time step and then computes an HOTP value for that step:

	| ?- atom_codes('12345678901234567890', Secret),
	     otp::totp(sha1, Secret, 59, 8, OTP).
	OTP = '94287082'
	yes

Generated OTP values are returned as zero-padded atoms so that leading zeros
are preserved.


Verification
------------

The `otp::hotp_verify/7` predicate searches forward from the supplied counter
through a bounded counter window and returns the matching counter on success.

	| ?- atom_codes('12345678901234567890', Secret),
	     otp::hotp_verify(sha1, Secret, 0, 9, 6, '520489', MatchedCounter).
	MatchedCounter = 9
	Secret = [49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,48]
	yes

The `otp::totp_verify/7` predicate searches within a symmetric time-step
window around the current time step and returns the matching time step on
success.

	| ?- atom_codes('12345678901234567890', Secret),
	     otp::totp_verify(sha1, Secret, 29, 1, 8, '94287082', MatchedTimeStep).
	MatchedTimeStep = 1
	Secret = [49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,48]
	yes


Hash support
------------

The `Hash` argument must identify a currently loaded hash object that provides
`digest/2`, `digest_size/1`, and `block_size/1`, and whose digest size is at
least 20 bytes.

In practice, the standard RFC examples in this library use `sha1`, `sha256`,
and `sha512` on unbounded-integer backends. Hashes such as `md5` are rejected
because their digest size is too short for this library's validation rules.


Bounded-backend limitation
--------------------------

The current repository hash implementation only defines `sha1`, `sha256`, and
`sha512` on unbounded-integer backends. On bounded-integer backends, the
`hashes` library loads `hash_32.lgt`, which does not provide those digest
objects.

As a consequence:

- the `otp` library itself still loads on bounded backends
- standard HOTP/TOTP generation with `sha1`, `sha256`, or `sha512` is not
	available there
- calls such as `otp::hotp(sha1, Secret, Counter, Digits, OTP)` fail with
  `domain_error(otp_hash, sha1)` on bounded-integer backends

This is a limitation of the currently available hash objects in the repository,
not of the OTP control-flow logic itself.
