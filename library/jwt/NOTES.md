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


`jwt`
=====

The `jwt` library provides compact JWT parsing, JWS signing and verification,
JWK/JWKS key selection, and reusable registered-claim validation predicates.

The current implementation supports native `HS256` signing/verification and
OpenSSL-backed `RS256`/`ES256` verification. `ES256` signatures are converted
from JOSE raw `R || S` encoding to DER before calling OpenSSL.

This library requires a Prolog backend supporting unbound integer arithmetic.


Requirements
------------

The `openssl` command must be available on the current `PATH`.

On macOS, it can be installed using e.g. Homebrew:

	$ brew install openssl

Or using MacPorts:

	$ sudo port install openssl

On Ubuntu, it can be installed using:

	$ sudo apt install openssl

On RedHat distributions (8.x and later):

	$ sudo dnf install openssl

For older RedHat distributions:

    $ sudo yum install openssl

On Windows, it can be installed using e.g. Chocolatey:

	> choco install openssl


Loading
-------

To load the library, load the `loader.lgt` file:

	| ?- logtalk_load(jwt(loader)).


Testing
-------

To test this library, load the `tester.lgt` file:

	| ?- logtalk_load(jwt(tester)).


Options
-------

The facade predicates that take an options list validate the following option
terms. Each option only affects predicates that perform the corresponding
operation.

Signature and algorithm options:

* `allow_algorithms(Algorithms)` - List of allowed JOSE `alg` atom values for
	verification and signing. The default is `['HS256', 'RS256', 'ES256']`.
* `algorithm(Algorithm)` - Requires one exact JOSE `alg` atom value. When used
	with `allow_algorithms/1`, both checks must pass.
* `allow_none(Boolean)` - Allows or rejects the unsecured `none` algorithm. The
	default is `false`.

Claim validation options:

* `allow_missing_exp(Boolean)` - Allows tokens without an `exp` claim when set
	to `true`. The default is `false`.
* `clock_skew(Seconds)` - Non-negative numeric leeway, in seconds, for time
	claim validation. The default is `60`.
* `now(Timestamp)` - Numeric timestamp to use instead of the current operating
	system time when validating time claims.
* `max_age(Seconds)` - Non-negative maximum age, in seconds, for policies that
	validate an `iat` claim as `time(issued_at)`.
* `required_claims(Names)` - List of claim-name atoms that must be present when
	validating claims. The default is `[]`.
* `claim_policy(Policy)` - Claim policy list applied by `verify/4` and
	`verify/5` after signature verification. When calling `validate_claims/3`
	directly, pass the policy as the second argument instead.

OpenSSL options:

* `openssl_executable(Executable)` - Atom naming the OpenSSL executable or its
	path. The default command name is `openssl`.
* `openssl_arguments(Arguments)` - List accepted by the option checker for
	OpenSSL command customization. The current facade implementation does not add
	these arguments to the generated OpenSSL command.


Validation policy
-----------------

The verification predicates apply a conservative JWT validation policy in
addition to signature verification:

* JWT headers and claims must decode to JSON objects. Objects with duplicate
	member names are rejected instead of using first-wins or last-wins semantics.
* JWS `crit` header parameters are rejected unless explicitly supported by the
	library. The current implementation does not support extension header
	parameters that can be marked critical.
* The `exp` claim is required by default. This is stricter than the base JWT
	specification, where registered claims are optional. Use the option
	`allow_missing_exp(true)` when verifying tokens whose issuer legitimately
	omits this claim.
* Use `allow_algorithms/1` to configure the allowed algorithm set. Use
	`algorithm/1` when the caller expects one exact JOSE `alg` value. When both
	options are present, both constraints must be satisfied.
* When verifying with a JWK Set, all keys matching the token header `alg` and
	optional `kid` are tried in set order until a signature verifies. Verification
	fails only after all matching candidate keys fail.
* `HS256` symmetric keys must contain at least 32 bytes of key material,
	matching the algorithm's 256-bit security requirement.
* RSA JWK `n` and `e` members must be minimally encoded positive
	Base64urlUInt values. `RS256` public moduli must be odd and at least 2048
	bits, and public exponents must be odd integers greater than one and smaller
	than the modulus.
* P-256 EC JWK coordinates must be 32-byte unsigned values in range and the
	point `(x,y)` must lie on the P-256 curve.
* Octet, RSA, and P-256 EC JWKs are validated before key material is used.
	Malformed key objects raise JWT key-domain errors.

OpenSSL-backed `RS256` and `ES256` verification requires an `openssl` command in
the current command search path, unless overridden using `openssl_executable/1`.


Basic usage
-----------

Decode a compact JWT without verifying its signature:

	| ?- jwt::decode(Token, Header, Claims).

Inspect only selected header or claims data:

	| ?- jwt::peek_algorithm(Token, Algorithm).

	| ?- jwt::claim(Claims, sub, Subject).

Sign and verify a token using native `HS256` support:

	| ?- Header = {alg-'HS256', typ-'JWT'},
	     Claims = {sub-'123', exp-4102444800},
	     jwt::sign(Header, Claims, '0123456789abcdef0123456789abcdef', Token, []),
	     jwt::verify(Token, '0123456789abcdef0123456789abcdef', VerifiedClaims, []).

Validate claims independently from signature verification:

	| ?- Policy = [
	         claim(iss, expected('https://issuer.example')),
	         claim(aud, contains('client-id')),
	         claim(exp, time(expiration))
	     ],
	     jwt::validate_claims(Claims, Policy, [now(1700000001)]).

Verify an asymmetric token using a JWK Set. The library selects matching keys
using the JWT header `alg` and optional `kid` values:

	| ?- JWKSet = {keys-[PublicJWK]},
	     jwt::verify(Token, JWKSet, Claims, [
	         allow_algorithms(['RS256', 'ES256']),
	         claim_policy([
	             claim(iss, expected('https://issuer.example')),
	             claim(aud, contains('client-id'))
	         ])
	     ]).

Use the `openssl_executable/1` option when the OpenSSL command is not named
`openssl` or is not found in the default command search path:

	| ?- jwt::verify(Token, JWKSet, Claims, [
	         openssl_executable('/opt/local/bin/openssl')
	     ]).
