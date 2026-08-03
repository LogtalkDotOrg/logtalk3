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


`paseto`
========

The `paseto` library implements Platform-Agnostic Security Tokens (PASETO)
version 4. It supports both `v4.local`, for authenticated encryption using a
shared key, and `v4.public`, for Ed25519 signatures. No earlier PASETO versions
are supported.

The high-level `paseto` object provides JSON claims workflows intended as a
simpler alternative to the `jwt` library. The lower-level `paseto_v4` object
operates on byte lists and exposes footers and implicit assertions directly.

The library requires a backend Prolog compiler with unbounded integer
arithmetic.

API documentation
-----------------

Open the [../../apis/library_index.html#paseto](../../apis/library_index.html#paseto)
link for a detailed description of the library API.

Loading
-------

To load all entities in this library, use the goal:

	| ?- logtalk_load(paseto(loader)).

To test this library, use the goal:

	| ?- logtalk_load(paseto(tester)).

Claims tokens
-------------

Claims are represented using the JSON object representation configured by the
`json` library. By default, authenticated decryption and verification require
an `exp` claim and allow 60 seconds of clock skew. For example:

	| ?- paseto_v4::local_key(Key),
	     Claims = {sub-'alice', exp-4102444800},
	     paseto::encrypt(Claims, Key, Token, []),
	     paseto::decrypt(Token, Key, VerifiedClaims, []).

Use `allow_missing_exp(true)` only when a token deliberately has no expiration.
Additional validation can be selected using `required_claims/1` and
`claim_policy/1`. Supported claim policies are:

- `claim(Name, required)`
- `claim(Name, expected(Value))`
- `claim(Name, one_of(Values))`
- `claim(Name, contains(Value))`
- `claim(Name, time(expiration))`
- `claim(Name, time(not_before))`
- `claim(Name, time(issued_at))`
- `claim(Name, custom(Verifier))`

Time validation options include `now/1`, `clock_skew/1`, and `max_age/1`.

Public tokens
-------------

Generate an Ed25519 seed and public key, sign claims, and verify the token as
follows:

	| ?- paseto_v4::public_keypair(Seed, PublicKey),
	     Claims = {sub-'alice', exp-4102444800},
	     paseto::sign(Claims, Seed, Token, []),
	     paseto::verify(Token, PublicKey, VerifiedClaims, []).

The `claims/2` predicate decodes claims from a `v4.public` token without
verifying its signature. Its result is untrusted and must not be used for
authorization decisions.

Footers and key sets
--------------------

A footer can carry application metadata. The `key_id/1` option adds a `kid`
member and can be combined with `footer/1`. Native key sets use purpose-tagged
records:

	key_set([
		local('local-2026', LocalKey),
		public('public-2026', PublicKey)
	])

When decrypting or verifying with a key set, the facade reads the unauthenticated
footer to select matching keys and authenticates the complete footer before
returning claims. The `peek_key_id/2` and `paseto_v4::footer/2` results are only
suitable for this pre-authentication selection step.

Raw byte API
------------

The `paseto_v4` object implements `paseto_protocol` and provides:

- `local_encrypt/3-5` and `local_decrypt/3-5`
- `public_sign/3-5` and `public_verify/3-5`
- `local_key/1` and `public_keypair/2`
- `footer/2`

Payloads, keys, footers, and implicit assertions are lists of bytes. Local keys,
Ed25519 seeds, and Ed25519 public keys are 32 bytes. Authentication failures
fail without returning unauthenticated plaintext.
