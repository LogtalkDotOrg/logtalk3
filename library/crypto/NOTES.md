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

This library provides transport-neutral cryptographic predicates. Some of
its functionality requires a backend supporting unbound integer arithmetic.


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

Some of the official test vectors for PBKDF2 use 4096 and 16777216 iterations.
Those test vectors are not used as they that don't increase code coverage: in
PBKDF2, iteration count is a work factor, not an algorithmic requirement.
Instead, tests with  much smaller number of iterations are used to speedup
testing on slower backends without loosing code coverage.


Scope
-----

The `crypto` object provides with the following public predicates:

- `random_below/2`
- `random_bytes/2`
- `hex_bytes/2`
- `token_hex/2`
- `token_urlsafe/2`
- `secure_compare/2`
- `hkdf/5`
- `pbkdf2/6`
- `apr1/3`
- `bcrypt/4`
- `password_hash/4`
- `password_hash_atom/2`
- `password_hash_needs_rehash/3`
- `verify_password_hash/2`

When using a backend that supports unbound integer arithmetic, the following
authenticated encryption with associated data and public-key signature public
predicates are also available:

- `xchacha20_poly1305_encrypt/5`
- `xchacha20_poly1305_decrypt/5`

- `ed25519_keypair/2`
- `ed25519_public_key/2`
- `ed25519_sign/3`
- `ed25519_verify/3`

- `x25519_keypair/2`
- `x25519_public_key/2`
- `x25519_shared_secret/3`

- `authenticated_channel_initiate/4`
- `authenticated_channel_accept/5`
- `authenticated_channel_finalize/3`
- `authenticated_channel_encrypt/5`
- `authenticated_channel_decrypt/5`

The `random_below/2` predicate returns a uniformly distributed random integer
greater than or equal to zero and less than the given exclusive upper bound.

The `random_bytes/2` predicate returns a list with the requested number of
random bytes. It tries to read bytes from `/dev/urandom` first and falls back
to a pseudo-random generator when that source is unavailable.

The `hex_bytes/2` predicate relates hexadecimal atoms with lists of bytes.

The `token_hex/2` and `token_urlsafe/2` predicates return either a lowercase
hexadecimal token or a unpadded Base64URL token generated from the requested
number of random bytes.

The `secure_compare/2` predicate provides constant-time comparison for byte
sequences represented either as byte lists or atoms.

The `hkdf/5` and `pbkdf2/6` predicates provide portable key derivation
conforming to RFC 5869 and RFC 8018, respectively, implemented on top of
the existing `hashes` and `hmac` libraries.

The `apr1/3` and `bcrypt/4` predicates compute portable Apache APR1 and bcrypt
2b checksums. Bcrypt accepts passwords of at most 72 bytes, costs from 4
through 31, and exactly 16 raw salt bytes.

The `password_hash/4` predicate generates structured password hashes using
`pbkdf2(Hash)`, `bcrypt`, or `apr1` method descriptors; a bare hash object is
PBKDF2/PKCS#5 shorthand. Options are method-specific and inapplicable options
are rejected. The `password_hash_needs_rehash/3` predicate compares a valid
stored term with the selected target method and its parameters. The
`verify_password_hash/2` predicate verifies PBKDF2/PKCS#5, digest, APR1, and
bcrypt terms. The bidirectional `password_hash_atom/2` predicate converts
canonical `$2b$` bcrypt and `$apr1$` atoms to and from their structured terms.

The `xchacha20_poly1305_encrypt/5` and `xchacha20_poly1305_decrypt/5`
predicates provide authenticated encryption with associated data (AEAD)
using XChaCha20-Poly1305: ChaCha20 with the 24-byte extended nonce defined
by the XChaCha construction (via HChaCha20 subkey derivation), combined with
a Poly1305 authentication tag over the associated data and ciphertext,
following the IETF ChaCha20-Poly1305 construction (RFC 8439). Decryption
verifies the tag with constant-time comparison before decrypting anything,
and fails, without producing any plaintext, if the tag does not match. The
`xchacha20/4` predicate exposes the underlying unauthenticated stream cipher
for clients such as PASETO that provide their own authentication. It must only
be used as part of an authenticated construction. These predicates are only
available on backend Prolog compilers supporting unbounded integer arithmetic,
since Poly1305 requires an exact 130-bit accumulator.

The `ed25519_keypair/2`, `ed25519_public_key/2`, `ed25519_sign/3`, and
`ed25519_verify/3` predicates implement EdDSA signatures over Curve25519
(RFC 8032). The 32-byte seed passed to `ed25519_sign/3` (and returned by
`ed25519_keypair/2`) is what RFC 8032 calls the private key; signing is
fully deterministic, with no separate nonce to generate or protect.
`ed25519_verify/3` checks the cofactored group equation `[8][S]B = [8]R +
[8][k]A`, as recommended by RFC 8032, and additionally requires the public
key and the R component of the signature to be canonically encoded and of
large order, rejecting the seven points of order dividing 8 (matching the
hardening most widely-deployed Ed25519 implementations apply for "strongly
binding signature" security; see Chalkias, Garillot, and Nikolaenko,
"Taming the many EdDSAs", 2020). `S` is required to satisfy `0 <= S < L`,
which RFC 8032 mandates unconditionally. Signing uses simple double-and-add
scalar multiplication and is not constant-time, so timing side channels on
the secret scalar are possible on the signing path; verification never
touches secret data and is unaffected by that limitation. These four
predicates are only available on backend Prolog compilers supporting
unbounded integer arithmetic, since Curve25519 arithmetic is carried out
modulo a 255-bit prime.

The `x25519_keypair/2`, `x25519_public_key/2`, and `x25519_shared_secret/3`
predicates implement X25519 Diffie-Hellman key agreement (RFC 7748). Private
keys and public keys are represented by 32-byte lists. Public-key derivation
and shared-secret computation apply the RFC 7748 scalar clamping rules and
mask the most significant bit of received u-coordinates. Shared-secret
computation fails for an all-zero result, preventing the use of low-order
public keys. These predicates require unbounded integer arithmetic. Although
the Montgomery ladder has fixed control flow and uses arithmetic conditional
swaps, the underlying Prolog big-integer operations are not guaranteed to be
constant-time.

The authenticated channel predicates compose Ed25519 identity signatures,
ephemeral X25519 key agreement, HKDF-SHA-256, and XChaCha20-Poly1305. The
initiator calls `authenticated_channel_initiate/4` with its identity seed and
the responder Ed25519 public key obtained through a trusted out-of-band
mechanism. The responder calls `authenticated_channel_accept/5` with its
identity seed, the pinned initiator identity public key, and the offer. The
initiator completes the handshake using `authenticated_channel_finalize/3`.
Both signatures bind the protocol version, roles, both identity keys, and the
ephemeral keys; the responder signature also binds the initiator offer
signature.

The handshake derives independent initiator-to-responder and
responder-to-initiator chain keys and nonce prefixes. The
`authenticated_channel_encrypt/5` and `authenticated_channel_decrypt/5`
predicates are pure state transitions: every successful call returns a
replacement channel state. Messages use strict 64-bit counters and must be
received in order. Their XChaCha20-Poly1305 associated data binds the protocol
version, direction, transcript hash, counter, and caller-provided associated
data. Replay, out-of-order delivery, modified associated data, and modified
ciphertext therefore fail without advancing the state.

Each message derives a one-use message key and replacement directional chain
key. This symmetric ratchet protects already processed messages if only the
current state is later compromised, provided applications discard all prior
immutable state terms. Logtalk cannot guarantee that discarded key material
is immediately overwritten in memory. This construction is not a Double
Ratchet: compromise of the current state is not healed without a new
authenticated ephemeral handshake, and out-of-order message delivery is not
supported.

The key-pair predicates use `random_bytes/2`. If `/dev/urandom` is unavailable,
that predicate falls back to the library pseudo-random generator. Applications
that require fail-closed cryptographic entropy must generate private key bytes
externally and call `ed25519_public_key/2` or `x25519_public_key/2` instead.


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

Compute an Apache APR1 checksum:

	| ?- crypto::apr1([112,97,115,115,119,111,114,100], [112,111,114,116,97,98,108,101], Checksum).
	Checksum = [107,117,110,70,78,90,57,114,48,81,57,54,51,88,98,115,116,101,79,79,87,46]
	yes

Encrypt and authenticate a short message with XChaCha20-Poly1305, then
decrypt and verify it:

	| ?- Key = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31],
	     Nonce = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23],
	     crypto::xchacha20_poly1305_encrypt(Key, Nonce, [], [72,105], CiphertextAndTag),
	     crypto::xchacha20_poly1305_decrypt(Key, Nonce, [], CiphertextAndTag, Plaintext).
	Plaintext = [72,105]
	yes

Generate an Ed25519 keypair, sign a message, and verify the signature:

	| ?- crypto::ed25519_keypair(Seed, PublicKey),
	     crypto::ed25519_sign(Seed, [72,105], Signature),
	     crypto::ed25519_verify(PublicKey, [72,105], Signature).
	yes

Create an authenticated ephemeral channel between Alice and Bob and send a
message from Alice to Bob. In an application, `AlicePublicKey` and
`BobPublicKey` must be obtained through a trusted identity mechanism:

	| ?- crypto::ed25519_keypair(AliceSeed, AlicePublicKey),
	     crypto::ed25519_keypair(BobSeed, BobPublicKey),
	     crypto::authenticated_channel_initiate(AliceSeed, BobPublicKey, Offer, Pending),
	     crypto::authenticated_channel_accept(BobSeed, AlicePublicKey, Offer, Response, BobChannel0),
	     crypto::authenticated_channel_finalize(Pending, Response, AliceChannel0),
	     crypto::authenticated_channel_encrypt(AliceChannel0, [], [72,105], Message, AliceChannel1),
	     crypto::authenticated_channel_decrypt(BobChannel0, [], Message, Plaintext, BobChannel1).
	Plaintext = [72,105]
	yes

The application must retain `AliceChannel1` and `BobChannel1` for subsequent
messages and discard the corresponding input states.
