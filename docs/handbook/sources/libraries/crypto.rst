.. _library_crypto:

``crypto``
==========

This library provides transport-neutral cryptographic predicates. The
``crypto`` object provides with the following public predicates:

- ``random_below/2``
- ``random_bytes/2``
- ``hex_bytes/2``
- ``token_hex/2``
- ``token_urlsafe/2``
- ``secure_compare/2``
- ``hkdf/5``
- ``pbkdf2/6``
- ``apr1/3``
- ``password_hash/4``
- ``password_hash_needs_rehash/3``
- ``verify_password_hash/2``

When using a backend that supports unbound integer arithmetic, the
following authenticated encryption with associated data and public-key
signature public predicates are also available:

- ``xchacha20_poly1305_encrypt/5``

- ``xchacha20_poly1305_decrypt/5``

- ``ed25519_keypair/2``

- ``ed25519_public_key/2``

- ``ed25519_sign/3``

- ``ed25519_verify/3``

The ``random_below/2`` predicate returns a uniformly distributed random
integer greater than or equal to zero and less than the given exclusive
upper bound.

The ``random_bytes/2`` predicate returns a list with the requested
number of random bytes. It tries to read bytes from ``/dev/urandom``
first and falls back to a pseudo-random generator when that source is
unavailable.

The ``hex_bytes/2`` predicate relates hexadecimal atoms with lists of
bytes.

The ``token_hex/2`` and ``token_urlsafe/2`` predicates return either a
lowercase hexadecimal token or a unpadded Base64URL token generated from
the requested number of random bytes.

The ``secure_compare/2`` predicate provides constant-time comparison for
byte sequences represented either as byte lists or atoms.

The ``hkdf/5`` and ``pbkdf2/6`` predicates provide portable key
derivation implemented on top of the existing ``hashes`` and ``hmac``
libraries.

The ``apr1/3`` predicate computes Apache APR1 encoded checksums for
password and salt byte sequences using a portable MD5-based
implementation.

The ``password_hash/4`` predicate builds on top of ``pbkdf2/6`` to
generate structured password-hash terms. The
``password_hash_needs_rehash/3`` predicate checks stored password-hash
terms against the current PBKDF2 policy. The ``verify_password_hash/2``
predicate verifies ``pbkdf2(Hash, Iterations, Salt, DerivedKey)``,
``digest(Hash, StoredDigest)``, and ``apr1(Salt, Checksum)`` terms.

The ``xchacha20_poly1305_encrypt/5`` and
``xchacha20_poly1305_decrypt/5`` predicates provide authenticated
encryption with associated data (AEAD) using XChaCha20-Poly1305:
ChaCha20 with the 24-byte extended nonce defined by the XChaCha
construction (via HChaCha20 subkey derivation), combined with a Poly1305
authentication tag over the associated data and ciphertext, following
the IETF ChaCha20-Poly1305 construction (RFC 8439). Decryption verifies
the tag with constant-time comparison before decrypting anything, and
fails, without producing any plaintext, if the tag does not match. The
``xchacha20/4`` predicate exposes the underlying unauthenticated stream
cipher for clients such as PASETO that provide their own authentication.
It must only be used as part of an authenticated construction. These
predicates are only available on backend Prolog compilers supporting
unbounded integer arithmetic, since Poly1305 requires an exact 130-bit
accumulator.

The ``ed25519_keypair/2``, ``ed25519_public_key/2``, ``ed25519_sign/3``,
and ``ed25519_verify/3`` predicates implement EdDSA signatures over
Curve25519 (RFC 8032). The 32-byte seed passed to ``ed25519_sign/3``
(and returned by ``ed25519_keypair/2``) is what RFC 8032 calls the
private key; signing is fully deterministic, with no separate nonce to
generate or protect. ``ed25519_verify/3`` checks the cofactored group
equation ``[8][S]B = [8]R + [8][k]A``, as recommended by RFC 8032, and
additionally requires the public key and the R component of the
signature to be canonically encoded and of large order, rejecting the
seven points of order dividing 8 (matching the hardening most
widely-deployed Ed25519 implementations apply for "strongly binding
signature" security; see Chalkias, Garillot, and Nikolaenko, "Taming the
many EdDSAs", 2020). ``S`` is required to satisfy ``0 <= S < L``, which
RFC 8032 mandates unconditionally. Signing uses simple double-and-add
scalar multiplication and is not constant-time, so timing side channels
on the secret scalar are possible on the signing path; verification
never touches secret data and is unaffected by that limitation. These
four predicates are only available on backend Prolog compilers
supporting unbounded integer arithmetic, since Curve25519 arithmetic is
carried out modulo a 255-bit prime.

API documentation
-----------------

Open the
`../../apis/library_index.html#crypto <../../apis/library_index.html#crypto>`__
link in a web browser.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(crypto(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(crypto(tester)).

Examples
--------

Generate sixteen random bytes:

::

   | ?- crypto::random_bytes(16, Bytes).
   Bytes = [42,17,203,91,16,88,121,4,238,75,63,142,7,210,119,55]
   yes

Convert bytes to a hexadecimal atom:

::

   | ?- crypto::hex_bytes(Hex, [80,26,206]).
   Hex = '501ace'
   yes

Derive 32 bytes using HKDF-SHA-256:

::

   | ?- crypto::hkdf(sha256, [1,2,3,4], 32, Bytes, [salt([5,6,7,8]),info([9,10])]).
   Bytes = [...]
   yes

Compute an Apache APR1 checksum:

::

   | ?- crypto::apr1([112,97,115,115,119,111,114,100], [112,111,114,116,97,98,108,101], Checksum).
   Checksum = [107,117,110,70,78,90,57,114,48,81,57,54,51,88,98,115,116,101,79,79,87,46]
   yes

Encrypt and authenticate a short message with XChaCha20-Poly1305, then
decrypt and verify it:

::

   | ?- Key = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31],
        Nonce = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23],
        crypto::xchacha20_poly1305_encrypt(Key, Nonce, [], [72,105], CiphertextAndTag),
        crypto::xchacha20_poly1305_decrypt(Key, Nonce, [], CiphertextAndTag, Plaintext).
   Plaintext = [72,105]
   yes

Generate an Ed25519 keypair, sign a message, and verify the signature:

::

   | ?- crypto::ed25519_keypair(Seed, PublicKey),
        crypto::ed25519_sign(Seed, [72,105], Signature),
        crypto::ed25519_verify(PublicKey, [72,105], Signature).
   yes
