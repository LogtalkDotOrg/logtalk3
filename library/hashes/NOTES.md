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


`hashes`
========

The `hashes` library provides portable implementations of several commonly
used hashing algorithms. All hash objects implement the `hash_protocol`
protocol by providing a `hash/2` predicate that takes a list of bytes and
returns the computed hash as a lowercase hexadecimal atom.

The fixed-size cryptographic hash objects that can be safely used with HMAC
(`blake2s`, `md5`, `sha1`, `sha256`, `blake2b`, `sha512`, `sha512_256`,
`sha3_224`, `sha3_256`, `sha3_384`, and `sha3_512`) also implement the
`hash_digest_protocol` protocol. This protocol adds `digest/2`,
`digest_size/1`, and `block_size/1` predicates so that libraries such as
`hmac` can compute keyed digests without duplicating hash function internals.

The library implements the following hashing algorithms:

- DJB2 32-bit (`djb2_32`)
- DJB2 64-bit (`djb2_64`)
- SDBM 32-bit (`sdbm_32`)
- SDBM 64-bit (`sdbm_64`)
- FNV-1a 32-bit (`fnv1a_32`)
- FNV-1a 64-bit (`fnv1a_64`)
- SipHash (`siphash_2_4`)
- CRC-32 parametric reflected implementation (`crc32_reflected(Polynomial)`)
- CRC-32 parametric non-reflected implementation (`crc32_non_reflected(Polynomial, Initial, FinalXor, AppendLength)`)
- CRC-32/ISO-HDLC (`crc32b`)
- CRC-32C/Castagnoli (`crc32c`)
- CRC-32/POSIX (`crc32posix`)
- CRC-32/MPEG-2 (`crc32mpeg2`)
- CRC-32/BZIP2 (`crc32bzip2`)
- CRC-32Q (aka CRC-32/AIXM) (`crc32q`)
- MurmurHash3 x86 32-bit (`murmurhash3_x86_32`)
- MurmurHash3 x86 128-bit (`murmurhash3_x86_128`)
- MurmurHash3 x64 128-bit (`murmurhash3_x64_128`)
- BLAKE2s (`blake2s`)
- SHA3-224 (`sha3_224`)
- SHA3-256 (`sha3_256`)
- SHA3-384 (`sha3_384`)
- SHA3-512 (`sha3_512`)
- SHAKE128 (`shake128(OutputBytes)`)
- SHAKE256 (`shake256(OutputBytes)`)
- MD5 (`md5`)
- SHA1 (`sha1`)
- SHA256 (`sha256`)
- BLAKE2b (`blake2b`)
- SHA-512 (`sha512`)
- SHA-512/256 (`sha512_256`)

The `djb2_64`, `sdbm_64`, `fnv1a_64`, `siphash_2_4`, `murmurhash3_x86_128`,
`murmurhash3_x64_128`, `blake2b`, `sha3_224`, `sha3_256`, `sha3_384`,
`sha3_512`, `shake128(OutputBytes)`, `shake256(OutputBytes)`, `sha1`,
`sha256`, `sha512`, and `sha512_256` objects are only loaded on backend
Prolog compilers supporting unbounded integer arithmetic.

The SHAKE objects are parametric extensible-output functions. Pass the number
of output bytes to generate when constructing the object.

The `crc32_reflected(Polynomial)` object implements a reflected CRC-32 family
using initial value `0xFFFFFFFF` and final xor value `0xFFFFFFFF`, where
`Polynomial` is the reflected CRC-32 polynomial.

The `crc32_non_reflected(Polynomial, Initial, FinalXor, AppendLength)` object
implements a non-reflected CRC-32 family using a canonical polynomial,
 configurable initial and final xor values, and an `AppendLength` flag that
 controls whether the message length is appended as little-endian bytes.

The `crc32b` object implements the CRC-32/ISO-HDLC variant, also widely used
by Ethernet, gzip, and PKZip. It uses reflected input/output processing, the
reflected polynomial `0xEDB88320` (equivalent to the canonical polynomial
`0x04C11DB7`), initial value `0xFFFFFFFF`, and final xor value `0xFFFFFFFF`.

The `crc32c` object implements the CRC-32C/Castagnoli variant using reflected
input/output processing, the reflected polynomial `0x82F63B78` (equivalent to
the canonical polynomial `0x1EDC6F41`), initial value `0xFFFFFFFF`, and final
xor value `0xFFFFFFFF`.

The `crc32posix` object implements the CRC-32/POSIX variant used by the
standard `cksum` utility. It is an instance of the non-reflected
`crc32_non_reflected(Polynomial, Initial, FinalXor, AppendLength)` family:
it uses the canonical polynomial `0x04C11DB7`,
initial value `0x00000000`, processes bits most-significant first, appends the
message length as little-endian bytes, and applies a final xor value of
`0xFFFFFFFF`.

The `crc32mpeg2` object implements the CRC-32/MPEG-2 variant using the
canonical polynomial `0x04C11DB7`, initial value `0xFFFFFFFF`, no appended
length bytes, and final xor value `0x00000000`.

The `crc32bzip2` object implements the CRC-32/BZIP2 variant using the
canonical polynomial `0x04C11DB7`, initial value `0xFFFFFFFF`, no appended
length bytes, and final xor value `0xFFFFFFFF`.

The `crc32q` object implements the CRC-32Q variant, also used by AIXM-style
formats, using the canonical polynomial `0x814141AB`, initial value
`0x00000000`, no appended length bytes, and final xor value `0x00000000`.

The `siphash_2_4` object uses the standard reference key `00 01 02 ... 0f`.
For custom keys, use the parametric object `siphash_2_4(Key)` where `Key`
is a list of 16 bytes.

The implementations of the hashing algorithms make no attempt to validate
that the input is a list of bytes. When necessary, use the `types` library
`type::check(list(byte), Input)` goal before calling the `hash/2` predicate.


Segmented hashing
------------------

Objects implementing the `hash_state_protocol` protocol (which extends
`hash_protocol`) additionally provide `new_hash_state/1`, `update_hash_state/3`,
and `final_hash_state/2` predicates for genuine incremental hash computation.
Unlike computing a hash from a single, fully materialized message, these
predicates let the caller feed the message to the hash function one chunk at
a time (e.g. as each chunk is read from a file or a network stream),
discarding each chunk as soon as it has been folded into the state. Only the
current chunk and a small, bounded amount of algorithm state (for
block-padded and rate-limited algorithms, a leftover buffer smaller than one
block or rate) are ever resident at once, regardless of the total message
length.

All hashing algorithms in this library implement `hash_state_protocol`.

Example, hashing a message fed in three chunks:

	| ?- md5::new_hash_state(S0),
	     md5::update_hash_state(S0, [84,104,101], S1),
	     md5::update_hash_state(S1, [32,113,117,105,99,107], S2),
	     md5::update_hash_state(S2, [32,98,114,111,119,110], S3),
	     md5::final_hash_state(S3, Hash).
	Hash = '5aa207e85988921b8733912c4f526c80'
	yes


API documentation
-----------------

Open the [../../apis/library_index.html#hashes](../../apis/library_index.html#hashes)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(hashes(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(hashes(tester)).


Examples
--------

Compute the SHA-256 hash of a text message:

	| ?- atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
	     sha256::hash(Bytes, Hash).
	Bytes = [84,104,101,32,113,117,105,99,107,32,98,114,111,119,110,32,102,111,120,32,106,117,109,112,115,32,111,118,101,114,32,116,104,101,32,108,97,122,121,32,100,111,103],
	Hash = 'd7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592'
	yes

Compute a 32-byte SHAKE128 digest:

	| ?- atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
	     shake128(32)::hash(Bytes, Hash).
	Bytes = [84,104,101,32,113,117,105,99,107,32,98,114,111,119,110,32,102,111,120,32,106,117,109,112,115,32,111,118,101,114,32,116,104,101,32,108,97,122,121,32,100,111,103],
	Hash = 'f4202e3c5852f9182a0430fd8144f0a74b95e7417ecae17db0f8cfeed0e3e66e'
	yes

Compute the CRC-32/ISO-HDLC checksum for the standard `123456789` test vector:

	| ?- atom_codes('123456789', Bytes), crc32b::hash(Bytes, Hash).
	Bytes = [49,50,51,52,53,54,55,56,57],
	Hash = 'cbf43926'
	yes

Compute the CRC-32C/Castagnoli checksum for the standard `123456789` test vector:

	| ?- atom_codes('123456789', Bytes), crc32c::hash(Bytes, Hash).
	Bytes = [49,50,51,52,53,54,55,56,57],
	Hash = 'e3069283'
	yes

Compute the CRC-32/POSIX checksum for the standard `123456789` test vector:

	| ?- atom_codes('123456789', Bytes), crc32posix::hash(Bytes, Hash).
	Bytes = [49,50,51,52,53,54,55,56,57],
	Hash = '377a6011'
	yes

Compute the CRC-32/MPEG-2 checksum for the standard `123456789` test vector:

	| ?- atom_codes('123456789', Bytes), crc32mpeg2::hash(Bytes, Hash).
	Bytes = [49,50,51,52,53,54,55,56,57],
	Hash = '0376e6e7'
	yes

Compute the CRC-32/BZIP2 checksum for the standard `123456789` test vector:

	| ?- atom_codes('123456789', Bytes), crc32bzip2::hash(Bytes, Hash).
	Bytes = [49,50,51,52,53,54,55,56,57],
	Hash = 'fc891918'
	yes

Compute the CRC-32Q checksum for the standard `123456789` test vector:

	| ?- atom_codes('123456789', Bytes), crc32q::hash(Bytes, Hash).
	Bytes = [49,50,51,52,53,54,55,56,57],
	Hash = '3010bf7f'
	yes

Use SipHash-2-4 with a custom key on a backend supporting unbounded integer
arithmetic:

	| ?- siphash_2_4([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15])::hash([0,1,2,3], Hash).
	Hash = 'cf2794e0277187b7'
	yes
