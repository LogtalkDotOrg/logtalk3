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


`typeid`
========

This library implements a type-safe, K-sortable, globally unique identifier
(TypeID) generator, as specified at:

https://github.com/jetify-com/typeid/tree/main/spec

A TypeID is the concatenation of a type prefix and a version 7 UUID suffix,
separated by an underscore, e.g. `user_01h2xcejqtf2nbrexx3vqjhp41`. The type
prefix is a, possibly empty, lowercase string that, together with a leading
"_" separator when non-empty, identifies the "type" of the entity that the
identifier refers to. The UUID suffix is always a fixed 26 character string
that encodes the 128 bits of a UUID using a strict, lowercase variant of
Crockford base32 (the alphabet used is `0123456789abcdefghjkmnpqrstvwxyz`,
i.e. the letters "i", "l", "o", and "u" are not used, avoiding ambiguity with
"1", "1", "0", and "v"/"y"). This base32 variant is unrelated to the RFC 4648
base32 encoding implemented by the `base32` library: the suffix always
encodes exactly 128 bits (the two zero bits conceptually prepended to a UUID
plus its 128 bits are split into 26 groups of 5 bits, most significant group
first) using no padding, whereas RFC 4648 base32 encodes byte streams of any
length in 40-bit (5-byte) chunks, using "=" padding for partial chunks.

Generation always uses a version 7 UUID, which is time-ordered using a Unix
Epoch timestamp in milliseconds, as specified in RFC 9562, making the
generated TypeIDs K-sortable. Decoding a TypeID from an user provided UUID
(using the `from_uuid/3` predicate) accepts any UUID version, however, as
implementations are expected to allow encoding of other UUID versions at the
user discretion.

TypeIDs can be generated as atoms, lists of characters, or lists of character
codes.

See also the `cuid2`, `ksuid`, `ids`, `nanoid`, `snowflakeid`, `sqids`,
`ulid`, and `uuid` libraries.


API documentation
-----------------

Open the [../../apis/library_index.html#typeid](../../apis/library_index.html#typeid)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(typeid(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(typeid(tester)).


Generating TypeIDs
-------------------

By default, TypeIDs are generated as atoms. To generate a TypeID with an
empty type prefix:

	| ?- typeid::generate(TypeID).
	TypeID = '01kyaqvrf2f13t3fe2b16329jj'
	yes

To generate a TypeID with a given type prefix:

	| ?- typeid::generate(user, TypeID).
	TypeID = 'user_01kyaqvrf2e2mry9en73p93dkp'
	yes

To generate a TypeID using a list of characters representation, use instead
the `typeid/1` parametric object:

	| ?- typeid(chars)::generate(user, TypeID).
	TypeID = [u,s,e,r,'_','0','1',k,y,a,q,v,r,f,'2',e,z,g,'8',z,'7','4',r,r,z,d,'6',g,'8',p,j]
	yes

Similarly, to get a TypeID using a list of character codes representation:

	| ?- typeid(codes)::generate(user, TypeID).
	TypeID = [117,115,101,114,95,48,49,107,121,97,113,118,114,102,50,101,51,107,114,55,109,107,51,118,122,120,106,122,116,114,120]
	yes

When the backend only provides access to local time, use `generate/3` with
the current local UTC offset to compute the UUID timestamp using UTC:

	| ?- typeid::generate(user, '+01:00', TypeID).
	TypeID = 'user_01kyaqvrf2f72a4eksrcy9vnwe'
	yes

Generation fails if the given type prefix is not valid per the TypeID
specification (see below).


Converting to and from UUIDs
-----------------------------

To construct a TypeID from an existing UUID (which does not need to be a
version 7 UUID):

	| ?- typeid::from_uuid(prefix, '0188bac7-4afa-78aa-bc3b-bd1eef28d881', TypeID).
	TypeID = 'prefix_01h2xcejqtf2nbrexx3vqjhp41'
	yes

To recover the UUID encoded in the suffix of a TypeID:

	| ?- typeid::to_uuid('prefix_01h2xcejqtf2nbrexx3vqjhp41', UUID).
	UUID = '0188bac7-4afa-78aa-bc3b-bd1eef28d881'
	yes


Decomposing and composing TypeIDs
-----------------------------------

To split a TypeID into its type prefix and its base32 encoded UUID suffix:

	| ?- typeid::decompose('prefix_01h2xcejqtf2nbrexx3vqjhp41', Prefix, Suffix).
	Prefix = prefix, Suffix = '01h2xcejqtf2nbrexx3vqjhp41'
	yes

To perform the reverse operation:

	| ?- typeid::compose(prefix, '01h2xcejqtf2nbrexx3vqjhp41', TypeID).
	TypeID = 'prefix_01h2xcejqtf2nbrexx3vqjhp41'
	yes

The `prefix/2` and `suffix/2` predicates can also be used to access just the
type prefix or just the UUID suffix of a TypeID.


Validating TypeIDs
--------------------

To check that a TypeID, a type prefix, or a base32 encoded UUID suffix, is
valid per the TypeID specification:

	| ?- typeid::valid('prefix_01h2xcejqtf2nbrexx3vqjhp41').
	yes

	| ?- typeid::valid_prefix(prefix).
	yes

	| ?- typeid::valid_suffix('01h2xcejqtf2nbrexx3vqjhp41').
	yes

A type prefix must be empty or match the regular expression
`^[a-z]([a-z_]{0,61}[a-z])?$` (i.e. at most 63 lowercase ASCII letters,
optionally including underscores, but never starting or ending with an
underscore). A base32 encoded UUID suffix must be exactly 26 characters long,
using only the strict, lowercase Crockford base32 alphabet described above,
with its first character decoding to a value no greater than 7 (as required
for the suffix to represent no more than the 128 bits of a UUID).
