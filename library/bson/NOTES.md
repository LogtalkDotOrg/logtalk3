________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>  
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


`bson`
======

The `bson` library provides predicates for parsing and generating the BSON
1.1 binary interchange format:

- https://bsonspec.org/spec.html

The library requires a backend Prolog compiler with unbounded integer
arithmetic support.


API documentation
-----------------

Open the [../../apis/library_index.html#bson](../../apis/library_index.html#bson)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(bson(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(bson(tester)).

The test set uses the official MongoDB BSON corpus files in the `test_files`
directory. Canonical BSON vectors are parsed successfully and used for semantic
generator round-trip tests, and BSON decode-error vectors are checked for
rejection. See `test_files/NOTES.md` for provenance.


Representation
--------------

- Documents are represented by ordered curly-bracketed terms, `{Pairs}`,
where each pair is represented by `Key-Value`. The empty document is `{}`.
Pair order and duplicate keys are preserved.

- Arrays are represented by lists. BSON array keys are generated and validated
as the canonical sequence `"0"`, `"1"`, and so on.

- Strings and document keys can be represented by atoms, `chars(List)`, or
`codes(List)`. Use the `bson/1` parameterized object with the parameter bound
to `atom`, `chars`, or `codes` to select the decoded representation. BSON
strings and keys are validated using strict UTF-8 conversion.

- The BSON Boolean, null, undefined, minimum-key, and maximum-key values are
represented by `@false`, `@true`, `@null`, `@undefined`, `@min_key`, and
`@max_key`, respectively.

- BSON 32-bit and 64-bit integers are represented by `int32(Integer)` and
`int64(Integer)`. Decoding preserves the encoded width. Generation also
accepts plain integers, using `int32` when possible and `int64` otherwise.

- BSON doubles are represented by floats. IEEE 754 infinities and NaNs use
`@infinity`, `@negative_infinity`, `@not_a_number`, and
`not_a_number(Bytes)`, following the `ieee_754` library conventions.
Standard Prolog does not provide a portable distinct representation for IEEE
754 negative zero. Depending on the backend, parsing negative zero may produce
`0.0`, in which case generation cannot recover the original sign bit.

- Binary values use `binary(Subtype, bytes(Bytes))`. Subtypes 0 through 9 and
user-defined subtypes 128 through 255 are supported. The deprecated subtype 2
inner length is parsed and generated transparently. Payloads for encrypted,
compressed-column, sensitive, and vector subtypes are kept opaque.

- Object identifiers use `object_id(bytes(Bytes))`, where `Bytes` contains
exactly 12 bytes.

- UTC datetimes use `date_time(Milliseconds)`, where `Milliseconds` is the
signed 64-bit count since the Unix epoch.

- Regular expressions use `regular_expression(Pattern, Options)`. Options
must be unique, alphabetically sorted, and selected from `i`, `m`, `s`, `u`,
and `x`.

- DBPointer, JavaScript, symbol, and JavaScript-with-scope values use
`db_pointer(Namespace, ObjectId)`, `javascript(Code)`, `symbol(Symbol)`, and
`javascript(Code, Scope)`, respectively. These deprecated BSON values remain
fully readable and writable.

- Timestamps use `timestamp(Increment, Seconds)`, with both fields represented
as unsigned 32-bit integers.

- Decimal128 values use the lossless wire representation
`decimal128(bytes(Bytes))`, where `Bytes` contains exactly 16 bytes. Decimal
arithmetic and conversion are outside this library's scope.


Encoding and decoding
---------------------

The public API consists of `generate/2` and `parse/2` predicates operating
on complete BSON documents represented as byte lists:

	| ?- bson::generate({name-alice, active - @true}, Bytes).
	Bytes = [30,0,0,0,2,110,97,109,101,0,6,0,0,0,97,108,105,99,101,0,8,97,99,116,105,118,101,0,1,0]
	yes

	| ?- bson::parse([12,0,0,0,16,110,0,42,0,0,0,0], Document).
	Document = {n-int32(42)}
	yes

Parsing rejects malformed lengths, terminators, UTF-8, array keys, Boolean
bytes, type codes, subtypes, regular-expression options, fixed-width payloads,
and JavaScript-with-scope lengths. The MongoDB 16 MiB server document limit is
not a BSON format limit and is therefore not imposed by this library.

For untrusted input, prefer `bson(codes)` or `bson(chars)` to avoid interning
arbitrary decoded strings as atoms.
