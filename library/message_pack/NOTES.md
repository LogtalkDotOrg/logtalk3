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


`message_pack`
==============

The `message_pack` library provides predicates for parsing and generating data
in the [MessagePack](https://msgpack.org/) format based on the current
specification and reference documents found at:

- https://github.com/msgpack/msgpack/blob/master/spec.md
- https://github.com/msgpack/msgpack/blob/master/spec-old.md

It follows the representation conventions used by the existing binary and JSON
libraries whenever practical.


API documentation
-----------------

Open the [../../apis/library_index.html#message_pack](../../apis/library_index.html#message_pack)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(message_pack(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(message_pack(tester)).


Representation
--------------

The following choices of syntax have been made to represent MessagePack
elements as terms:

- Maps are represented using curly-bracketed terms, `{Pairs}`, where each pair
uses the representation `Key-Value`.

- Arrays are represented using lists.

- Strings can be represented as atoms, `chars(List)`, or `codes(List)`. Use
the `message_pack/1` parameterized object with the parameter bound to `atom`,
`chars`, or `codes` to select the representation used when decoding strings.

- MessagePack strings are validated using strict UTF-8 decoding and encoding.
Invalid UTF-8 byte sequences and invalid Unicode scalar values are rejected.

- Binary data is represented by `bytes(List)`.

- Extension values are represented by `ext(Type, bytes(Bytes))`, where `Type`
is the signed 8-bit extension type and `Bytes` is the extension payload.

- The MessagePack values `false`, `true`, and `nil` are represented by,
respectively, the `@false`, `@true`, and `@null` compound terms.

- IEEE 754 infinities and NaN values are represented by the `@infinity`,
`@negative_infinity`, and `@not_a_number` compound terms.

The following table exemplifies the term equivalents of common MessagePack
elements when using `message_pack(atom)`:

|    MessagePack value          |    term                       |
| :---------------------------: | :---------------------------: |
|    nil                        |    @null                      |
|    false                      |    @false                     |
|    true                       |    @true                      |
|    "foo"                      |    foo                        |
|    [1,2,3]                    |    [1,2,3]                    |
|    {"a":1,"b":true}           |    {a-1, b-@true}             |
|    bin payload                |    bytes([...])               |
|    extension payload          |    ext(Type, bytes([...]))    |


Notes
-----

- Generated integers always use the smallest valid MessagePack integer format.

- Generated floating point values use the single-precision format when it can
exactly represent the input value; otherwise, the double-precision format is
used.

- Recommended string representation choice depends on the trust boundary of the
input. For trusted input, `message_pack(atom)` is usually the most convenient
choice when the rest of the application already works with atoms. For
untrusted input, prefer `message_pack(codes)` or `message_pack(chars)` to
avoid interning arbitrary incoming strings as atoms.

- For untrusted input, `message_pack(codes)` is generally the best default
choice. It avoids atom creation while preserving the exact decoded Unicode code
points. `message_pack(chars)` provides the same safety with a more readable but
less compact representation.

- The predefined timestamp extension is currently exposed using the generic
`ext(-1, bytes(Bytes))` representation.

- The library requires a backend Prolog compiler with unbounded integer
support, matching the requirements of the existing `cbor`, `avro`, and
`protobuf` libraries.
