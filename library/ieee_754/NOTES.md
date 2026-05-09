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


`ieee_754`
==========

The `ieee_754` library is a support package for parsing and generating IEEE
754 floating-point encodings shared by binary interchange libraries such as
`message_pack`, `avro`, `protobuf`, `cbor`, and `wkt_wkb`.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(ieee_754(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

    | ?- logtalk_load(ieee_754(tester)).


Object model
------------

The library provides two parameterized implementation objects:

	`ieee_754(Precision, ByteOrder, NaNRepresentation)`

  `ieee_754_fields(Precision, ByteOrder)`

with the following parameter values:

- `Precision`: `half`, `single`, or `double`
- `ByteOrder`: `big` or `little`
- `NaNRepresentation`: `canonical` or `payloads`

The parameterization keeps format-specific policy out of the shared core.
Advanced callers can use `ieee_754_fields` to inspect exact sign, exponent,
mantissa, finite binary-rational decompositions, and NaN payload bits
without going through backend float values.


Value representation
--------------------

The intended term representation is:

- finite values: backend Prolog floats
- positive infinity: `@infinity`
- negative infinity: `@negative_infinity`
- canonical NaN: `@not_a_number`
- payload-preserving NaN values: `not_a_number(Bytes)`

The `not_a_number(Bytes)` representation is only intended to be accepted and
produced by objects configured with `NaNRepresentation = payloads`.

Under the `payloads` policy, decoding a canonical quiet NaN should still yield
`@not_a_number`. Non-canonical NaN encodings should decode to
`not_a_number(Bytes)` using canonical byte order for the selected precision.

The high-level `ieee_754/3` object API deliberately abstracts away the IEEE 754
quiet-versus-signaling NaN distinction. In `canonical` mode, all NaN encodings
decode to `@not_a_number`. In `payloads` mode, the designated canonical quiet
NaN encoding decodes to `@not_a_number` while all other NaN encodings decode to
`not_a_number(Bytes)` for roundtrip preservation. Callers that need exact NaN
inspection should use `ieee_754_fields/2` object API.


Public API
----------

The high-level value API is defined in `ieee_754_protocol.lgt` and consists of:

- `parse/2`
  Decodes `bytes(Bytes)` or `bits(Bits)` into a value term.

- `generate/2`
  Encodes a value term into `bytes(Bytes)` or `bits(Bits)`.

- `generate/3`
  Encodes a value term to a byte list with an open tail for difference-list
  based binary generators.

- `valid/1`
  Checks whether a term is a valid value representation for the selected
  object.

- `exactly_representable/1`
  Checks whether a term can be encoded and decoded in the selected precision
  without loss of value information.

- `precision/1`
  Returns the selected precision.

- `order/1`
  Returns the selected byte order.

- `nan_representation/1`
  Returns the selected NaN representation policy.

- `byte_count/1`
  Returns the number of bytes used by the selected precision.

The low-level exact field API is defined in `ieee_754_fields_protocol.lgt`
and consists of:

- `classify/2`
  Classifies `bytes(Bytes)` or `bits(Bits)` as `zero`, `subnormal`,
  `normal`, `infinity`, or `not_a_number`.

- `fields/5`
  Extracts the exact sign bit, exponent bits, mantissa bits, and class.

- `finite_binary_rational/4`
  Extracts finite encodings as exact `(-1)^Sign * Significand * 2^Exponent`
  terms.

- `nan_payload/2`
  Extracts the raw NaN mantissa payload bits.

- `nan_kind/2`
  Classifies a NaN encoding as `quiet` or `signaling`.

- `precision/1`
  Returns the selected precision.

- `order/1`
  Returns the selected byte order.

- `byte_count/1`
  Returns the number of bytes used by the selected precision.


Semantics
---------

The intended shared semantics are:

- finite encoding rounds according to the selected IEEE 754 precision
- `exactly_representable/1` succeeds only when no value information is lost
- signed zero is preserved when supported by the backend Prolog compiler
- infinities and NaN values are represented independently from backend float
  syntax
- subnormal values are supported for all selected precisions
- payload-preserving NaN decoding is available when explicitly requested
- the `ieee_754_fields` API works from exact encodings and therefore does not
  depend on backend float decomposition behavior
- exact quiet/signaling NaN inspection is exposed by `ieee_754_fields/2`
  instead of the high-level value codec API
