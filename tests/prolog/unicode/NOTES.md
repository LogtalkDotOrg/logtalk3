________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>  
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


This directory contains *work-in-progress* test sets for Prolog Unicode
support. Currently, three test sets are provided: `builtins` (for flags,
built-in predicates, and stream properties), `encodings` (for UTF-8,
UTF-16, and UTF-32 encodings, with and without a BOM), and `syntax` (for
the `\uXXXX` and `\UXXXXXXXX` escape sequences). The `encodings` test set
is only enabled for backends supporting all the above encodings (currently,
CxProlog and SICStus Prolog).

The tests are based on an extended version of the October 5, 2009 WG17 ISO
Prolog Core revision standardization proposal, which specifies the following
minimal language features:

1. An `encoding` Prolog flag, allowing applications to query the default
encoding for opening streams. When the Prolog systems supports multiple
encodings, the default encoding can be changed by setting this flag to a
supported encoding.

2. Encodings are represented by atoms after the names specified by the
Internet Assigned Numbers Authority (IANA) and marked as the "(preferred
MIME name)" alias when available:

	http://www.iana.org/assignments/character-sets

For example, `'UTF-8'`, `'UTF-16LE'`, or `'UTF-32'`.

3. Two new `open/4` predicate options, `encoding(Atom)` and `bom(Boolean)`.
The handling of these options depends on the mode argument, only applies to
text files, and follows from the Unicode standard guidelines and current
practice:

- `write` mode: If an `encoding/1` option is present, use the specified
encoding, otherwise use the default encoding (which can be queried using
the `encoding` flag). If `bom(true)` option is present, write a BOM if the
encoding is a Unicode encoding. If no `bom/1` option is used, write a BOM
if the encoding is `UTF-16` or `UTF-32` but not if the encoding is `UTF-8`,
`UTF-16LE`, `UTF-16BE`, `UTF-32LE`, or `UTF-32LE`. If the encoding is
`UTF-16` or `UTF-32`, write the data big-endian.

- `append` mode: If an `encoding/1` option is present, use that encoding,
otherwise use the default encoding (which can be queried using the
`encoding` flag). Ignore `bom/1` option if present and never write a BOM.

- `read` mode: the default is `bom(true)`, i.e. perform BOM detection and use
the corresponding encoding if a BOM is found. If no BOM is detected, then use
the `encoding/1` option if present and the default encoding otherwise. When a
`bom(false)` option is present, no BOM detection is performed, an `encoding/1`
is required if the file encoding is different from the default encoding, and
a BOM at the beginning of the stream is to be interpreted as a ZERO WIDTH
NON-BREAKING SPACE (ZWNBSP).

The `bom/1` option is ignored when not using a Unicode encoding.

4. The `open/3` predicate (for text files) always perform BOM detection on mode
`read` and uses the corresponding encoding if a BOM is found. Otherwise the
default encoding is used (which can be queried using the `encoding` flag).
In `write` mode, a BOM is written if the encoding is `UTF-16` or `UTF-32` but
not if the encoding is `UTF-8`, `UTF-16LE`, `UTF-16BE`, `UTF-32LE`, or
`UTF-32LE`. If the encoding is `UTF-16` or `UTF-32`, the data is written
big-endian.

5. Two new stream properties, `encoding(Atom)` and `bom(Boolean)`, set from
the `open/3-4` calls and the default values as described above, that can be
queried using the standard `stream_property/2` predicate.

6. The standard built-in predicates that must be Unicode aware include:

- `atom_chars/2`
- `atom_codes/2`
- `atom_concat/3`
- `atom_length/2`
- `char_code/2`
- `current_prolog_flag/2`
- `get_char/1-2`
- `get_code/1-2`
- `open/3-4`
- `peek_char/1-2`
- `peek_code/1-2`
- `put_char/1-2`
- `put_code/1-2`
- `read_term/3`
- `set_prolog_flag/2`
- `set_stream_position/2`
- `stream_property/2`
- `sub_atom/5`
- `write_term/3`
