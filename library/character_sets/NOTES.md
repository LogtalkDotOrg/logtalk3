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


`character_sets`
================

This library provides a `character_set_protocol` protocol plus concrete
objects for converting between lists of character codes and lists of bytes.
It also provides metadata predicates `preferred_mime_name/1`, `name/1`,
`alias/1`, and `mibenum/1` based on the IANA character set registry:

https://www.iana.org/assignments/character-sets/character-sets.xhtml

The currently provided objects are:

* `us_ascii`
* `iso_8859_1`
* `iso_8859_2`
* `iso_8859_3`
* `iso_8859_4`
* `iso_8859_9`
* `iso_8859_10`
* `iso_8859_13`
* `iso_8859_14`
* `iso_8859_15`
* `iso_8859_16`
* `windows_1250`
* `windows_1251`
* `windows_1252`
* `windows_1253`
* `windows_1254`
* `windows_1257`
* `utf_8`
* `utf_16le`
* `utf_16be`
* `utf_32le`
* `utf_32be`

Object names are derived from the preferred IANA MIME names by lowercasing
them and replacing hyphens with underscores. When a registry entry has no
distinct preferred MIME alias, the registered IANA name is used instead.
A compatibility alias object named `utf16be` is also provided for `utf_16be`.

The Unicode character set objects work with Unicode scalar values and do not
emit or consume a byte order mark (BOM).

This library intentionally does not currently provide `Shift_JIS` or
`GB18030` objects because portable mapping tables for those multibyte
encodings are not yet included.

No input validation is performed when converting between character codes
and bytes. When necessary, use the `types` library validation and checking
predicates before calling the `codes_to_bytes/2` and `bytes_to_codes/2`
predicates.


API documentation
-----------------

Open the [../../apis/library_index.html#character_sets](../../apis/library_index.html#character_sets)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(character_sets(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(character_sets(tester)).


Usage
-----

The UTF, ISO 8859, and Windows character set objects are grouped in three
main files:

- `utf_character_sets.lgt`
- `iso_8859_character_sets.lgt`
- `windows_character_sets.lgt`

This allows some customization of the character set objects loaded by your
application. Note that the `character_set_protocol.lgt` and `character_sets.lgt`
base files must always be loaded (they include the `us_ascii` character set,
which is thus always loaded).
