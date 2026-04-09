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


`mime_types`
============

The `mime_types` library provides convenience predicates for mapping file
extensions, file names, and URL-like resources to MIME media types and content
encodings.

The current implementation ships with a built-in registry containing a practical
set of common standard mappings plus a smaller set of de-facto common mappings.
Additional mappings can be loaded at runtime from `mime.types`-style files.

Lookup is lenient by default. The convenience predicates also accept a boolean
`Strict` argument to restrict lookup to strict runtime overlays plus the built-in
standard registry.

The library uses the `os` library `decompose_file_name/4` predicate to split
paths, and recognizes common compound suffixes such as `.tgz` and `.svgz`.


API documentation
-----------------

Open the [../../apis/library_index.html#mime_types](../../apis/library_index.html#mime_types)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(mime_types(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(mime_types(tester)).


Usage
-----

Guess the MIME type and content encoding for a file name:

	| ?- mime_types::guess_file_type('archive.tgz', Type, Encoding).
	Type = 'application/x-tar',
	Encoding = gzip
	yes

Lookup a preferred extension for a MIME type:

	| ?- mime_types::guess_extension('application/json', Extension).
	Extension = '.json'
	yes

Read a `mime.types`-style file without mutating the runtime registry:

	| ?- mime_types::read_mime_types('mime.types', Pairs).

Load additional runtime mappings from a `mime.types`-style file:

	| ?- mime_types::load('mime.types').
