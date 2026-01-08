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


`toon`
======

The `toon` library provides predicates for parsing and generating data in
the TOON (Token-Oriented Object Notation) format. TOON is a compact,
human-readable, line-oriented format that encodes the JSON data model
while minimizing tokens. For more information on the TOON format, see:

https://github.com/toon-format/toon


API documentation
-----------------

Open the [../../apis/library_index.html#toon](../../apis/library_index.html#toon)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

    | ?- logtalk_load(toon(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

    | ?- logtalk_load(toon(tester)).


Representation
--------------

TOON objects are represented using the same conventions as the `json`
library. The representation of TOON objects and TOON pairs can be
controlled using the `toon/3` parametric object. The three parameters
are:

- `ObjectRepresentation`: `curly` (default) or `list`
- `PairRepresentation`: `dash` (default), `equal`, or `colon`
- `StringRepresentation`: `atom` (default), `chars`, or `codes`

The `toon(StringRepresentation)` and `toon` objects use the default
representations.

### Objects

TOON objects are represented using curly terms by default:

    {Key1-Value1, Key2-Value2, ...}

Or using lists when using `list` for the `ObjectRepresentation` parameter:

    toon([Key1-Value1, Key2-Value2, ...])

### Arrays

TOON arrays are represented as lists:

    [Element1, Element2, ...]

### Primitives

TOON primitive values are represented as follows:

- Strings: atoms (default), `chars(ListOfChars)`, or `codes(ListOfCodes)`
- Numbers: Prolog numbers
- Booleans: `@true` and `@false`
- Null: `@null`


TOON format features
--------------------

TOON is a line-oriented format with the following key features:

- Uses indentation (2 spaces) instead of braces for objects
- Arrays declare their length: `[N]:` for inline or `[N]{fields}:` for tabular
- Tabular form for uniform arrays of objects with primitive values
- Minimal quoting rules for strings
- Three delimiters: comma (default), tab, and pipe
- UTF-8 encoding with LF line endings
- File extension: `.toon`, media type: `text/toon`


Usage examples
--------------

Parsing TOON from an atom:

    | ?- toon::parse(atom('name: John\nage: 30'), Term).
    Term = {name-'John', age-30}
    yes

Generating TOON to an atom:

    | ?- toon::generate(atom(Atom), {name-'John', age-30}).
    Atom = 'name: John\nage: 30'
    yes

Parsing TOON from a file:

    | ?- toon::parse(file('data.toon'), Term).
    ...

Generating TOON to a file:

    | ?- toon::generate(file('output.toon'), {name-'John', age-30}).
    ...

Using different representations:

    | ?- toon(list, dash, atom)::parse(atom('name: John'), Term).
    Term = toon([name-'John'])
    yes
