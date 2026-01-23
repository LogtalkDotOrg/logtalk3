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


`strings`
=========

This library provides string manipulation predicates with support for
different string representations: atoms, character lists, or character
code lists. Its API is based on work and libraries found in ECLiPSe and
SWI-Prolog.

The predicates are defined in the `string(_Representation_)` parametric
object where `_Representation_` can be one of:

- `atom` - strings are represented as atoms
- `chars` - strings are represented as lists of characters
- `codes` - strings are represented as lists of character codes

The parameter must be bound when sending messages to the object.


API documentation
-----------------

Open the [../../apis/library_index.html#strings](../../apis/library_index.html#strings)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(strings(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(strings(tester)).


Predicates
----------

The library provides the following predicates:

- `atom_string/2` - converts between atoms and strings
- `number_string/2` - converts between numbers and strings
- `string_chars/2` - converts between strings and character lists
- `string_codes/2` - converts between strings and character code lists
- `string_concat/3` - concatenates two strings
- `string_length/2` - returns the length of a string
- `sub_string/5` - extracts substrings
- `string_upper/2` - converts a string to uppercase
- `string_lower/2` - converts a string to lowercase
- `split_string/4` - splits a string into substrings using separators and padding
- `atomics_to_string/2` - concatenates a list of atomic terms into a string
- `atomics_to_string/3` - concatenates a list of atomic terms into a string with separator

For converting between terms and strings, see the `term_io` library.
