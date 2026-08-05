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


`tries`
=======

This library provides a persistent trie (also known as a prefix tree) that
associates complete strings with values. The supported string representations
are atoms, lists of character codes, and lists of characters, selected using
the `trie(atom)`, `trie(codes)`, and `trie(chars)` parametric objects. Trie
representations are opaque terms and should only be accessed using the library
predicates.


API documentation
-----------------

Open the [../../apis/library_index.html#tries](../../apis/library_index.html#tries)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(tries(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(tries(tester)).


Usage
-----

Select the parametric object matching the desired string representation. For
example, to create an atom-based trie from a list of string-value pairs:

	| ?- trie(atom)::as_trie([
	        ''-default_handler,
	        '/users'-users_handler,
	        '/users/me'-profile_handler,
	        '/usage'-usage_handler
	     ], Trie).
	Trie = ...
	yes

The string in each pair is the complete stored string. The value is arbitrary
application data associated with that string. Individual characters and
internal trie nodes are not exposed by the API.

For autocomplete-style queries, `lookup_prefix/4` enumerates all stored
strings beginning with a prefix in lexicographic order:

	| ?- trie(atom)::lookup_prefix('/us', String, Handler, Trie).
	String = '/usage',
	Handler = usage_handler ;
	String = '/users',
	Handler = users_handler ;
	String = '/users/me',
	Handler = profile_handler ;
	no

The `as_list/3`, `strings/3`, and `values/3` predicates provide materialized
versions of the same prefix-restricted traversal.

A more traditional trie use case is a dictionary of words. For example, long
words with a common stem can be stored with their definitions and efficiently
enumerated from a partial spelling:

	| ?- trie(atom)::as_trie([
	        electroencephalogram-'record of brain electrical activity',
	        electroencephalograph-'instrument for recording brain electrical activity',
	        electroencephalographic-'relating to electroencephalography',
	        electroencephalography-'recording of brain electrical activity'
	     ], Trie),
	     trie(atom)::lookup_prefix(
	        electroencephalogra, Word, Definition, Trie
	     ).
	Word = electroencephalogram,
	Definition = 'record of brain electrical activity' ;
	Word = electroencephalograph,
	Definition = 'instrument for recording brain electrical activity' ;
	Word = electroencephalographic,
	Definition = 'relating to electroencephalography' ;
	Word = electroencephalography,
	Definition = 'recording of brain electrical activity' ;
	no

For routing-style queries, `longest_prefix/4` finds the most specific stored
string that prefixes a query:

	| ?- trie(atom)::longest_prefix(
	        Trie, '/users/me/settings', Prefix, Handler
	     ).
	Prefix = '/users/me',
	Handler = profile_handler
	yes

The `prefixes/3` predicate returns all matching stored prefixes from shortest
to longest. The empty string is a valid stored string and can therefore be
used as a default route.

All updates are persistent: insertion, update, exact deletion, and prefix
deletion return a new trie and leave the original trie unchanged. Exact
deletion preserves longer strings that share the deleted string as a prefix;
`delete_prefix/3` removes the complete matching subtree.
