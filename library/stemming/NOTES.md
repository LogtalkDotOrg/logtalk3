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


`stemming`
==========

This library provides word stemming predicates for English text, with support
for different word representations: atoms, character lists, or character code
lists.

The library includes implementations of two well-known stemming algorithms:

- **Porter Stemmer** - The Porter stemming algorithm (Porter, 1980) is a widely
  used algorithm for reducing English words to their root form by applying a
  series of rules that remove common suffixes.

- **Lovins Stemmer** - The Lovins stemming algorithm (Lovins, 1968) removes the
  longest suffix from a word using a list of endings, each associated with a
  condition for removal. It then applies transformation rules to fix spelling.


API documentation
-----------------

Open the [../../apis/library_index.html#stemming](../../apis/library_index.html#stemming)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(stemming(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(stemming(tester)).


Usage
-----

The stemming predicates are defined in parametric objects where the parameter
specifies the word representation:

- `atom` - words are represented as atoms
- `chars` - words are represented as lists of characters
- `codes` - words are represented as lists of character codes

The parameter must be bound when sending messages to the objects.


### Porter Stemmer

To stem a single word using atoms:

	| ?- porter_stemmer(atom)::stem(running, Stem).
	Stem = run
	yes

To stem a list of words:

	| ?- porter_stemmer(atom)::stems([running, walks, easily], Stems).
	Stems = [run, walk, easili]
	yes

Using character lists:

	| ?- porter_stemmer(chars)::stem([r,u,n,n,i,n,g], Stem).
	Stem = [r,u,n]
	yes


### Lovins Stemmer

To stem a single word using atoms:

	| ?- lovins_stemmer(atom)::stem(running, Stem).
	Stem = run
	yes

To stem a list of words:

	| ?- lovins_stemmer(atom)::stems([running, walks, easily], Stems).
	Stems = [run, walk, eas]
	yes


Algorithms
----------

### Porter Stemmer

The Porter stemming algorithm, developed by Martin Porter in 1980, is one of
the most widely used stemming algorithms for the English language. It operates
through a series of steps that progressively remove suffixes from words:

1. **Step 1a**: Handle plurals (e.g., "caresses" → "caress", "ponies" → "poni")
2. **Step 1b**: Handle past tense and progressive forms (e.g., "agreed" → "agree")
3. **Step 1c**: Replace terminal "y" with "i" when preceded by a vowel
4. **Steps 2-4**: Remove various suffixes based on the "measure" of the stem
5. **Step 5**: Clean up final "e" and double consonants

The algorithm uses the concept of "measure" (m), which counts vowel-consonant
sequences in the stem, to determine when suffixes can be safely removed.

**Reference**: Porter, M.F. (1980). An algorithm for suffix stripping.
Program, 14(3), 130-137.


### Lovins Stemmer

The Lovins stemming algorithm, developed by Julie Beth Lovins in 1968, was one
of the earliest stemming algorithms. It takes a different approach from Porter:

1. **Ending removal**: The algorithm maintains a list of 294 possible endings,
   ordered by length. It removes the longest matching ending that satisfies its
   associated condition (e.g., minimum stem length).

2. **Transformation rules**: After removing the ending, spelling transformations
   are applied to fix common irregularities (e.g., "iev" → "ief", "uct" → "uc").

The Lovins algorithm tends to be more aggressive than Porter, sometimes
producing stems that are not actual words but are consistent across related
word forms.

**Reference**: Lovins, J.B. (1968). Development of a stemming algorithm.
Mechanical Translation and Computational Linguistics, 11(1-2), 22-31.


Choosing an Algorithm
---------------------

- **Porter**: More conservative, produces more readable stems, widely used in
  information retrieval and search applications. Good choice for most applications.

- **Lovins**: More aggressive, may conflate more word forms together. Can be
  useful when broader matching is desired, but may over-stem in some cases.

Both algorithms are designed for English text only.


Known Limitations
-----------------

- Both algorithms work only with English words.
- Stemming is not lemmatization - stems may not be valid dictionary words.
- Proper nouns and abbreviations may not be handled correctly.
- Very short words (1-2 characters) are returned unchanged.
