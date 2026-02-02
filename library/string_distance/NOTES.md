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


`string_distance`
=================

This library provides string distance predicates with support for different
string representations: atoms, character lists, or character code lists.

The predicates are defined in the `string_distance(_Representation_)`
parametric object where `_Representation_` can be one of:

- `atom` - strings are represented as atoms
- `chars` - strings are represented as lists of characters
- `codes` - strings are represented as lists of character codes

The parameter must be bound when sending messages to the object.


API documentation
-----------------

Open the [../../apis/library_index.html#string_distance](../../apis/library_index.html#string_distance)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(string_distance(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(string_distance(tester)).


Algorithms
-----------

This library implements the following string distance algorithms:

- **Levenshtein** distance is probably the most well-known. It counts the minimum number of single-character edits — insertions, deletions, or substitutions — needed to transform one string into another. For example, the distance between "kitten" and "sitting" is 3.

- **Damerau-Levenshtein** distance extends Levenshtein by also allowing transpositions of two adjacent characters as a single edit operation. This makes it more practical for catching common typos like "ab" -> "ba".

- **Hamming** distance only works on strings of equal length and counts the number of positions where the corresponding characters differ. It's very fast but limited by that equal-length constraint, making it useful for things like comparing binary codes or fixed-format identifiers.
  
- **Jaro and Jaro-Winkler** distance produce a similarity score between 0 and 1 rather than a raw edit count. Jaro considers matching characters and transpositions, while Jaro-Winkler adds a prefix-weighting boost — the idea being that strings matching at the start are more likely to be the same (useful for name matching).

- **Edit similarity** is a normalized version of the edit distance, defined as 1 - (edit distance / max length of the two strings). It produces a similarity score between 0 and 1. Can be computed from Levenshtein, Damerau-Levenshtein, Hamming, or Longest Common Subsequence distances.

- **Longest Common Subsequence** (LCS) finds the length of the longest sequence of characters that appears in both strings in the same relative order (not necessarily contiguous). It's heavily used in diff tools for comparing files.

- **Longest Common Substring** is similar to LCS but requires the shared characters to be contiguous. Useful when caring about shared blocks of text rather than scattered matches.

- **Cosine Similarity** shifts the approach from character edits to vector-based comparison. Strings are converted into vectors (often using character n-grams or word tokens), and then the cosine of the angle between them is computed. It's widely used in NLP and information retrieval.

- **Jaccard Index** compares two sets of tokens (often words or n-grams) by dividing the size of their intersection by the size of their union. Simple and intuitive, though it ignores term frequency.

- **Soundex/Metaphone** are phonetic algorithms rather than strict string comparisons. They encode strings based on how they sound, so "Smith" and "Smythe" would match. Useful for name deduplication where spelling varies but pronunciation is similar.

General advice: use Levenshtein or Damerau-Levenshtein for general-purpose edit distance, Jaro-Winkler for short strings like names, LCS for diff-style comparisons, and cosine/Jaccard when working at the word or document level rather than character level.
