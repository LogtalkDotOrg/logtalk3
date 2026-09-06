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


`text_normalization`
====================

This library provides extensible Unicode aware text cleaning and normalization
for atoms, character lists, and character code lists. It implements Unicode
17.0.0 normalization and default casing independently of backend-specific
Unicode normalization predicates.


API documentation
-----------------

Open the [../../apis/library_index.html#text-normalization](../../apis/library_index.html#text-normalization)
link in a web browser.


Loading
-------

To load the library:

	| ?- logtalk_load(text_normalization(loader)).


Testing
-------

To test this library predicates:

	| ?- logtalk_load(text_normalization(tester)).

To run the optimized Unicode conformance tests:

	| ?- logtalk_load(text_normalization(tester_unicode_conformance)).

The conformance test suite checks all 20,034 vectors in Unicode 17.0.0
`NormalizationTest.txt` and all 1,585 full default case-fold mappings.


Usage
-----

The `text_normalizer(Representation, Profile)` object accepts the
representations `atom`, `chars`, and `codes`. The profile must implement the
`text_normalization_profile_protocol` protocol. The bundled
`default_text_normalization` profile is language-neutral.

	| ?- text_normalizer(codes, default_text_normalization)::normalize_unicode(
	         nfd, [197], Normalized
	     ).
	Normalized = [65, 778]
	yes

	| ?- text_normalizer(codes, default_text_normalization)::case_fold(
	         [83, 116, 114, 97, 223, 101], Folded
	     ).
	Folded = [115, 116, 114, 97, 115, 115, 101]
	yes

	| ?- text_normalizer(atom, default_text_normalization)::decode_entities(
	         '&lt;A&amp;B&gt;', Decoded
	     ).
	Decoded = '<A&B>'
	yes

	| ?- text_normalizer(atom, default_text_normalization)::clean(
	         '  FOO&nbsp;BAR  ', Cleaned,
	         [case(lower)]
	     ).
	Cleaned = 'foo bar'
	yes


Operations
----------

`normalize_unicode/3` implements NFC, NFD, NFKC, and NFKD, including
algorithmic Hangul decomposition and composition. `case_fold/2` implements
full default Unicode folding. Lowercase and uppercase conversion use default
Unicode mappings and unconditional special casing; lowercase conversion also
implements the language-independent final-sigma rule. `title_case/2` uses a
simple boundary definition: a non-cased, non-case-ignorable character starts a
new word.

`remove_diacritics/2` performs canonical decomposition, removes every Unicode
Mark-category character, and returns NFC. `fold_diacritics/2` additionally
applies profile mappings for characters such as `ø` and `ł` that do not have a
canonical decomposition. Mark removal and transliteration are intentionally
lossy operations and are distinct from Unicode normalization.

`decode_entities/2-3` accepts semicolon-terminated decimal and hexadecimal
numeric references and the case-sensitive names supplied by the profile. The
default profile includes XML's five predefined entities and all the
semicolon-terminated named character references defined by WHATWG HTML.


Options
-------

`decode_entities/3` accepts `unknown(preserve|error)`, defaulting to
`preserve`.

`normalize_whitespace/3` accepts:

- `trim(Boolean)`, defaulting to `true`.
- `collapse(none|horizontal|all)`, defaulting to `all`.
- `line_endings(lf|crlf|cr|preserve)`, defaulting to `lf`.
- `controls(preserve|remove)`, defaulting to `preserve`.

`clean/3` accepts:

- `unicode(nfc|nfd|nfkc|nfkd)`, defaulting to `nfc`.
- `entities(Boolean)`, defaulting to `true`.
- `unknown_entities(preserve|error)`, defaulting to `preserve`.
- `case(preserve|fold|lower|upper|title)`, defaulting to `preserve`.
- `diacritics(none|remove|fold)`, defaulting to `none`.
- `whitespace(Boolean)`, defaulting to `true`.
- The four whitespace options listed above.

Duplicate options and options that do not apply to the called predicate are
rejected. The fixed clean pipeline decodes entities, converts case, handles
diacritics, applies the requested final Unicode normalization, and finally
normalizes whitespace.


Adding profiles
---------------

A custom profile can extend `default_text_normalization` and override only the
required policy hooks while inheriting built-in mappings:

	:- object(turkic_text_normalization,
		extends(default_text_normalization)).

		case_conversion(lower, [73], [305]) :-
			!.
		case_conversion(Mode, Codes, Converted) :-
			^^case_conversion(Mode, Codes, Converted).

		diacritic_fold(216, [79]) :-
			!.
		diacritic_fold(Code, Folded) :-
			^^diacritic_fold(Code, Folded).

		named_entity(example, [88]) :-
			!.
		named_entity(Name, Codes) :-
			^^named_entity(Name, Codes).

	:- end_object.

Unicode normalization itself is deliberately not overridable. Profile case
overrides receive and return complete code lists, while diacritic and entity
hooks operate on numeric code points.

Spelling correction is outside the core facade because it requires an
application-specific language model or dictionary. Providers can implement the
separate `spelling_normalizer_protocol` protocol and compose correction with
this library.


Unicode data and portability
----------------------------

The Unicode 17.0.0 normalization, casing, character-property data, and
normalization conformance vectors are owned by the `unicode_data` library and
loaded as a dependency. The static XML and WHATWG HTML entity table remains
local to this library and contains only ASCII source text and numeric character
values.

The `codes` representation supports every Unicode scalar value on all
supported backends. The `atom` and `chars` representations can only round-trip
characters supported by the backend's native character representation; use
`codes` when processing the complete scalar range portably.

The library implements Unicode normalization and default casing, not grapheme
segmentation, locale negotiation, encoding conversion, or full HTML parsing.
Use `character_sets` for encoding conversion and `tokenization` for downstream
text segmentation.
