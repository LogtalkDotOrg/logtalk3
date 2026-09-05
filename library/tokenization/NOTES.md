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


`tokenization`
==============

This library provides extensible tokenization and sentence splitting for
atoms, character lists, and character code lists. A representation-aware
facade delegates canonical character-code lists to a language provider. The
included English provider uses portable deterministic rules for words,
contractions, hyphenated words, abbreviations, numbers, email addresses,
punctuation, and sentence boundaries. URL and email address recognition
delegates to the `url` library for parsing and validation.


API documentation
-----------------

Open the [../../apis/library_index.html#tokenization](../../apis/library_index.html#tokenization)
link in a web browser.


Loading
-------

To load the library and the English provider:

	| ?- logtalk_load(tokenization(loader)).


Testing
-------

To test this library predicates:

	| ?- logtalk_load(tokenization(tester)).


Usage
-----

The `tokenizer(Representation, Language)` object accepts `atom`, `chars`, and
`codes` representations. The language parameter must be an object implementing
the `tokenizer_language_protocol` protocol.

	| ?- tokenizer(atom, english_tokenizer)::tokenize(
	         'Mr. Smith bought a new car for $12,500. Isn\'t that great?',
	         Tokens
	     ).
	Tokens = ['Mr.', 'Smith', bought, a, new, car, for, '$', '12,500', '.', 'Isn\'t', that, great, '?']
	yes

	| ?- tokenizer(atom, english_tokenizer)::tokenize(
	         'Hello, world!', Tokens, [keep_punctuation(false)]
	     ).
	Tokens = ['Hello', world]
	yes

Sentence splitting is available independently or combined with tokenization:

	| ?- tokenizer(atom, english_tokenizer)::split_sentences(
	         'Dr. Brown arrived at 3.14 p.m. She was late. Really?', Sentences
	     ).
	Sentences = ['Dr. Brown arrived at 3.14 p.m.', 'She was late.', 'Really?']
	yes

	| ?- tokenizer(atom, english_tokenizer)::tokenize_sentences(
	         'She was late. Really?', Sentences
	     ).
	Sentences = [['She', was, late, '.'], ['Really', '?']]
	yes


Options
-------

The `tokenize/3`, `split_sentences/3`, and `tokenize_sentences/3` predicates
accept these options:

- `keep_punctuation(Boolean)` controls emission of standalone punctuation
  tokens. The default is `true`. Punctuation internal to abbreviations,
  numbers, URLs, email addresses, contractions, and hyphenated words is kept.
- `lowercase(Boolean)` controls lowercasing of emitted tokens. The default is
  `false`.
- `normalize_quotes(Boolean)` maps common Unicode single and double quotation
  marks to ASCII quotes before processing. The default is `true`.
- `normalize_dashes(Boolean)` maps common Unicode dash characters to the ASCII
  hyphen before processing. The default is `true`.


Pipeline integration
--------------------

Token lists can be passed directly to the other text and NLP libraries:

	text_to_trigrams(Text, Trigrams) :-
		tokenizer(atom, english_tokenizer)::tokenize(
			Text, Tokens0, [keep_punctuation(false), lowercase(true)]
		),
		stop_words(atom, stopwords_en)::exclude(Tokens0, Tokens1),
		lemmatizer(atom, english_lemmatizer)::lemmas(Tokens1, Tokens2),
		n_grams(atom)::trigrams(Tokens2, Trigrams).


Adding providers
----------------

Providers operate on character-code lists and receive validated, merged
options. A provider can implement `tokenizer_language_protocol` directly:

	:- object(example_tokenizer,
		implements(tokenizer_language_protocol)).

		tokenize_codes(Codes, [Codes], _).
		split_sentence_codes(Codes, [Codes], _).

	:- end_object.

Rule-based language providers can instead import the `tokenizer_rules`
category and define its protected abbreviation and internal punctuation hooks,
as illustrated by the `english_tokenizer` object. The provider protocol does
not otherwise constrain the implementation; finite-state, statistical,
cached, dynamically loaded, and external tokenizers can use the same facade.


English coverage
----------------

The English provider includes common honorifics and ranks, month names,
academic and professional credentials, organization and editorial forms,
geographic initialisms, time abbreviations, and common Latin abbreviations.
Titles and ranks are treated as non-terminal when followed by more text;
other abbreviations use the following token and surrounding context to decide
whether a period ends a sentence.


Limitations
-----------

The included rules are practical heuristics, not a complete linguistic model.
They do not implement Unicode normalization or Unicode text segmentation,
emoji grapheme clustering, social-media conventions, typed tokens, or
multi-word expression recognition. Non-ASCII characters not recognized as
supported punctuation are conservatively treated as word characters.
