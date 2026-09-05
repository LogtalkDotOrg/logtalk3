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


`lemmatization`
===============

This library provides extensible word lemmatization for atoms, character
lists, and character code lists. A representation-aware facade delegates
canonical lowercase atoms to a language provider. The included English
provider uses curated exceptions and base forms together with conservative,
lexicon-validated inflection rules.


API documentation
-----------------

Open the [../../apis/library_index.html#lemmatization](../../apis/library_index.html#lemmatization)
link in a web browser.


Loading
-------

To load the library main files and the English language provider, load the
`loader.lgt` file:

	| ?- logtalk_load(lemmatization(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(lemmatization(tester)).


Usage
-----

The `lemmatizer(Representation, Language)` object accepts `atom`, `chars`, and
`codes` representations. The language parameter must be an object implementing
the `lemmatizer_language_protocol` protocol.

The default predicates enumerate all scalar candidates and select the first
candidate for each word in a list:

	| ?- lemmatizer(atom, english_lemmatizer)::lemma(running, Lemma).
	Lemma = run
	yes

	| ?- findall(Lemma, lemmatizer(atom, english_lemmatizer)::lemma(saw, Lemma), Lemmas).
	Lemmas = [saw, see]
	yes

	| ?- lemmatizer(atom, english_lemmatizer)::lemmas([the,children,were,running,better], Lemmas).
	Lemmas = [the, child, be, run, good]
	yes

All returned values, including unknown words, are canonical lowercase values
in the configured representation.


Options
-------

The `lemma/3` and `lemmas/3` predicates accept these options:

- `part_of_speech(PartOfSpeech)` constrains a scalar lookup or applies the
  same hint to all words in a list.
- `parts_of_speech(PartsOfSpeech)` provides one hint per word and is accepted
  only by `lemmas/3`. It is mutually exclusive with `part_of_speech/1`.
- `ambiguity(first)` selects the first provider candidate.
- `ambiguity(all)` enumerates all candidates. For `lemmas/3`, this enumerates
  the Cartesian product of the candidates for the individual words.
- `unknown(normalize)` returns the normalized input when the provider has no
  candidate.
- `unknown(preserve)` returns unrecognized words unchanged, preserving their
  original representation and case.
- `unknown(fail)` fails when the provider has no candidate.

The scalar default is `ambiguity(all)`. The list default is
`ambiguity(first)`. Both default to `unknown(normalize)`.

	| ?- lemmatizer(atom, english_lemmatizer)::lemma(saw, Lemma, [part_of_speech(verb)]).
	Lemma = see
	yes

	| ?- lemmatizer(atom, english_lemmatizer)::lemmas(
	         [saw, better], Lemmas,
	         [parts_of_speech([verb,adjective])]
	     ).
	Lemmas = [see, good]
	yes

	| ?- lemmatizer(atom, english_lemmatizer)::lemma(unknown, _, [unknown(fail)]).
	no

	| ?- lemmatizer(atom, english_lemmatizer)::lemma('UNKNOWN', Lemma, [unknown(preserve)]).
	Lemma = 'UNKNOWN'
	yes

	| ?- lemmatizer(atom, english_lemmatizer)::lemmas([children,'UNKNOWN'], Lemmas, [unknown(preserve)]).
	Lemmas = [child, 'UNKNOWN']
	yes


English coverage
----------------

The English provider recognizes a curated set of common base forms and
irregular nouns, verbs, adjectives, and adverbs. Its rules cover common plural,
third-person singular, past-tense, participle, comparative, and superlative
forms only when the generated base form is known. This avoids aggressive blind
suffix stripping.

This library is not a full morphological analyzer or part-of-speech tagger.
Tokenization, punctuation handling, Unicode normalization, contextual POS
tagging, and multi-word expressions are the caller's responsibility.


Adding providers
----------------

A provider implements `lemmatizer_language_protocol` using canonical lowercase
atoms. It enumerates recognized, distinct candidates in stable preference
order and fails for unknown words. Unknown-word fallback, representation
conversion, ambiguity selection, and list processing belong to the facade.

	:- object(example_lemmatizer,
		implements(lemmatizer_language_protocol)).

		lemma(mice, mouse).
		lemma(saw, saw).
		lemma(saw, see).

		lemma(mice, noun, mouse).
		lemma(saw, noun, saw).
		lemma(saw, verb, see).

	:- end_object.

The protocol deliberately does not expose dictionaries, exceptions, or rules.
Providers may instead use finite-state, statistical, cached, dynamically loaded,
or external implementations while presenting the same interface.
